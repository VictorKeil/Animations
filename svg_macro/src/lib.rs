#![allow(dead_code, unused_imports)]
extern crate proc_macro;

use std::{any::Any, iter::{FromIterator, Zip}};

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{self, AngleBracketedGenericArguments, Attribute, Block, Data, DeriveInput, Field, Fields, FieldsUnnamed, GenericArgument, GenericParam, Generics, Ident, Item, ItemFn, Lifetime, Lit, Path, PathArguments, PathSegment, Token, Type, TypePath, Variant, parse_quote, punctuated::Punctuated, token::{Comma, Gt, Lt, Paren, Token}};

#[proc_macro_attribute]
pub fn svg_tag(_attributes: TokenStream, items: TokenStream) -> TokenStream {
    let mut ast = syn::parse(items).unwrap();
    
    match impl_svg_element(&mut ast) {
	Err(e) => panic!("{}", e),
	Ok(r) => r
    }
}

fn snake_case(input: String) -> String {
    let upper_chars: Vec<char> = ('A'..'Z').collect();
    let mut indices: Vec<usize> = Vec::new();

    for (i, c) in input.chars().enumerate() {
	if i > 0  && upper_chars.contains(&c) {
	    indices.push(i);
	}
    }

    let mut result = String::with_capacity(input.len() + indices.len());
    result.push_str(&input);
    let mut offset = 0;
    for i in indices {
	result.insert(i + offset, '_');
	offset += '_'.len_utf8();
    }
    result.make_ascii_lowercase();

    result
}

fn type_contains(ty: &Type, ident: Ident) -> bool {
    match ty {
	Type::Path(path) => {
	    path.path.segments.iter()
		.any(|seg| {
		    seg.ident == ident
		})
	},
	Type::Array(arr) => type_contains(&arr.elem, ident),
	_ => panic!("Type: '{:?}' not supported.", ty),
    }
}

///Returns params with only the lifetime- and type parameters found
///in `lifetimes` and `types`
fn filter_generic_params(params: Punctuated<GenericParam, Comma>,
			 lifetimes: &Vec<Lifetime>,
			 types: &Vec<Type>
) -> Punctuated<GenericParam, Comma> {
    let contains_param = move |param: &GenericParam| -> bool {
	match param {
	    GenericParam::Lifetime(lt_def) =>
		lifetimes.contains(&lt_def.lifetime),
	    GenericParam::Type(ty) =>
		types.iter().any(|it| type_contains(it, ty.ident.clone())),
	    _ => false,
	}
    };

    let generic_params = params.into_iter().filter(contains_param);
    Punctuated::from_iter(generic_params)
}

fn consume_explicit_field_name(field: &mut Field) -> Option<Ident> {
    let name_attr_pos = field.attrs.iter().position(|attr| {
	match attr.path.segments.first() {
	    Some(seg) => {
		let ident = format!("{}", seg.ident);
		ident == "name"
	    },
	    None => false,
	}
    })?;

    let name_attr = &field.attrs[name_attr_pos];

    let args: Lit = name_attr.parse_args().unwrap();
    let name = match args {
	Lit::Str(s) => s.value(),
	err => panic!("Invalid type for name: {:?}", err),
    };

    let result = Some(Ident::new(name.as_str(), Span::call_site()));
    field.attrs.remove(name_attr_pos);

    result
}

fn get_field_name(type_path: &TypePath,
		  lifetimes: &mut Vec<Lifetime>,
		  types: &mut Vec<Type>,
		  generic_args: &mut Vec<GenericArgument>
) -> Ident
{
    //Get the name of the type, store its generic arguments
    let type_name = match type_path.path.segments.last() {
	Some(name) => name,
	None => panic!("Invalid type path: {:#?}", type_path),
    };
    
    if let PathArguments::AngleBracketed(args) = &type_name.arguments {
	for arg in args.args.iter() {
	    let arg_clone = arg.clone();
	    if !generic_args.contains(&arg_clone) {
		generic_args.push(arg.clone());
	    }

	    match arg.clone() {
		GenericArgument::Lifetime(lt) =>
		    lifetimes.push(lt),
		GenericArgument::Type(ty) =>
		    types.push(ty),
		_ => ()
	    }
	}
    }

    let type_name_str = format!("{}", &type_name.ident);
    let snake = snake_case(type_name_str);

    Ident::new(snake.as_str(), type_name.ident.span())
}

///used_generic_params takes a collection of params and modifies it to only contain
///the ones used by the fields
fn parse_fields<'a>(fields: &'a mut Punctuated<Field, Comma>,
		used_generic_params: &mut Generics,
		generic_args: &mut Vec<GenericArgument>
) -> Zip<<Vec<Ident> as IntoIterator>::IntoIter, <Vec<TypePath> as IntoIterator>::IntoIter>
{
    let mut contained_lifetimes = Vec::new();
    let mut contained_types = Vec::new();

    let mut type_paths = Vec::new();
    let mut field_names = Vec::new();

    for field in fields {
	let explicit_name = consume_explicit_field_name(field);

	let type_path = match &field.ty {
	    Type::Path(tp) => tp.clone(),
	    e => {
		println!("{:?}", e);
		panic!("Fields must only contain type paths.") ;
	    },
	};

	let field_name = explicit_name.unwrap_or_else(|| {
	    get_field_name(&type_path,
			   &mut contained_lifetimes,
			   &mut contained_types,
			   generic_args)
	});


	type_paths.push(type_path);
	field_names.push(field_name);
    }

    used_generic_params.params = filter_generic_params(used_generic_params.params.clone(),
						       &contained_lifetimes,
						       &contained_types);

    field_names.into_iter().zip(type_paths.into_iter())
}

///Takes all the variants of the svg_tag enum, and parses them and their fields.
///Returns generated 'elem' types and variants for the svg_tag enum, containing
///new variants corresponding to the new 'elem' types.
///
///All type arguments to types in the fields of variants are extracted,
///and type parameters are created for them in the generated 'elem' types.
fn parse_variants(mod_name: Ident,
		  variants: &Punctuated<Variant, Comma>,
		  tag_def: &DeriveInput)
		  -> (proc_macro2::TokenStream, Punctuated<Variant, Comma>)
{
    let mut attribute_types = Vec::new();
    let mut new_variants = Punctuated::new();

    // Every variant in the enum represents an svg tag.
    // An svg attributes type corresponding to each variant will be generated.
    for v in variants.clone() {
	let mut new_variant = v.clone();
	let attrs = new_variant.attrs;
	new_variant.attrs = Vec::new();

	let attributes_type_name = format_ident!("{}", v.ident);

	// Each field annotates a type that will be included in the attributes.
	// The name of that field will be the identifier of the type, in snake_case
	let mut fields = match v.fields {
	    Fields::Unnamed(fs) => fs.unnamed,
	    Fields::Unit => {
		new_variant.fields = Fields::Unit;
		new_variants.push(new_variant);
		continue;
	    },
	    _ => panic!("Must use unnamed fields."),
	};

	let mut generic_args = Vec::new();
	let mut generic_params = tag_def.generics.clone();

	let field_names_zip_types = parse_fields(&mut fields, &mut generic_params, &mut generic_args);
	let (field_names, type_paths): (Vec<_>, Vec<_>) = field_names_zip_types.unzip();

	let ab_generic_args = AngleBracketedGenericArguments {
	    colon2_token: None,
	    lt_token: Lt::default(),
	    args: Punctuated::from_iter(generic_args.into_iter()),
	    gt_token: Gt::default(),
	};

	let tag_attrs = &tag_def.attrs;

	let (delta_functionality, compute_delta_fun) =
	    generate_delta_func(&attributes_type_name,
				&generic_params,
				&ab_generic_args,
				&type_paths,
				&field_names);

	let attribute_type = quote! {
	    #(#tag_attrs)*
	    #(#attrs)*
	    #[derive(Default)]
	    pub struct #attributes_type_name #generic_params {
		#(pub #field_names : Option<#type_paths>),*
	    }

	    #delta_functionality

	    impl #generic_params #attributes_type_name #ab_generic_args {
		pub fn new() -> Self {
		    Self::default()
		}

		#compute_delta_fun
	    }
	};

	new_variant.fields = Fields::Unnamed(
	    parse_quote!((#mod_name::#attributes_type_name #ab_generic_args))
	);

	new_variants.push(new_variant);

	attribute_types.push(attribute_type);
    }

    let elem_defs = quote! {
	pub mod #mod_name {
	    use super::*;

	    #(
		#attribute_types
	    )*
	}
    };

    (elem_defs, new_variants)
}

fn generate_delta_func(for_attr_type: &Ident,
		       generic_params: &Generics,
		       generic_args: &AngleBracketedGenericArguments,
		       fields: &Vec<TypePath>,
		       field_names: &Vec<Ident>
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let enum_name = format_ident!("{}DeltaPart", for_attr_type);
    let variant_names = fields.iter().filter_map(|it| {
	Some(&it.path.segments.last()?.ident)
    });

    let partial_delta_enum = quote! {
	pub enum #enum_name #generic_params {
	    #(#variant_names ( #fields )),*
	}
    };

    let delta_type_name = format_ident!("{}Delta", for_attr_type);
    let delta_type_def = quote! {
	pub type #delta_type_name #generic_params = Vec<#enum_name #generic_args>;
    };

    let compute_delta = quote! {
	pub fn apply_delta<#generic_params>(&mut self, delta: #delta_type_name #generic_args) {
	    for delta_part in delta {
		match delta_part {
		    #(#enum_name :: #fields (d) => {
			self. #field_names = Some(d);
		    },)*
		}
	    }
	}
    };

    let all_defs = quote! {
	#partial_delta_enum

	#delta_type_def
    };

    (all_defs, compute_delta)
}

fn impl_svg_element(tag: &mut syn::DeriveInput) -> Result<TokenStream, String> {
    if let Some(_) = &tag.generics.where_clause {
	panic!("where clauses are not currently supported");
    }

    let data_enum = match &tag.data {
	Data::Enum(data_enum) => Ok(data_enum),
	_ => Err("Must be defined with an enum."),
    }?;

    let variants = &data_enum.variants;

    let mod_name = Ident::new("attributes", Span::call_site());
    let (elem_defs, new_variants) = parse_variants(mod_name, variants, &tag);

    if let Data::Enum(de) = &mut tag.data {
	 de.variants = new_variants;
    }

    let result = quote! {
	#elem_defs
	
	#tag
    };

    Ok(result.into())
}
