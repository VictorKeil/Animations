use std::{hash::Hash, str::FromStr};

use gdk::{self, EventMask, SeatCapabilities, EventMotion, EventType, Window, WindowExt, };
use cairo;

use ::svg::{node::element::{Element, path::{Command, Data, Position}}};
use ::svg::node::element::tag::{SVG, Circle, Rectangle, Path, Group};
use ::svg::node::element::tag;
use ::svg::parser::Event;

use nom::{Finish, IResult, Parser, bytes::complete::tag, character::complete::{anychar, char, space0}, combinator::opt, error::{Error, ParseError}, multi::{many_till, many_m_n}, number::complete::float, sequence::{
	preceded,
	delimited
}};

use std::f64::consts::PI;

mod helpers;

fn make_event_mask() -> EventMask {
    EventMask::empty()
	| EventMask::FOCUS_CHANGE_MASK
	| EventMask::ENTER_NOTIFY_MASK
	| EventMask::LEAVE_NOTIFY_MASK
	| EventMask::POINTER_MOTION_MASK
	| EventMask::BUTTON_MOTION_MASK
	| EventMask::BUTTON_PRESS_MASK
	| EventMask::BUTTON_RELEASE_MASK
	| EventMask::KEY_PRESS_MASK
	| EventMask::KEY_RELEASE_MASK
}

fn make_win_attrs(screen: &gdk::Screen) -> gdk::WindowAttr {
    let width = 800;
    let height = 800;

    let display = screen.get_display();
    let seat = display.get_default_seat().expect("Error: could not get default seat");

    let point = seat.get_pointer().expect("Error: could not get pointer").get_position();
    let monitor = display.get_monitor_at_point(point.1, point.2).expect("Error: could not get monitor at point");

    let geo = monitor.get_geometry();

    let x = Some((geo.width - width) / 2);
    let y = Some((geo.height - height) / 2);
    
    gdk::WindowAttr {
	title: Some(String::from("Volume Indicator")),
	x,
	y,
	width,
	height,
	event_mask: make_event_mask(),
	window_type: gdk::WindowType::Temp,
	type_hint: Some(gdk::WindowTypeHint::Notification),
	..gdk::WindowAttr::default()
    }
}

fn rounded_rectangle(cr: &cairo::Context, x: f64, y: f64, width: f64, height: f64, r: f64) {
    let lw = width - 2.0 * r;
    let lh = height - 2.0 * r;

    cr.translate(x, y);

    cr.move_to(r, 0.0);
    cr.rel_line_to(lw, 0.0);
    cr.arc(r+lw, r, r, 3.0 * PI / 2.0, 0.0);

    cr.rel_line_to(0.0, lh);
    cr.arc(r+lw, r+lh, r, 0.0, PI / 2.0);

    cr.rel_line_to(-lw, 0.0);
    cr.arc(r, r+lh, r, PI / 2.0, PI);

    cr.rel_line_to(0.0, -lh);
    cr.arc(r, r, r, PI, 3.0 * PI / 2.0);

    cr.translate(-x, -y);
}

#[derive(Debug, Clone, Copy)]
enum Color {
    RGBA(f64, f64, f64, f64),
    RGB(f64, f64, f64),
    None,
}

#[derive(Debug)]
pub struct ParseColorError(String);
#[derive(Debug)]
pub struct NoColorError;

impl Color {
    pub fn apply_to_context(self, cr: &cairo::Context) -> Result<(), NoColorError> {
	match self {
	    Color::RGBA(r, g, b, a) => Ok(cr.set_source_rgba(r, g, b, a)),
	    Color::RGB(r, g, b) => Ok(cr.set_source_rgb(r, g, b)),
	    Color::None => Err(NoColorError),
	}
    }

    pub fn set_alpha(self, alpha: f64) -> Color {
	match self {
	    Color::RGBA(r, g, b, _) => Color::RGBA(r, g, b, alpha),
	    Color::RGB(r, g, b) => Color::RGBA(r, g, b, alpha),
	    Color::None => Color::None,
	}
    }
}

impl FromStr for Color {
    fn from_str(s: &str) -> Result<Self, Self::Err> {
	if s.eq_ignore_ascii_case("none") {
	    return Ok(Color::None);
	}

	let err = || ParseColorError(String::from(s));
	let parse_hex = |s_rep| i16::from_str_radix(s_rep, 16)
	    .map(|v| (v as f64) / 255.0)
	    .map_err(|err| ParseColorError(format!("{:?}, input: {}", err, s)));
	let si = s.find('#').map(|v| v + 1).ok_or(err())?;

	if s.len() - si < 6 {
	    return Err(err());
	}

	let r = parse_hex(&s[si..si + 2])?;
	let g = parse_hex(&s[si + 2..si + 4])?;
	let b = parse_hex(&s[si + 4..si + 6])?;

	if s.len() - si < 8 {
	    return Ok(Color::RGB(r, g, b));
	}

	let a = parse_hex(&s[si + 6..si + 8])?;
	Ok(Color::RGBA(r, g, b, a))
    }

    type Err = ParseColorError;
}

#[derive(Debug, Clone, Copy)]
enum Linecap {
    Butt,
    Round,
    Square,
}

impl Into<cairo::LineCap> for Linecap {
    fn into(self) -> cairo::LineCap {
	match self {
	    Linecap::Butt => cairo::LineCap::Butt,
	    Linecap::Round => cairo::LineCap::Round,
	    Linecap::Square => cairo::LineCap::Square,
	}
    }
}

impl FromStr for Linecap {
    type Err = ParseStyleError;
	
    fn from_str(s: &str) -> Result<Self, Self::Err> {
	Ok(match s.to_lowercase().as_str() {
	    "butt" => Self::Butt,
	    "round" => Self::Round,
	    "square" => Self::Square,
	    err => return Err(ParseStyleError(String::from(err)))
	})
    }
}

#[derive(Debug, Clone, Copy)]
enum Linejoin {
    Arcs,
    Bevel,
    Miter,
    Miterclip,
    Round,
}

impl Into<cairo::LineJoin> for Linejoin {
    fn into(self) -> cairo::LineJoin {
	match self {
	    Linejoin::Bevel => cairo::LineJoin::Bevel,
	    Linejoin::Miter => cairo::LineJoin::Miter,
	    Linejoin::Round => cairo::LineJoin::Round,
	    _ => {
		println!("WARNING: line-join not implemented: {:?}", self);
		cairo::LineJoin::Miter
	    }
	}
    }
}

impl FromStr for Linejoin {
    type Err = ParseStyleError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
	Ok(match s.to_lowercase().as_str() {
	    "arcs" => Self::Arcs,
	    "bevel" => Self::Bevel,
	    "miter" => Self::Miter,
	    "miterclip" => Self::Miterclip,
	    "round" => Self::Round,
	    _ => return Err(ParseStyleError(String::from(s))),
	}) 
    }
}

#[derive(Debug)]
enum Dasharray {
    None,
}

#[derive(Debug)]
enum DisplayVal {
    None,
}

impl FromStr for DisplayVal {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
	if s.eq_ignore_ascii_case("none") {
	    Ok(DisplayVal::None)
	} else {
	    Err(format!("Invalid value: {}", s))
	}
    }
}

#[derive(Debug, Default)]
struct Style {
    display: Option<DisplayVal>,
    fill: Option<Color>,
    fill_opacity: Option<f64>,
    stroke: Option<Color>,
    stroke_width: Option<f64>,
    stroke_linecap: Option<Linecap>,
    stroke_linejoin: Option<Linejoin>,
    stroke_miterlimit: Option<f64>,
    stroke_dasharray: Option<Dasharray>,
    stroke_opacity: Option<f64>,
}

#[derive(Debug)]
struct ParseStyleError(String);

impl Style {
    fn new() -> Self { Style::default() }

    fn apply(&self, cr: &cairo::Context) {
	match self.display {
	    Some(DisplayVal::None) => {
		cr.new_path();
		return;
	    },
	    None => (),
	}

	if let Some(col) = self.fill {
	    col.apply_to_context(&cr).unwrap();
	    cr.fill_preserve();
	}

	if let Some((stroke, stroke_width)) = self.stroke.zip(self.stroke_width) {
	    stroke.apply_to_context(&cr).unwrap();
	    cr.set_line_width(stroke_width);

	    self.stroke_linecap.map(|lc| cr.set_line_cap(lc.into()));

	    cr.stroke_preserve();
	};

	cr.new_path();
    }
}

impl FromStr for Style {
    type Err = ParseStyleError;

    fn from_str(s: &str) -> Result<Self, ParseStyleError> {
	    let decls = s.split(';');

	    let wrap_err = |err| ParseStyleError(format!("{:?}", err));
	    let decl_err = |decl| ParseStyleError(format!("Invalid decl pair: {}", decl));

	    let mut result = Style::new();

	    for decl in decls {
		let mut decl_pair = decl.split(':');
		let prop_name = decl_pair.next().ok_or(decl_err(decl))?;
		let val: &str = decl_pair.next().ok_or(decl_err(decl))?.trim();

		let parse_color = |val: &str| -> Result<Option<Color>, ParseStyleError> {
		    Ok(match val.parse().map_err(wrap_err)? {
			Color::None => None,
			any => Some(any),
		    })
		};

		match prop_name.trim() {
		    "display" => result.display = val.parse().ok(),
		    "fill" => result.fill = parse_color(val)?,
		    "stroke" => result.stroke = parse_color(val)?,
		    "stroke-width" => result.stroke_width = Some(val.parse().map_err(|err| ParseStyleError(format!("{:?}", err)))?),
		    "stroke-linejoin" => result.stroke_linejoin = Some(val.parse()?),
		    "stroke-linecap" => result.stroke_linecap = Some(val.parse()?),
		    _ => (),
		};
	    }

	    Ok(result)
    }
}

fn snd<T, U>(input: (U, T)) -> T { input.1 }

fn css_function<'a, F, O, E: ParseError<&'a str>>(
    name: &'a str,
    arg_parser: F
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
    O: core::fmt::Debug,
    E: core::fmt::Debug,
{
    let mut parser = tag::<_,&str,E>(name)
	.and(space0)
	.and(delimited(char('('),
			arg_parser,
		       char(')')))
        .map(snd);

    move |s| parser.parse(s)
}

fn handle_transform<'a>(cr: &'a cairo::Context, transform_data: &'a str) -> Option<Box<dyn FnOnce() + 'a>> {
	let mut restore_proc: Vec<_> = Vec::new();

	let arg_parser = float
	    .and(opt(preceded(space0
			      .and(char(','))
			      .and(space0),
			      float)));
	let mut translate =
	    many_till(anychar,
		      css_function::<_,_,Error<&str>>(
			  "translate",
			  arg_parser))
	    .map(snd);
	
	let res = translate.parse(transform_data).finish();
	if let Ok((_, trans)) = res {
	    let (dx, dy) = match trans {
		(dx, Some(dy)) => (dx as f64, dy as f64),
		(dx, None) => (dx as f64, 0.0),
	    };
	    
	    cr.translate(dx, dy);

	    let restore = move || {
		cr.translate(-dx, -dy);
	    };
	    restore_proc.push(Box::new(restore) as Box<dyn FnOnce()>);

	}

	let arg_parser = float
	    .and(many_m_n(5, 5,
			  delimited(space0,
				    char(','),
				    space0)
			  .and(float)
			  .map(snd)))
	    .map(|(fst, rest)| {
		let mut temp = rest;
		let mut new = vec![fst];
		new.append(&mut temp);
		new
	    });

	let mut matrix =
	    many_till(anychar, css_function::<_,_,Error<&str>>(
		"matrix",
		arg_parser))
	    .map(snd);

	let res = matrix.parse(transform_data).finish();
	if let Ok((_, mat)) = res {
	    let old_mat = cr.get_matrix();
	    let mat: Vec<f64> = mat.iter().map(|v| *v as f64).collect();
	    let new_mat = cairo::Matrix::new(mat[0], mat[1], mat[2], mat[3], mat[4], mat[5]);

	    cr.set_matrix(new_mat);
	    let restore = move || {
		cr.set_matrix(old_mat);
	    };
	    restore_proc.push(Box::new(restore));
	}

	if restore_proc.len() > 0 {
	    Some(Box::new(move || {
		for f in restore_proc {
		    (f)();
		}
	    }))
	} else {
	    None
	}
}

fn draw(window: &gdk::Window, events: &Vec<::svg::parser::Event>) -> Option<()> {
    let vis_reg = window.get_visible_region()?;
    let context = window.begin_draw_frame(&vis_reg).expect("Error: no context");

    let mut ext = cairo::RectangleInt {
	x: 0,
	y: 0,
	width: 0,
	height: 0,
    };
    vis_reg.get_extents(&mut ext);

    let cr = context.get_cairo_context().expect("Error: no cairo context");

    let mut restores: Vec<Option<Box<dyn FnOnce()>>> = Vec::new();
    let mut is_empty_tag = false;

    for event in events {
	if let Event::Tag(type_, kind, attributes) = &event {
	    if let Some(id_) = attributes.get("id") {
		println!("tag: {:?}\tkind: {:?}\tid: {}", type_, kind, id_);
	    }

	    let restore = if let Some(transform_data) = attributes.get("transform") {
		handle_transform(&cr,transform_data)
	    } else {
		None
	    };
	    if restore.is_some() {
	    }

	    is_empty_tag = *kind == tag::Type::Empty;

	    if *kind == tag::Type::Start || *kind == tag::Type::Empty {
		restores.push(restore);
	    } else if *kind == tag::Type::End {
		if let Some(f) = restores.pop().flatten() {
		    (f)();
		};
	    }
	};

	match &event {
	    Event::Tag(SVG, tag::Type::Start, attributes) => {
		// ***** SET BACKGROUND *******
		cr.set_source_rgb(1.0, 1.0, 1.0);
		cr.paint();

		if let Some(vb) = attributes.get("viewBox") {
		    let mut vb_iter = vb.split_whitespace().skip(2);
		    let width: f64 = vb_iter.next().expect("Error: no viewBox width.").parse().unwrap();
		    let height: f64 = vb_iter.next().expect("Error: no viewBox height.").parse().unwrap();
		    if width != 0.0 && height != 0.0 {
			match ext.width as f64 / width {
			    s if !s.is_nan() => {cr.scale(s, s)},
			    _ => ()
			};

		    }
		} else {
		    println!("NO VIEWBOX");
		};
	    },
	    Event::Tag(Path, _, attributes) => {
		let raw_data = attributes.get("d").unwrap();
		let data = Data::parse(raw_data).unwrap();

		for command in data.iter() {
		    match command {
			Command::Move(pos_type, pos) => {
			    let (x, y) = match *pos_type {
				Position::Absolute => (pos[0] as f64, pos[1] as f64),
				Position::Relative => {
				    let p = cr.get_current_point();
				    (p.0 + pos[0] as f64, p.1 + pos[1] as f64)   
				}
			    };

			    cr.move_to(x, y);
			},
			Command::CubicCurve(pos_type, p) => {
			    for c in p.chunks_exact(6) {
				let p: Vec<f64> = c.iter().map(|&v| v as f64).collect();
				match *pos_type {
				    Position::Absolute => {
					cr.curve_to(p[0], p[1], p[2], p[3], p[4], p[5]);
				    },
				    Position::Relative => {
					cr.rel_curve_to(p[0], p[1], p[2], p[3], p[4], p[5]);
				    },
				}
			    }
			},
			Command::EllipticalArc(pos_type, p) => {
			    let p = p.iter().collect::<Vec<&f32>>();
			    let p_chunks = p.chunks(7);

			    p_chunks.for_each(|p| {
				let (x1,y1) = cr.get_current_point();
				let rx = *p[0] as f64;
				let ry = *p[1] as f64;
				let phi = (*p[2] as f64) * PI / 180.0; // x axis rotation
				let large_arc_flag = *p[3] as f32;
				let sweep_flag = *p[4] as f32;
				let mut x2 = *p[5] as f64;
				let mut y2 = *p[6] as f64;

				if *pos_type == Position::Relative {
				    x2 = x1 + x2;
				    y2 = y1 + y2;
				}

				// *** MATRIX MULTIPLICATION ***
				let b11 = (x1 - x2) / 2.0;
				let b21 = (y1 - y2) / 2.0;
				let a11 = phi.cos();
				let a12 = phi.sin();
				let a21 = -phi.sin();
				let a22 = a11;

				let x1_prime = b11 * a11 + b21 * a12;
				let y1_prime = b11 * a21 + b21 * a22;

				let r11 = rx * y1_prime / ry;
				let r21 = -ry * x1_prime / rx;
				let mut r_scalar =
				    (( rx.powi(2) * ry.powi(2) - rx.powi(2) * y1_prime.powi(2) -
					ry.powi(2) * x1_prime.powi(2) ) /
					( rx.powi(2) * y1_prime.powi(2) + ry.powi(2) * x1_prime.powi(2) )).sqrt();

				if r_scalar.is_nan() {
				    panic!("r_scalar is NaN!");
				}

				if large_arc_flag == sweep_flag {
				    r_scalar = -r_scalar;
				}

				let xc_prime = r11 * r_scalar;
				let yc_prime = r21 * r_scalar;

				let xc = xc_prime * a11 + yc_prime * a21 + (x1 + x2) / 2.0;
				let yc = xc_prime * a12 + yc_prime * a11 + (y1 + y2) / 2.0;

				let ux = (x1_prime - xc_prime) / rx;
				let uy = (y1_prime - yc_prime) / ry;
				let mag_u = (ux.powi(2) + uy.powi(2)).sqrt();
				let vx = (-x1_prime - xc_prime) / rx;
				let vy = (-y1_prime - yc_prime) / ry;
				let mag_v = (vx.powi(2) + vy.powi(2)).sqrt();

				let theta1 = uy.signum() * (ux / mag_u).acos();
				let mut d_theta = ( (ux * vx + uy * vy) / (mag_u * mag_v) ).acos() % (2.0 * PI);

				if sweep_flag == 0.0 && d_theta > 0.0 {
				    d_theta = d_theta - 2.0 * PI;
				} else if sweep_flag == 1.0 && d_theta < 0.0 {
				    d_theta = d_theta + 2.0 * PI;
				}

				if rx > 0.0 && ry > 0.0 {
				    let mat = cr.get_matrix();
				    cr.translate(xc, yc);
				    cr.scale(rx, ry);

				    if d_theta < 0.0 {
					cr.arc_negative(0.0, 0.0, 1.0, theta1, theta1 + d_theta);
				    } else {
					cr.arc(0.0, 0.0, 1.0, theta1, theta1 + d_theta);
				    }

				    cr.set_matrix(mat);
				    let mat = cr.get_matrix();
				}
			    })
			},
			Command::VerticalLine(pos_type, p) => {
			    p.iter().for_each(|&y| {
				let (x,_) = cr.get_current_point();
				match *pos_type {
				    Position::Relative => cr.rel_line_to(0.0, y as f64),
				    Position::Absolute => cr.line_to(x, y as f64),
				}
			    });
			},
			Command::Line(pos_type, p) => {
			    let p = p.iter().collect::<Vec<&f32>>();
			    let p_iter = p.chunks(2);
			    p_iter.for_each(|coord| {
				let x = *coord[0] as f64;
				let y = *coord[1] as f64;
				if *pos_type == Position::Relative {
				    cr.rel_line_to(x, y);
				} else if *pos_type == Position::Absolute {
				    cr.line_to(x, y);
				}
			    });
			},
			Command::Close => {
			    cr.close_path();
			}
			_ => (),
		    }
		}
	    },
	    Event::Tag(Circle, _, attributes) => {
		let cx = attributes.get("cx").unwrap().parse().unwrap();
		let cy = attributes.get("cy").unwrap().parse().unwrap();
		let r = attributes.get("r").unwrap().parse().unwrap();

		cr.arc(cx, cy, r, 0.0, 2.0 * PI);
	    },
	    Event::Tag(Rectangle, _, attributes) => {
		let x = attributes.get("x").unwrap().parse().unwrap();
		let y = attributes.get("y").unwrap().parse().unwrap();
		let width = attributes.get("width").unwrap().parse().unwrap();
		let height = attributes.get("height").unwrap().parse().unwrap();
		let ry = attributes.get("ry").map(|v| v.parse().unwrap());
		let rx = attributes.get("rx").map(|v| v.parse().unwrap());

		rounded_rectangle(&cr, x, y, width, height, ry.or(rx).unwrap_or(0.0));
	    },
	    _ => ()
	}

	match &event {
	    Event::Tag(_,_, attributes) => {
		if cr.has_current_point() {
		    if let Some(style_data) = attributes.get("style") {
			style_data.parse::<Style>().map(|style| style.apply(&cr));
		    }
		}
	    }
	    _ => ()
	}

	if is_empty_tag {
	    if let Some(f) = restores.pop().flatten() {
		(f)();
	    }
	}
    }

    window.end_draw_frame(&context);
    Some(())
}

fn main() {
    gdk::init();
    
    let display = gdk::Display::get_default().expect("Error getting default display");

    let screen = display.get_default_screen();
    let root_win = screen.get_root_window().expect("Error getting root window");

    println!("display name: {}", display.get_name());

    let attrs = make_win_attrs(&screen);
    let window = gdk::Window::new(Some(&root_win), &attrs);

    window.show();

    println!("Viewable: {:?}", window.is_viewable());

    println!("accept focus: {}", window.get_accept_focus());
    println!("focus: {}", window.get_focus_on_map());

    let refresh_rate = 60; // 60 Hz

    let path = "volume-icon.svg";
    let mut content = String::new();

    let mut svg_doc = ::svg::open(path, &mut content).unwrap();
    let events: Vec<Event> = svg_doc.collect();

    'main: loop {
	std::thread::sleep(std::time::Duration::new(0, 1000_000_000 / refresh_rate));

	draw(&window, &events);

	while display.has_pending() {
	    if let Some(ev) = display.get_event() {
		if ev.get_event_type() != EventType::Configure {
		    println!("even type: {}", ev.get_event_type());
		}

		match ev.get_event_type() {
		    EventType::EnterNotify => println!("Entered"),
		    EventType::LeaveNotify => println!("Left"),
		    EventType::MotionNotify => {
			if let Some((x, y)) = ev.get_coords() {
			    println!("x: {}, y: {}", x, y);
			    let mot_ev: &EventMotion = ev.downcast_ref().unwrap();
			    println!("HINT: {:?}", mot_ev.get_is_hint());
			}
		    },
		    EventType::KeyPress => {
			println!("keycode: {}\tkeyval: {}",
				ev.get_keycode().unwrap(),
				ev.get_keyval().unwrap());
			match std::char::from_u32(ev.get_keyval().unwrap()) {
			    Some('h') => window.hide(),
			    Some('s') => window.show(),
			    _ => continue,
			}
		    },
		    EventType::Delete => break 'main,
		    EventType::Nothing => println!("focused: {}", window.get_state().contains(gdk::WindowState::FOCUSED)),
		    EventType::FocusChange => {
			let foc_ev: &gdk::EventFocus = ev.downcast_ref().unwrap();
			println!("focus event: {:?}", foc_ev);
		    },
		    EventType::ButtonPress => {
			println!("focused: {}", window.get_state().contains(gdk::WindowState::FOCUSED));
		    }
		    _ => continue,
		}
	    }
	}
    }
}
