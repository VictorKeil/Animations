use std::slice::Iter;

use svg::{node::element::{Element, path::{Command, Data, Position}}};
use svg::node::element::tag::{SVG, Circle, Rectangle, Path, Group};
use svg::node::element::tag;
use svg::parser::Event;

use crate::tag::{Node, Tag, attributes};

fn parse_node(events: &mut Iter<Event>) -> Vec<Node> {
    let mut siblings = Vec::new();

    while let Some(event) = events.next() {
    
	let mut node = match &event {
	    Event::Tag(SVG, tag::Type::Start, attributes) => {
		Some(Node {
		    tag: Tag::SVG(attributes::SVG::new()),
		    children: Vec::new(),
		})
	    },
	    Event::Tag(Path, _, attributes) => {
		let data_raw = attributes.get("d").unwrap();
		let data = Data::parse(data_raw).unwrap();

		for command in data.iter() {
		    match command {
			Command::Move(pos_type, pos) => {
			},
			Command::CubicCurve(pos_type, p) => {
			},
			Command::EllipticalArc(pos_type, p) => {
			},
			Command::HorizontalLine(pos_type, p) => {
			},
			Command::VerticalLine(pos_type, p) => {
			},
			Command::Line(pos_type, p) => {
			},
			Command::Close => {
			}
			_ => (),
		    }
		}

		None
	    },
	    Event::Tag(Circle, _, attributes) => {
		None
	    },
	    Event::Tag(Rectangle, _, attributes) => {
		Some(Node {
		    tag: Tag::Rectangle(attributes::Rectangle::new()),
		    children: Vec::new(),
		})
	    },
	    Event::Tag(Group, _, attributes) => {
		println!("found group");
		Some(Node {
		    tag: Tag::Group(attributes::Group::new()),
		    children: Vec::new(),
		})
	    },
	    _ => {
		None
	    }
	};

	if let Event::Tag(_, kind, _) = event {
	    let sibling = match kind {
		tag::Type::Start => {
		    let mut children = parse_node(events);
		    node.as_mut().map(|n| n.children.append(&mut children));
		    node
		},
		tag::Type::Empty => node,
		tag::Type::End => return siblings,
	    };

	    sibling.map(|it| siblings.push(it));
	}
    }

    siblings
}

fn parse_svg(events: &mut Vec<Event>) -> Node {
    let mut events_iter = events.iter();
    let mut result = parse_node(&mut events_iter);

    for event in events {
	let node = match &event {
	    Event::Tag(SVG, tag::Type::Start, attributes) => {
		let elem = Node {
		    tag: Tag::SVG(attributes::SVG::new()),
		    children: Vec::new(),
		};
	    },
	    Event::Tag(Path, _, attributes) => {
		let data_raw = attributes.get("d").unwrap();
		let data = Data::parse(data_raw).unwrap();
		
		for command in data.iter() {
		    match command {
			Command::Move(pos_type, pos) => {
			},
			Command::CubicCurve(pos_type, p) => {
			},
			Command::EllipticalArc(pos_type, p) => {
			},
			Command::HorizontalLine(pos_type, p) => {
			},
			Command::VerticalLine(pos_type, p) => {
			},
			Command::Line(pos_type, p) => {
			},
			Command::Close => {
			}
			_ => (),
		    }
		}
	    },
	    Event::Tag(Circle, _, attributes) => {
	    },
	    Event::Tag(Rectangle, _, attributes) => {
	    },
	    _ => ()
	};
    }

    result.remove(0)
}

pub fn load_svg(path: &str) -> Node {
    let mut content = String::new();
    let svg_doc = svg::open(path, &mut content).unwrap();
    let mut events = svg_doc.collect();

    parse_svg(&mut events)
}

