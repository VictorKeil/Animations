use svg_macro::svg_tag;
use crate::types::*;

#[svg_tag]
#[derive(Debug)]
pub enum Tag {
    SVG(
	ViewBox,
	Id,
	Width,
	Height,
    ),
    Group(
	Id,
	Transform,
    ),
    Defs(
	Id,
	Transform,
    ),
    Line(
	Id,
	Transform,
    ),
    Path(
	Id,
	Transform,
	Data,
    ),
    Circle(
	Id,
	Transform,
	Cx,
	Cy,
	R,
    ),
    Ellipse(
	Id,
	Transform,
    ),
    Rectangle(
	Id,
	Transform,
	X,
	Y,
	Width,
	Height,
	Rx,
	Ry,
    ),
    Polygon(
	Id,
	Transform,
    ),
    Polyline(
	Id,
	Transform,
    ),
    LinearGradient(
	Id,
	Transform,
    ),
    RadialGradient(
	Id,
	Transform,
    ),
    Text(
	Id,
	Transform,
    ),
}

#[derive(Debug)]
pub struct Node {
    pub tag: Tag,
    pub children: Vec<Node>,
}

impl Node {
    pub fn empty() -> Node {
	Node {
	    tag: Tag::SVG(attributes::SVG::new()),
	    children: Vec::new(),
	}
    }
}

