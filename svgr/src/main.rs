mod types;
mod tag;
mod parse;

use tag::{Tag, attributes, attributes::Rectangle, attributes::RectangleDelta, attributes::RectangleDeltaPart};

fn main() {
    let svg = parse::load_svg("../volume-icon.svg");

    let mut rect = Rectangle::new();
    rect.x = Some(20.0);

    println!("fill: {:?}", rect);
}
