pub type Id = String;

#[derive(Debug)]
pub struct Transform{
    xx: f64,
    yx: f64,
    xy: f64,
    yy: f64,
    x0: f64,
    y0: f64,
}

#[derive(Debug)]
pub struct ViewBox {
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
}

#[derive(Debug)]
pub enum Position {
    Absolute,
    Relative,
}

#[derive(Debug)]
pub struct MoveParams(Vec<(f64, f64)>);
#[derive(Debug)]
pub struct LineParams(Vec<(f64, f64)>);
#[derive(Debug)]
pub struct HorizontalLineParams(Vec<f64>);
#[derive(Debug)]
pub struct VerticalLineParams(Vec<f64>);
#[derive(Debug)]
pub struct QuadraticCurveParams(Vec<(f64, f64, f64, f64)>);
#[derive(Debug)]
pub struct SmoothQuadraticCurveParams(Vec<(f64, f64, f64, f64)>);
#[derive(Debug)]
pub struct CubicCurveParams(Vec<(f64, f64, f64, f64, f64, f64)>);
#[derive(Debug)]
pub struct SmoothCubicCurveParams(Vec<(f64, f64, f64, f64)>);
#[derive(Debug)]
pub struct EllipticalArcParams(Vec<(f64, f64, u8, u8, f64, f64)>);

#[derive(Debug)]
pub enum Command {
    Move(Position, MoveParams),
    Line(Position, LineParams),
    HorizontalLine(Position, HorizontalLineParams),
    VerticalLine(Position, VerticalLineParams),
    QuadraticCurve(Position, QuadraticCurveParams),
    SmoothQuadraticCurve(Position, SmoothQuadraticCurveParams),
    CubicCurve(Position, CubicCurveParams),
    SmoothCubicCurve(Position, SmoothCubicCurveParams),
    EllipticalArc(Position, EllipticalArcParams),
    Close,
}

#[derive(Debug)]
pub struct Data(Vec<Command>);

pub type X = f64;
pub type Y = f64;

pub type Width = f64;
pub type Height = f64;

pub type R = f64;
pub type Rx = f64;
pub type Ry = f64;

pub type Cx = f64;
pub type Cy = f64;

