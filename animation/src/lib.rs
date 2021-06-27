use std::time::{Instant, Duration};
use timing::{TimingFn, linear};

pub trait Animatable {
    fn animate<'a>(&'a self, other: &'a Self, duration: Duration) -> Animation<Self>;
    fn apply_interpol<'a>(&'a mut self, v1: &'a Self, v2: &'a Self, at: f64);
}

pub struct Frame<'a, T: Animatable + ?Sized> {
    at: Duration,
    value: &'a T,
}

///Returns a value based on the input, range: [0,1]
pub struct ValueFn<'a, T: ?Sized>
(
    Box<dyn Fn(f64) -> Box<T> + 'a>
);

impl <'a, F, T: Animatable> From<F> for ValueFn<'a, T>
where F: Fn(f64) -> T + 'a
{
    fn from(f: F) -> Self {
	ValueFn(Box::new(move |num: f64| Box::new((f)(num))))
    }
}
    
pub struct Animation<'a, T: Animatable + ?Sized>
{
    inst_value: Box<T>,
    frames: Vec<Frame<'a, T>>,
    pub timing_fn: TimingFn<'a>,
    start: Option<Instant>,
}

impl <'a, T: Animatable> Animation<'a, T>
{
    pub fn new(base: &'a T) -> Self
    where T: Clone
    {
	Animation {
	    inst_value: Box::new(base.clone()),
	    frames: vec![Frame {
		at: Duration::new(0, 0),
		value: base,
	    }],
	    timing_fn: TimingFn::from(linear),
	    start: None,
	}
    }

    pub fn add_frame(&mut self, value: &'a T, at: Duration) {
	let order_of = |it: &Frame<'_, T>| it.at.cmp(&at);

	match self.frames.binary_search_by(order_of) {
	    Ok(i) => self.frames[i] = Frame{value, at},
	    Err(i) => self.frames.insert(i, Frame{value, at}),
	};
    }

    pub fn play(&mut self) {
	self.start = Some(Instant::now());
    }

    pub fn current_value<'b>(&'b mut self) -> &'b T
    where T: std::fmt::Debug {
	let start = match self.start {
	    Some(t) => t,
	    None => return &self.frames[0].value,
	};

	let frames_len = self.frames.len();
	if frames_len == 1 {
	    return &self.frames[0].value;
	}
	
	let now = Instant::now() - start;

	let f1_index = match self.frames.binary_search_by(|it| it.at.cmp(&now)) {
	    Ok(i) => i,
	    Err(i) => i - 1,
	};
	if f1_index >= frames_len - 1 {
	    return &self.frames[frames_len - 1].value;
	}

	let f1 = &self.frames[f1_index];
	let f2 = &self.frames[f1_index + 1];
	let at = now - f1.at;
	let end_point = f2.at - f1.at;

	let at_normalized = self.timing_fn.call(at, end_point);
	self.inst_value.as_mut().apply_interpol(f1.value, f2.value, at_normalized);
	
	&self.inst_value.as_ref()
    }
}

pub mod timing {
    use std::time::Duration;

    pub enum Either<T, U> {
	Left(T),
	Right(U),
    }

    ///The first duration signifies the duration since the start of the animation,
    ///the second, one the duration of the whole animation
    pub struct TimingFn<'a> (
	Box<dyn Fn(Duration, Duration) -> f64 + 'a>
    );

    impl <'a> TimingFn<'a> {
	pub fn call(&'a self, time: Duration, end_point: Duration) -> f64 {
	    (self.0)(time, end_point)
	}
    }

    impl <'a, F> From<F> for TimingFn<'a>
    where F: Fn(f64) -> f64 + 'a
    {
        fn from(f: F) -> Self {
	    TimingFn(Box::new(move |x, end_point| (f)(duration_to_ratio(x, end_point))))
	}
    }

    fn duration_to_ratio(x: Duration, end_point: Duration) -> f64 {
	let x = x.as_secs_f64();
	let end_point = end_point.as_secs_f64();

	if end_point <= 0.0 {
	    return 0.0;
	}

	x / end_point
    }

    pub fn linear(x: f64) -> f64 { x }
}
