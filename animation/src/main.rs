mod lib;
use std::time::{Duration, Instant};

use lib::timing::TimingFn;
use lib::*;
use lib::ValueFn;

impl Animatable for i32 {
    fn animate<'a>(&'a self, other: &'a Self, duration: Duration) -> Animation<'a, Self> {
        let mut a = Animation::new(self);
	a.add_frame(other, duration);

	a
    }

    fn apply_interpol<'a>(&'a mut self, v1: &'a Self, v2: &'a Self, at: f64) {
	let val = v1 + ((v2 - v1) as f64 * at).round() as i32;
        *self = val;
    }
}

fn main() {
    let mut a = 0.animate(&20, Duration::new(10,0));
    a.add_frame(&-40, Duration::new(15, 0));
    a.timing_fn = TimingFn::from(|x: f64| x.powi(2));

    let period = Duration::new(0, 500000000);
    let mut last_time = Instant::now() - period;
    let mut now_time = Instant::now();

    let start_time = now_time;
    a.play();

    loop {

	now_time = Instant::now();
	let d_time = now_time - last_time;
	if d_time > period {
	    let since_start = Instant::now().duration_since(start_time);
	    println!("value of animation: {}, at {:.2} seconds", a.current_value(), since_start.as_millis() as f64 / 1E3);

	    println!("SLEEP\n\n");
	    std::thread::sleep(2 * period - d_time);
	    last_time = now_time;
	}
    }
}
