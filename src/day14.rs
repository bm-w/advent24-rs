// Copyright (c) 2024 Bastiaan Marinus van de Weerd

#[cfg(feature = "day14-cli")]
pub(crate) mod cli;


#[cfg_attr(test, derive(Debug))]
struct Robot {
	pos: [usize; 2],
	vel: [isize; 2],
}

impl Robot {
	fn tick<const W: usize, const H: usize>(&mut self) {
		self.pos[0] = (self.pos[0] + W).checked_add_signed(self.vel[0]).unwrap() % W;
		self.pos[1] = (self.pos[1] + H).checked_add_signed(self.vel[1]).unwrap() % H;
	}
}


fn try_input_robots() -> impl Iterator<Item = Result<Robot, parsing::RobotsError>> {
	parsing::try_robots(include_str!("day14.txt"))
}

fn input_robots() -> impl Iterator<Item = Robot> {
	try_input_robots().map(Result::unwrap)
}


pub(crate) fn part1() -> u64 {
	part1_impl::<101, 103>(input_robots())
}

fn part1_impl<const W: usize, const H: usize>(input_robots: impl Iterator<Item = Robot>) -> u64 {
	let mut robots = input_robots.collect::<Vec<_>>();

	for _ in 0..100 {
		for robot in &mut robots {
			robot.tick::<W, H>();
		}
	}

	let mut quads = [0; 4]; // UL, LL, UR, LR
	for robot in &robots {
		let x = match robot.pos[0] {
			l if l < W / 2 => 0,
			r if r > W / 2 => 1,
			_ => continue
		};
		let y = match robot.pos[1] {
			u if u < H / 2 => 0,
			l if l > H / 2 => 2,
			_ => continue,
		};
		quads[y + x] += 1;
	}

	quads.into_iter().product()
}


pub(crate) fn part2() -> usize {
	part2_impl(input_robots())
}

fn part2_impl(input_robots: impl Iterator<Item = Robot>) -> usize {
	let [mut start_x, mut start_y] = [None, None];
	let [mut period_x, mut period_y] = [None, None];

	const W: usize = 101;
	const H: usize = 103;
	const E: f64 = 1e-6;

	let mut robots = input_robots.collect::<Vec<_>>();
	for elapsed in 1.. {

		// Count numbers of robots per X and Y coordinates
		let (mut nx, mut ny) = ([0; W], [0; H]);
		for robot in &mut robots {
			robot.tick::<W, H>();

			nx[robot.pos[0]] += 1;
			ny[robot.pos[1]] += 1;
		}

		// Compute means and variances
		let [mut sx, mut sy] = [0, 0];
		let [mut ssx, mut ssy] = [0, 0];
		for n in nx { sx += n; ssx += n * n; }
		for n in ny { sy += n; ssy += n * n; }
		let [mx, my] = [sx as f64 / W as f64, sy as f64 / H as f64];
		let [vx, vy] = [ssx as f64 / W as f64 - mx * mx, ssy as f64 / H as f64 - my * my];

		// Record cycles where variances are significant
		match (start_x, period_x, vx > mx * mx) {
			(None, _, true) => start_x = Some((elapsed, vx)),
			(Some((start, start_vx)), None, true) if (vx - start_vx).abs() < E =>
				period_x = Some(elapsed - start),
			_ => (),
		}
		match (start_y, period_y, vy > my * my) {
			(None, _, true) => start_y = Some((elapsed, vy)),
			(Some((start, start_vy)), None, true) if (vy - start_vy).abs() < E =>
				period_y = Some(elapsed - start),
			_ => (),
		}

		// Compute answer once starts and periods of cycles are known
		if let (Some((sx, _)), Some((sy, _)), Some(px), Some(py))
			= (start_x, start_y, period_x, period_y)
		{
			assert_eq!((sx.max(sy) - sx.min(sy)) % num_integer::gcd(px, py), 0);

			// TODO: Get all fancy with LCM / GCD / modular multiplicative inverse / etc.
			for k in 0.. {
				let future_elapsed = sx + k * px;
				if (future_elapsed - sy) % py == 0 {
					return future_elapsed
				}
			}
		}
	}

	unreachable!()
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::Robot;

	#[derive(Debug)]
	pub(super) enum Axis { X, Y }

	#[derive(Debug)]
	pub(super) enum Prop { Pos, Vel }

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum RobotError {
		Format(Option<Prop>),
		Prop(Prop, Axis, ParseIntError),
	}

	impl FromStr for Robot {
		type Err = RobotError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let (pos, vel) = s.split_once(' ').ok_or(Self::Err::Format(None))?;
			let (px, py) = pos.strip_prefix("p=").and_then(|p| p.split_once(','))
				.ok_or(Self::Err::Format(Some(Prop::Pos)))?;
			let px = px.parse().map_err(|e| Self::Err::Prop(Prop::Pos, Axis::X, e))?;
			let py = py.parse().map_err(|e| Self::Err::Prop(Prop::Pos, Axis::Y, e))?;
			let (vx, vy) = vel.strip_prefix("v=").and_then(|v| v.split_once(','))
				.ok_or(Self::Err::Format(Some(Prop::Vel)))?;
			let vx = vx.parse().map_err(|e| Self::Err::Prop(Prop::Vel, Axis::X, e))?;
			let vy = vy.parse().map_err(|e| Self::Err::Prop(Prop::Vel, Axis::Y, e))?;
			Ok(Robot { pos: [px, py], vel: [vx, vy] })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct RobotsError { line: usize, source: RobotError }

	pub(super) fn try_robots(s: &str) -> impl Iterator<Item = Result<Robot, RobotsError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| line.parse()
				.map_err(|e| RobotsError { line: l, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		p=0,4 v=3,-3
		p=6,3 v=-1,-3
		p=10,3 v=-1,2
		p=2,0 v=2,-1
		p=0,0 v=1,3
		p=3,0 v=-2,-2
		p=7,6 v=-1,-3
		p=3,0 v=-1,-2
		p=9,3 v=2,3
		p=7,3 v=-1,2
		p=2,4 v=2,-3
		p=9,5 v=-3,-3
	"};
	assert_eq!(part1_impl::<11, 7>([Robot { pos: [2,4], vel: [2, -3] }].into_iter()), 0);
	assert_eq!(part1_impl::<11, 7>(parsing::try_robots(INPUT).map(|r| r.unwrap())), 12);
	assert_eq!(part1(), 225521010);
	assert_eq!(part2(), 7774);
}
