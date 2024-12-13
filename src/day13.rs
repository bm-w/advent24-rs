// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Machine {
	a: [usize; 2],
	b: [usize; 2],
	prize: [usize; 2],
}


fn input_machines() -> impl Iterator<Item = Machine> {
	parsing::try_machines(include_str!("day13.txt")).map(|r| r.unwrap())
}

pub(crate) fn part1() -> usize {
	solve::<0>(input_machines())
}

pub(crate) fn part2() -> usize {
	solve::<10000000000000>(input_machines())
}

fn solve<const PRIZE_ADD: usize>(input_machines: impl Iterator<Item = Machine>) -> usize {
	let mut tokens = 0;

	for Machine { a, b, prize } in input_machines {
		let prize = [prize[0] + PRIZE_ADD, prize[1] + PRIZE_ADD];

		// This can be represented as a 2x2 matrix-vector equation:
		// | a[0] b[0] |   | na |   | p[0] |
		// | a[1] b[1] | * | nb | = | p[1] |

		// Which can be solved by inverting the matrix:
		// | a[0] b[0] |^1   | p[0] |   | na |
		// | a[1] b[1] |   * | p[1] | = | nb |

		let m = [a[0] as isize, a[1] as isize, b[0] as isize, b[1] as isize]; // Column-major
		let det = m[0] * m[3] - m[1] * m[2];
		if det == 0 { continue }

		// As an aside, the sign of the determinant indicates the two vectors’
		// relative orientation. Imagining an analog clock with the 12 at
		// `[0, 1]` and the 3 at `[1, 0]`, a positive determinant means that
		// the vectors A and B are ordered counter-clockwise. From the first
		// example (see the test below),`[94, 34]` points near the 2 on the
		// clock, while `[22, 67]` points near the 1, so they are ordered
		// counter-clockwise and their determinant is positive. In contrast,
		// from the third example, `[17, 86]` points between the 12 and the 1,
		// while `[84, 37]` points between the 2 and the 3, and so they are
		// ordered clockwise and their determinant is negative.

		let imd = [m[3], -m[1], -m[2], m[0]]; // Column-major
		let nad = imd[0] * prize[0] as isize + imd[2] * prize[1] as isize;
		let nbd = imd[1] * prize[0] as isize + imd[3] * prize[1] as isize;

		// Must land on the prize exactly to win it
		if nad % det != 0 || nbd % det != 0 { continue }

		tokens += 3 * (nad / det) as usize + (nbd / det) as usize;
	}
	
	tokens
}


mod parsing {
	use std::str::FromStr;
	use super::Machine;

	pub(super) mod machine_error {
		use std::num::ParseIntError;

		#[derive(Debug)]
		pub(in super::super) enum Part { A, B, Prize }

		#[derive(Debug)]
		pub(in super::super) enum Location { Prefix, Midfix, Suffix }

		#[allow(dead_code)]
		#[derive(Debug)]
		pub(in super::super) enum Format { Line(Location), XPrefix, YPrefix }

		#[allow(dead_code)]
		#[derive(Debug)]
		pub(in super::super) enum Number { X, Y }

		#[allow(dead_code)]
		#[derive(Debug)]
		pub(in super::super) enum Error {
			Format(Part, Format),
			Number(Part, Number, ParseIntError),
		}
	}

	impl FromStr for Machine {
		type Err = machine_error::Error;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use machine_error::{Error as E, Part as EP, Format as EF, Location as EL, Number as EN};

			let s = s.trim_end_matches('\n');

			let (a, s) = s.strip_prefix("Button A: ").ok_or(E::Format(EP::A, EF::Line(EL::Prefix)))?
				.split_once('\n').ok_or(E::Format(EP::A, EF::Line(EL::Suffix)))?;
			let (ax, ay) = a.split_once(", ").ok_or(E::Format(EP::A, EF::Line(EL::Midfix)))?;
			let ax = ax.strip_prefix("X+").ok_or(E::Format(EP::A, EF::XPrefix))?
				.parse().map_err(|e| E::Number(EP::A, EN::X, e))?;
			let ay = ay.strip_prefix("Y+").ok_or(E::Format(EP::A, EF::YPrefix))?
				.parse().map_err(|e| E::Number(EP::A, EN::Y, e))?;
			let (b, s) = s.strip_prefix("Button B: ").ok_or(E::Format(EP::B, EF::Line(EL::Prefix)))?
				.split_once('\n').ok_or(E::Format(EP::B, EF::Line(EL::Suffix)))?;
			let (bx, by) = b.split_once(", ").ok_or(E::Format(EP::B, EF::Line(EL::Midfix)))?;
			let bx = bx.strip_prefix("X+").ok_or(E::Format(EP::B, EF::XPrefix))?
				.parse().map_err(|e| E::Number(EP::B, EN::X, e))?;
			let by = by.strip_prefix("Y+").ok_or(E::Format(EP::B, EF::YPrefix))?
				.parse().map_err(|e| E::Number(EP::B, EN::Y, e))?;
			let prize = s.strip_prefix("Prize: ")
				.ok_or(E::Format(EP::Prize, EF::Line(EL::Prefix)))?;
			let (px, py) = prize.split_once(", ").ok_or(E::Format(EP::B, EF::Line(EL::Midfix)))?;
			let px = px.strip_prefix("X=").ok_or(E::Format(EP::B, EF::XPrefix))?
				.parse().map_err(|e| E::Number(EP::Prize, EN::X, e))?;
			let py = py.strip_prefix("Y=").ok_or(E::Format(EP::B, EF::YPrefix))?
				.parse().map_err(|e| E::Number(EP::Prize, EN::Y, e))?;

			Ok(Machine { a: [ax, ay], b: [bx, by], prize: [px, py] })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct MachinesError { offset: usize, source: machine_error::Error }

	pub(super) fn try_machines(s: &str)
	-> impl Iterator<Item = Result<Machine, MachinesError>> + '_
	{
		s.split("\n\n")
			.enumerate()
			.map(|(i, machine)| machine.parse()
				.map_err(|e| MachinesError { offset: i, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		Button A: X+94, Y+34
		Button B: X+22, Y+67
		Prize: X=8400, Y=5400

		Button A: X+26, Y+66
		Button B: X+67, Y+21
		Prize: X=12748, Y=12176

		Button A: X+17, Y+86
		Button B: X+84, Y+37
		Prize: X=7870, Y=6450

		Button A: X+69, Y+23
		Button B: X+27, Y+71
		Prize: X=18641, Y=10279
	"};
	assert_eq!(solve::<0>(parsing::try_machines(INPUT).map(|r| r.unwrap())), 480);
	assert_eq!(part1(), 28138);
	assert_eq!(part2(), 108394825772874);
}
