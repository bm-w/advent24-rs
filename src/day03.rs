// Copyright (c) 2024 Bastiaan Marinus van de Weerd


enum Instr {
	Do,
	Dont,
	Mul([u64; 2])
}


fn input_instrs() -> impl Iterator<Item = Instr> {
	parsing::instrs(include_str!("day03.txt"))
}


pub(crate) fn part1() -> u64 {
	part1_impl(input_instrs())
}

fn part1_impl(input_instrs: impl Iterator<Item = Instr>) -> u64 {
	input_instrs
		.filter_map(|instr| match instr { Instr::Mul(ops) => Some(ops), _ => None })
		.map(|[lhs, rhs]| lhs * rhs)
		.sum()
}


pub(crate) fn part2() -> u64 {
	part2_impl(input_instrs())
}

fn part2_impl(input_instrs: impl Iterator<Item = Instr>) -> u64 {
	input_instrs
		.scan(true, |acc, instr| Some(match instr {
			Instr::Do => { *acc = true; None }
			Instr::Dont => { *acc = false; None }
			Instr::Mul(ops) => Some((*acc, ops))
		}))
		.flatten()
		.map(|(enabled, [lhs, rhs])| u64::from(enabled) * lhs * rhs)
		.sum()
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::Instr;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum InstrError {
		Prefix,
		OpenParen,
		CloseParen,
		Comma,
		Left(ParseIntError),
		Right(ParseIntError),
		Rest,
	}

	fn parse_mul(s: &str) -> Result<([u64; 2], &str), InstrError> {
		let s = s.strip_prefix("mul").ok_or(InstrError::Prefix)?;
		let s = s.strip_prefix('(').ok_or(InstrError::OpenParen)?;
		let (s, rest) = s.split_once(')').ok_or(InstrError::CloseParen)?;
		let (left, right) = s.split_once(',').ok_or(InstrError::Comma)?;
		let left = left.parse().map_err(InstrError::Left)?;
		let right = right.parse().map_err(InstrError::Right)?;
		Ok(([left, right], rest))
	}

	fn parse_instr(s: &str) -> Result<(Instr, &str), InstrError> {
		if let Some(rest) = s.strip_prefix("do()") {
			Ok((Instr::Do, rest))
		} else if let Some(rest) = s.strip_prefix("don't()") {
			Ok((Instr::Dont, rest))
		} else {
			parse_mul(s).map(|(o, r)| (Instr::Mul(o), r))
		}
	}

	pub(super) fn instrs(s: &str) -> impl Iterator<Item = Instr> + '_ {
		let mut chars = s.chars();
		std::iter::from_fn(move || {
			loop {
				let s = chars.as_str();
				if let Ok((instr, rest)) = parse_instr(s) {
					chars = rest.chars();
					return Some(instr)
				} else {
					_ = chars.next()?;
				}
			}
		})
	}

	impl FromStr for Instr {
		type Err = InstrError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let (instr, rest) = parse_instr(s)?;
			if !rest.is_empty() { return Err(InstrError::Rest) }
			Ok(instr)
		}
	}
}


#[test]
fn tests() {
	const INPUT_PART1: &str
		= "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
	assert_eq!(part1_impl(parsing::instrs(INPUT_PART1)), 161);
	assert_eq!(part1(), 159892596);
	const INPUT_PART2: &str
		= "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";
	assert_eq!(part2_impl(parsing::instrs(INPUT_PART2)), 48);
	assert_eq!(part2(), 92626942);
}
