// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[derive(Debug, Clone)]
enum Schematic {
	Lock([u8; 5]),
	Key([u8; 5]),
}


pub(crate) fn part1() -> usize {
	part1_impl(parsing::try_schematics(include_str!("day25.txt")).map(Result::unwrap))
}

fn part1_impl<I: Iterator<Item = Schematic> + Clone>(input_schematics: I) -> usize {
	use {itertools::Itertools as _, Schematic::*};

	let mut count = 0;
	'combinations: for (lhs, rhs) in input_schematics.tuple_combinations() {
		match (lhs, rhs) {
			(Lock(lock), Key(key)) | (Key(key), Lock(lock)) => {
				for (l, k) in std::iter::zip(lock, key) {
					if l & k != 0 { continue 'combinations }
				}
				count += 1;
			}
			_ => (),
		}
	}
	count
}


pub(crate) fn part2() -> &'static str {
	"Merry Christmas!"
}


mod parsing {
	use std::str::FromStr;
	use super::Schematic;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum SchematicError {
		LineLen { line: usize },
		NumLines(usize),
		Char { line: usize, column: usize, found: char },
		InvalidLock(Option<usize>),
		InvalidKey(Option<usize>),
	}

	impl FromStr for Schematic {
		type Err = SchematicError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let mut finalize: Option<fn([u8; 5]) -> Self> = None;
			let mut heights = [0; 5];
			for (l, line) in s.lines().enumerate() {
				match (l, line) {
					(_, long) if long.len() != 5 => return Err(Self::Err::LineLen { line: l }),
					(0, ".....") => (),
					(0, "#####") => finalize = Some(Schematic::Lock),
					(6, _) => match finalize.is_some() {
						true => if line != "....." { return Err(Self::Err::InvalidLock(None)) }
						false => if line != "#####" { return Err(Self::Err::InvalidKey(None)) }
							else {  finalize = Some(Schematic::Key) }
					}
					(7, _) => return Err(Self::Err::NumLines(8)),
					(l, line) => for (c, ch) in line.chars().enumerate() {
						match ch {
							'#' => heights[c] |= 1 << (l - 1),
							'.' => (),
							_ => return Err(Self::Err::Char { line: l, column: c, found: ch }),
						}
					}
				}
			}

			let finalize = finalize.ok_or_else(|| SchematicError::NumLines(s.lines().count()))?;
			Ok(match (finalize)(heights) {
				Self::Lock(heights) => {
					for (c, &h) in heights.iter().enumerate() {
						if !(h + 1).is_power_of_two() {
							return Err(SchematicError::InvalidLock(Some(c)))
						}
					}
					Self::Lock(heights)
				}
				Self::Key(heights) => {
					for (c, &h) in heights.iter().enumerate() {
						if !((h ^ 0b11111) + 1).is_power_of_two() {
							return Err(SchematicError::InvalidKey(Some(c)))
						}
					}
					Self::Key(heights)
				}
			})
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct SchematicsError { offset: usize, source: SchematicError }

	pub(super) fn try_schematics(s: &str)
	-> impl Iterator<Item = Result<Schematic, SchematicsError>> + Clone + '_ {
		s.split("\n\n")
			.enumerate()
			.map(|(offset, s)| s.parse()
				.map_err(|e| SchematicsError { offset, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		#####
		.####
		.####
		.####
		.#.#.
		.#...
		.....

		#####
		##.##
		.#.##
		...##
		...#.
		...#.
		.....

		.....
		#....
		#....
		#...#
		#.#.#
		#.###
		#####

		.....
		.....
		#.#..
		###..
		###.#
		###.#
		#####

		.....
		.....
		.....
		#....
		#.#..
		#.#.#
		#####
	"};
	assert_eq!(part1_impl(parsing::try_schematics(INPUT).map(Result::unwrap)), 3);
	assert_eq!(part1(), 3223);
}
