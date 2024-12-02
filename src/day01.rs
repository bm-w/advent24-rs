// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Lists([Vec<u64>; 2]);

fn input_lists() -> Lists {
	include_str!("day01.txt").parse().unwrap()
}

pub(crate) fn part1() -> u64 {
	part1_impl(input_lists())
}

fn part1_impl(mut lists: Lists) -> u64 {
	lists.0[0].sort();
	lists.0[1].sort();

	let mut dist = 0;
	for (&left, &right) in std::iter::zip(&lists.0[0], &lists.0[1]) {
		dist += left.abs_diff(right);
	}
	
	dist
}


pub(crate) fn part2() -> u64 {
	part2_impl(input_lists())
}

fn part2_impl(Lists([left, right]): Lists) -> u64 {
	use std::collections::HashMap;

	let mut right_counts = HashMap::<_, usize>::new();
	for right in right {
		*right_counts.entry(right).or_default() += 1;
	}

	left.into_iter()
		.map(|left| left * right_counts.get(&left)
			.copied()
			.unwrap_or_default() as u64)
		.sum::<u64>()
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::Lists;

	#[derive(Debug)]
	pub(super) enum NumberPos { Left, Right }

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum ListsError {
		Format { line: usize },
		Number { line: usize, pos: NumberPos, source: ParseIntError },
	}

	impl FromStr for Lists {
		type Err = ListsError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let mut lists = std::array::from_fn::<Vec<u64>, 2, _>(|_| Vec::new());
			for (l, line) in s.lines().enumerate() {
				let (left, right) = line.split_once("   ")
					.ok_or(ListsError::Format { line: l })?;
				let left = left.parse()
					.map_err(|e| ListsError::Number { line: l, pos: NumberPos::Left, source: e })?;
				lists[0].push(left);
				let right = right.parse()
					.map_err(|e| ListsError::Number { line: l, pos: NumberPos::Right, source: e })?;
				lists[1].push(right);
			}
			Ok(Lists(lists))
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc!{"
		3   4
		4   3
		2   5
		1   3
		3   9
		3   3
	"};
	assert_eq!(part1_impl(INPUT.parse().unwrap()), 11);
	assert_eq!(part1(), 2378066);
	assert_eq!(part2_impl(INPUT.parse().unwrap()), 31);
	assert_eq!(part2(), 18934359);
}
