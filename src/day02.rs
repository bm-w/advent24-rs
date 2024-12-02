// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Report {
	levels: Vec<u64>,
}

impl Report {
	fn are_levels_safe(levels: impl Iterator<Item = u64>) -> bool {
		use itertools::Itertools as _;
		levels.tuple_windows().all(|(l, r)| l < r && l + 3 >= r)
	}

	fn is_safe(&self, dampen: bool) -> bool {
		let skip = if dampen {
			0..self.levels.len()
		} else {
			usize::MAX - 1..usize::MAX
		};

		for n in skip {
			if Self::are_levels_safe(self.levels.iter().copied().skip_nth(n)) { return true }
			if Self::are_levels_safe(self.levels.iter().copied().rev().skip_nth(n)) { return true }
		}
		false
	}
}


fn input_reports() -> impl Iterator<Item = Report> {
	parsing::try_reports(include_str!("day02.txt")).map(|r| r.unwrap())
}

pub(crate) fn part1() -> usize {
	part1_and_2_impl(input_reports(), false)
}

pub(crate) fn part2() -> usize {
	part1_and_2_impl(input_reports(), true)
}

fn part1_and_2_impl(
	reports: impl Iterator<Item = Report>,
	dampen: bool,
) -> usize {
	reports
		.filter(|r| r.is_safe(dampen))
		.count()
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::Report;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum ReportError {
		Level { offset: usize, source: ParseIntError, }
	}

	impl FromStr for Report {
		type Err = ReportError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let levels = s.split(' ')
				.enumerate()
				.map(|(i, num)| num.parse::<u64>()
					.map_err(|e| ReportError::Level { offset: i, source: e }))
				.collect::<Result<_, _>>()?;
			Ok(Report { levels })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct ReportsError {
		line: usize,
		source: ReportError,
	}

	pub(super) fn try_reports(s: &str) -> impl Iterator<Item = Result<Report, ReportsError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| line.parse()
				.map_err(|e| ReportsError { line: l, source: e }))
	}
}

trait SkipNth: Iterator {
	fn skip_nth(&mut self, n: usize) -> impl Iterator<Item = Self::Item> {
		self.enumerate().flat_map(move |(i, item)| (i != n).then_some(item))
	}
}

impl<I> SkipNth for I where I: Iterator {}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		7 6 4 2 1
		1 2 7 8 9
		9 7 6 2 1
		1 3 2 4 5
		8 6 4 4 1
		1 3 6 7 9
	"};
	assert_eq!(part1_and_2_impl(parsing::try_reports(INPUT).map(|r| r.unwrap()), false), 2);
	assert_eq!(part1(), 269);
	assert_eq!(part1_and_2_impl(parsing::try_reports(INPUT).map(|r| r.unwrap()), true), 4);
	assert_eq!(part2(), 337);
}