// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[derive(PartialEq, Eq)]
struct OrdRule([u64; 2]);

struct Update(Box<[u64]>);


macro_rules! inputs_t {
	() => { (Box<[OrdRule]>, impl Iterator<Item = Update>) }
}

fn inputs_from_str(s: &'static str) -> inputs_t!() {
	let (ord_rules, rest) = parsing::try_ord_rules(s).unwrap();
	let rest = rest.strip_prefix('\n').unwrap();
	(ord_rules, parsing::try_updates(rest).map(Result::unwrap))
}

fn inputs() -> inputs_t!() {
	inputs_from_str(include_str!("day05.txt"))
}


pub(crate) fn part1() -> u64 {
	part1_impl(inputs())
}

fn part1_impl((input_ord_rules, input_updates): inputs_t!()) -> u64 {
	input_updates
		.filter_map(|update| {
			use itertools::Itertools as _;

			for (&left, &right) in update.0.iter().tuple_combinations() {
				if input_ord_rules.contains(&OrdRule([right, left])) {
					return None
				}
			}
			Some(update.0[update.0.len() / 2])
		})
		.sum()
}


pub(crate) fn part2() -> u64 {
	part2_impl(inputs())
}

fn part2_impl((input_ord_rules, input_updates): inputs_t!()) -> u64 {
	input_updates
		.filter(|update| {
			use itertools::Itertools as _;

			for (&left, &right) in update.0.iter().tuple_combinations() {
				if input_ord_rules.contains(&OrdRule([right, left])) {
					return true
				}
			}
			false
		})
		.map(|update| {
			use std::collections::{HashMap, VecDeque};

			// Kahn’s algorithm

			// First, compute the ‘degrees’ of right pages: count the numbers
			// of rules that require that a page be ordered after other pages.
			let mut right_degrees = input_ord_rules.iter()
				.fold(HashMap::new(), |mut acc, &OrdRule([left, right])| {
					if update.0.contains(&left) && update.0.contains(&right) {
						*acc.entry(right).or_insert(0u64) += 1;
					}
					acc
				});

			// Then, initialize a queue with those pages that are not ordered
			// after any other pages (those with 0-degrees).
			let mut queue = VecDeque::new();
			for &page in &update.0 {
				if !right_degrees.contains_key(&page) {
					queue.push_back(page);
				}
			}

			// Lastly, consume the queue, adding the dequeued pages to the
			// ordered list and decrementing the degrees of right pages:
			// discounting the numbers of rules that require that require that
			// pages be ordered after the dequeued pages.
			let mut ordered = Vec::new();
			while let Some(page) = queue.pop_front() {
				ordered.push(page);

				for &OrdRule([left, right]) in &input_ord_rules {
					if left != page { continue }
					if let Some(in_degree) = right_degrees.get_mut(&right) {
						*in_degree -= 1;

						// When a degree reaches zero, add the right page to
						// the queue.
						if *in_degree == 0 {
							queue.push_back(right);
						}
					}
				}
			}

			assert_eq!(ordered.len(), update.0.len());

			ordered[ordered.len() / 2]
		})
		.sum()
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::{OrdRule, Update};

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum OrdRuleError {
		Pipe,
		Left(ParseIntError),
		Right(ParseIntError),
	}

	impl FromStr for OrdRule {
		type Err = OrdRuleError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let (left, right) = s.split_once('|').ok_or(OrdRuleError::Pipe)?;
			let left = left.parse().map_err(OrdRuleError::Left)?;
			let right = right.parse().map_err(OrdRuleError::Right)?;
			Ok(OrdRule([left, right]))
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub struct OrdRulesError {
		line: usize,
		source: OrdRuleError,
	}

	pub(super) fn try_ord_rules(s: &str) -> Result<(Box<[OrdRule]>, &str), OrdRulesError> {
		let mut rest = None;
		let mut ord_rules = Vec::new();
		for (l, line) in s.lines().enumerate() {
			if line.is_empty() {
				// SAFETY: `line` is in `s` at a non-negative offset.
				let offset = unsafe { line.as_ptr().offset_from(s.as_ptr()) } as usize;
				rest = Some(&s[offset..]);
				break
			}

			let ord_rule = line.parse().map_err(|e| OrdRulesError { line: l, source: e })?;
			ord_rules.push(ord_rule);
		}

		Ok((ord_rules.into_boxed_slice(), rest.unwrap_or_else(|| &s[s.len()..])))
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum UpdateError {
		Empty,
		Page { offset: usize, source: ParseIntError },
	}

	impl FromStr for Update {
		type Err = UpdateError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let mut pages = Vec::new();
			for (i, page) in s.split(',').enumerate() {
				let page = page.parse().map_err(|e| UpdateError::Page { offset: i, source: e })?;
				pages.push(page);
			}
			if pages.is_empty() { return Err(UpdateError::Empty) }
			Ok(Update(pages.into_boxed_slice()))
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub struct UpdatesError {
		line: usize,
		source: UpdateError,
	}	

	pub(super) fn try_updates(s: &str) -> impl Iterator<Item = Result<Update, UpdatesError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| line.parse()
				.map_err(|e| UpdatesError { line: l, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		47|53
		97|13
		97|61
		97|47
		75|29
		61|13
		75|53
		29|13
		97|29
		53|29
		61|53
		97|53
		61|29
		47|13
		75|47
		97|75
		47|61
		75|61
		47|29
		75|13
		53|13

		75,47,61,53,29
		97,61,53,29,13
		75,29,13
		75,97,47,61,53
		61,13,29
		97,13,75,29,47
	"};
	assert_eq!(part1_impl(inputs_from_str(INPUT)), 143);
	assert_eq!(part1(), 4872);
	assert_eq!(part2_impl(inputs_from_str(INPUT)), 123);
	assert_eq!(part2(), 5564);
}
