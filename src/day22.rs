// Copyright (c) 2024 Bastiaan Marinus van de Weerd


fn mix(lhs: u64, rhs: u64) -> u64 {
	lhs ^ rhs
}

fn prune(val: u64) -> u64 {
	val % 16777216
}

fn next(mut secret: u64) -> u64 {
	let mult = secret * 64;
	secret = prune(mix(mult, secret));
	let div = secret / 32;
	secret = prune(mix(div, secret));
	let mult = secret * 2048;
	prune(mix(mult, secret))
}


fn input_secret_numbers() -> impl Iterator<Item = u64> {
	parsing::try_secret_numbers(include_str!("day22.txt")).map(Result::unwrap)
}


pub(crate) fn part1() -> u64 {
	part1_impl(input_secret_numbers())
}

fn part1_impl(input_secret_numbers: impl Iterator<Item = u64>) -> u64 {
	let mut sum = 0;

	for mut secret_number in input_secret_numbers {
		for _ in 0..2000 { secret_number = next(secret_number) }
		sum += secret_number;
	}

	sum
}


pub(crate) fn part2() -> usize {
	part2_impl(input_secret_numbers())
}

fn part2_impl(input_secret_numbers: impl Iterator<Item = u64>) -> usize {

	// Each change (-9..=+9) can be encoded into 5 bits (as 0..=19), so that a
	// sequence of four consecutive changes can be stored in 20 bits. The yield
	// for each sequence is then stored in a 2^20-element (about 1M) buffer. A
	// total possible yield of 3500+ monkey salesmen fits in `i16::MAX`.
	let mut total_counts = vec![0i16; 1 << 20];

	// Reused for every secret number (resetting to all-`false`).
	let mut seen = vec![false; 1 << 20]; // TODO: Bitfield?

	for (i, mut secret_number) in input_secret_numbers.enumerate() {
		if i > 0 { seen.fill(false) }

		let mut changes = 0;
		let mut prev = (secret_number % 10) as i16;

		for i in 0..2000 {
			secret_number = next(secret_number);
			let curr = (secret_number % 10) as i16;

			// Consecutive sequence of changes are easily obtained by some
			// some shifting and masking.
			changes <<= 5;
			changes &= 0b11111_11111_11111_11111;
			changes |= (9 + curr - prev) as usize;

			if i >= 3 && !std::mem::replace(&mut seen[changes as usize], true) {
				total_counts[changes] += curr;
			}

			prev = curr;
		}
	}

	total_counts.into_iter().max().unwrap() as usize
}


mod parsing {
	use std::num::ParseIntError;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct SecretNumbersError { line: usize, source: ParseIntError }

	pub(super) fn try_secret_numbers(s: &str)
	-> impl Iterator<Item = Result<u64, SecretNumbersError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(line, s)| s.parse()
				.map_err(|source| SecretNumbersError { line, source }))
	}
}


#[test]
fn tests() {
	const INPUT1: &str = indoc::indoc! {"
		1
		10
		100
		2024
	"};
	assert_eq!(part1_impl(parsing::try_secret_numbers(INPUT1).map(Result::unwrap)), 37327623);
	assert_eq!(part1(), 20332089158);
	const INPUT2: &str = indoc::indoc! {"
		1
		2
		3
		2024
	"};
	assert_eq!(part2_impl(parsing::try_secret_numbers(INPUT2).map(Result::unwrap)), 23);
	assert_eq!(part2(), 2191);
}
