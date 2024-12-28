// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Stone(u64);

impl Stone {
	fn blink(&mut self) -> Option<Self> {
		if self.0 == 0 {
			self.0 = 1;
			return None
		}

		let digits = crate::util::ulog10(self.0) + 1;
		if digits % 2 == 1 {
			self.0 *= 2024;
			return None
		}

		let modulus = 10_u64.pow(digits as u32 / 2);
		let next = self.0 % modulus;
		self.0 /= modulus;

		Some(Stone(next))
	}
}


fn input_stones() -> Vec<Stone> {
	parsing::try_stones(include_str!("day11.txt")).unwrap()
}

pub(crate) fn part1() -> usize {
	dynamic::<25>(input_stones())
}

pub(crate) fn part2() -> usize {
	dynamic::<75>(input_stones())
}

#[allow(dead_code)]
fn naive<const N: usize>(mut input_stones: Vec<Stone>) -> usize {
	for _ in 0..N {
		let mut i = 0;
		while i < input_stones.len() {
			let insert_stone = input_stones[i].blink();
			if let Some(insert_stone) = insert_stone {
				input_stones.insert(i + 1, insert_stone);
				i += 2;
			} else {
				i += 1;
			}
		}
	}
	input_stones.len()
}

fn dynamic<const N: usize>(input_stones: Vec<Stone>) -> usize {
	use std::collections::HashMap;

	#[derive(PartialEq, Eq, Hash)]
	struct Key {
		stone_num: u64,
		blinks: usize,
	}

	let mut cache = HashMap::new();
	fn stones(mut initial: Stone, blinks: usize, cache: &mut HashMap<Key, usize>) -> usize {
		if blinks == 0 { return 1 }
		if initial.0 == 0 { return stones(Stone(1), blinks - 1, cache) }

		let key = Key { stone_num: initial.0, blinks };
		if let Some(stones) = cache.get(&key) {
			return *stones
		}

		let next = initial.blink();
		let res = stones(initial, blinks - 1, cache)
			+ next.map_or(0, |s| stones(s, blinks - 1, cache));

		cache.insert(key, res);
		res
	}

	input_stones.into_iter()
		.map(|s| stones(s, N, &mut cache))
		.sum()
}


mod parsing {
	use std::num::ParseIntError;
	use super::Stone;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct StonesError {
		offset: usize,
		source: ParseIntError,
	}

	pub(super) fn try_stones(s: &str) -> Result<Vec<Stone>, StonesError> {
		s.trim_end_matches('\n')
			.split(' ')
			.enumerate()
			.map(|(i, s)| s.parse()
				.map(Stone)
				.map_err(|e| StonesError { offset: i, source: e }))
			.collect()
	}
}


#[test]
fn tests() {
	const TINY_INPUT: &str = "0 1 10 99 999";
	assert_eq!(naive::<1>(parsing::try_stones(TINY_INPUT).unwrap()), 7);
	assert_eq!(dynamic::<1>(parsing::try_stones(TINY_INPUT).unwrap()), 7);
	const LONGER_INPUT: &str = "125 17";
	assert_eq!(naive::<6>(parsing::try_stones(LONGER_INPUT).unwrap()), 22);
	assert_eq!(dynamic::<6>(parsing::try_stones(LONGER_INPUT).unwrap()), 22);
	assert_eq!(naive::<25>(parsing::try_stones(LONGER_INPUT).unwrap()), 55312);
	assert_eq!(dynamic::<25>(parsing::try_stones(LONGER_INPUT).unwrap()), 55312);
	assert_eq!(part1(), 209412);
	assert_eq!(part2(), 248967696501656);
}
