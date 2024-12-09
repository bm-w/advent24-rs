// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Entry {
	blocks: usize,
	free: usize,
}

struct Map {
	entries: Box<[Entry]>,
	last_blocks: usize,
}


fn input_map() -> Map {
	include_str!("day09.txt").parse().unwrap()
}


pub(crate) fn part1() -> u64 {
	part1_impl(input_map())
}

fn part1_impl(input_map: Map) -> u64 {
	if input_map.entries.is_empty() { return 0 }

	#[derive(Default)]
	struct Start {
		pos: usize,
		idx: usize,
		rem_free: Option<usize>,
	}

	impl Start {
		fn new(prev: Self) -> Self {
			debug_assert_eq!(prev.rem_free, Some(0));
			Self { pos: prev.pos, idx: prev.idx + 1, rem_free: None }
		}
	}

	struct End {
		idx: Option<usize>,
		rem_blocks: usize,
	}

	impl End {
		fn new(prev: Option<&Self>, map: &Map) -> Self {
			let Some(prev) = prev else { return Self { idx: None, rem_blocks: map.last_blocks } };

			debug_assert_eq!(prev.rem_blocks, 0);
			let idx = prev.idx.unwrap_or_else(|| map.entries.len()) - 1;
			Self { idx: Some(idx), rem_blocks: map.entries[idx].blocks }
		}

		fn id(end: Option<usize>, map: &Map) -> u64 {
			end.unwrap_or_else(|| map.entries.len()) as u64
		}
	}

	let mut start = Start::default();
	let mut end = End::new(None, &input_map);
	let mut checksum = 0;

	loop {
		match (&mut start, &mut end) {
			(Start { idx: ref start, .. }, End { idx: ref end, .. }) if Some(*start) == *end =>
				break,
			(Start { rem_free: Some(0), ..}, _) => start = Start::new(start),
			(Start { pos, ref idx, rem_free: rem_free @ None, .. }, _) => {
				let entry = &input_map.entries[*idx];
				let start_pos = *pos;
				*pos += entry.blocks;
				checksum += (start_pos..*pos).map(|p| (p * *idx) as u64).sum::<u64>();
				*rem_free = Some(entry.free);
			}
			(
				Start { pos, rem_free: Some(rem_free @ 1..), .. },
				End { ref idx, rem_blocks: rem_blocks @ 1.. }
			) => {
				let blocks = (*rem_blocks).min(*rem_free);
				*rem_free -= blocks;
				*rem_blocks -= blocks;
				let start_pos = *pos;
				*pos += blocks;
				let id = End::id(*idx, &input_map);
				checksum += (start_pos..*pos).map(|p| p as u64 * id).sum::<u64>();
			}
			(Start { rem_free: Some(1..), .. }, end) => {
				*end = End::new(Some(end), &input_map)
			}
		}
	}

	let id = End::id(end.idx, &input_map);
	checksum += (start.pos..start.pos + end.rem_blocks).map(|p| p as u64 * id).sum::<u64>();

	checksum
}


pub(crate) fn part2() -> u64 {
	part2_impl(input_map())
}

fn part2_impl(input_map: Map) -> u64 {
	if input_map.entries.is_empty() { return 0 }

	struct Free {
		rem: usize,
	}

	struct End {
		idx: Option<usize>,
	}

	impl End {
		fn new(prev: Option<&Self>, map: &Map) -> Self {
			let Some(prev) = prev else { return Self { idx: None } };

			let idx = prev.idx.unwrap_or_else(|| map.entries.len()) - 1;
			Self { idx: Some(idx) }
		}
	}

	let mut frees = Vec::with_capacity(input_map.entries.len());

	let mut end = End::new(None, &input_map);
	let mut checksum = 0;

	loop {
		let (end_idx, blocks) = end.idx.map_or_else(
			|| (input_map.entries.len(), input_map.last_blocks),
			|i| (i, input_map.entries[i].blocks),
		);

		let mut pos = 0;
		for idx in 0..end_idx {
			let entry = &input_map.entries[idx];
			pos += entry.blocks;
			let free = if let Some(free) = frees.get_mut(idx) {
				free
			} else {
				frees.push(Free { rem: entry.free });
				frees.last_mut().unwrap()
			};
			if free.rem >= blocks {
				pos += entry.free - free.rem;
				free.rem -= blocks;
				break;
			} else {
				pos += entry.free;
			}
		}

		let end_pos = pos + blocks;
		checksum += (pos..end_pos).map(|p| (p * end_idx) as u64).sum::<u64>();

		if end.idx == Some(0) { break }
		end = End::new(Some(&end), &input_map);
	}

	checksum
}


mod parsing {
	use std::str::FromStr;
	use super::{Entry, Map};

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum MapError {
		Blocks { offset: usize, found: u8 },
		Free { offset: usize, found: u8 },
		Len(usize),
		LastBlocks(u8),
	}

	impl FromStr for Map {
		type Err = MapError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use itertools::Itertools as _;

			let s = s.trim_end_matches('\n');

			let mut entries = Vec::new();
			for (i, (blocks, free)) in s.bytes().tuples().enumerate() {
				if !blocks.is_ascii_digit() {
					return Err(MapError::Blocks { offset: 2 * i, found: blocks })
				}
				if !free.is_ascii_digit() {
					return Err(MapError::Free { offset: 2 * i + 1, found: free })
				}

				entries.push(Entry { blocks: (blocks - b'0') as _, free: (free - b'0') as _ });
			}

			if s.len() != entries.len() * 2 + 1 {
				return Err(MapError::Len(s.len()));
			}

			let last_blocks = *s.as_bytes().last().unwrap();
			if !last_blocks.is_ascii_digit() {
				return Err(MapError::LastBlocks(last_blocks))
			}

			Ok(Map { entries: entries.into(), last_blocks: (last_blocks - b'0') as _ })
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = "2333133121414131402";
	assert_eq!(part1_impl(INPUT.parse::<Map>().unwrap()), 1928);
	assert_eq!(part1(), 6288707484810);
	assert_eq!(part2_impl(INPUT.parse::<Map>().unwrap()), 2858);
	assert_eq!(part2(), 6311837662089);
}
