// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Map {
	heights: Box<[usize]>,
	stride: usize,
}

#[derive(Clone, Copy)]
enum Dir { Up, Down, Left, Right }

impl Map {
	fn adj_poss(&self, pos: usize) -> impl Iterator<Item = (Dir, usize)> {
		let [l, s] = [self.heights.len(), self.stride];
		let mut poss = [None; 4];
		if pos >= s { poss[0] = Some((Dir::Up, pos - s)) }
		if pos < l - s { poss[1] = Some((Dir::Down, pos + s)) }
		let x = pos % s;
		if x > 0 { poss[2] = Some((Dir::Left, pos - 1)) }
		if x < s - 1 { poss[3] = Some((Dir::Right, pos + 1)) }
		poss.into_iter().flatten()
	}

	fn trail_ends(&self, head_pos: usize) -> impl Iterator<Item = usize> + '_ {
		debug_assert_eq!(self.heights[head_pos], 0);

		let mut stack = Vec::with_capacity(9);
		stack.push((0, self.adj_poss(head_pos)));
		std::iter::from_fn(move || loop {
			let &mut (height, ref mut iter) = stack.last_mut()?;
			let Some((_, next_pos)) = iter.next() else { stack.pop(); continue };
			let next_height = self.heights[next_pos];
			if next_height != height + 1 { continue }
			if next_height == 9 { return Some(next_pos) }
			stack.push((next_height, self.adj_poss(next_pos)));
		})
	}
}


fn input_map() -> Map {
	include_str!("day10.txt").parse().unwrap()
}


pub(crate) fn part1() -> u64 {
	part1_impl(input_map())
}

fn part1_impl(input_map: Map) -> u64 {
	let mut end_poss = std::collections::HashSet::new();

	let mut score = 0;
	for (head_pos, &height) in input_map.heights.iter().enumerate() {
		if height > 0 { continue }

		end_poss.clear();
		for end_pos in input_map.trail_ends(head_pos) {
			end_poss.insert(end_pos);
		}

		score += end_poss.len() as u64;
	}

	score
}


pub(crate) fn part2() -> u64 {
	part2_impl(input_map())
}

fn part2_impl(input_map: Map) -> u64 {
	let mut score = 0;
	for (head_pos, &height) in input_map.heights.iter().enumerate() {
		if height > 0 { continue }

		for _ in input_map.trail_ends(head_pos) {
			score += 1
		}
	}

	score
}


mod parsing {
	use std::str::FromStr;
	use super::Map;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum MapError {
		Empty,
		Stride { line: usize },
		Height { line: usize, column: usize, found: char },
	}

	impl FromStr for Map {
		type Err = MapError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let mut stride = None;
			let mut heights = Vec::new();
			for (l, line) in s.lines().enumerate() {
				let line_len = line.len();
				if stride.replace(line_len).is_some_and(|l| l != line_len) {
					return Err(MapError::Stride { line: l })
				}

				heights.reserve(line_len);
				for (c, byte) in line.bytes().enumerate() {
					match byte {
						b'0'..=b'9' => heights.push((byte - b'0') as usize),
						_ => return Err(MapError::Height { line: l, column: c, found: byte.into() })
					}
				}
			}

			let stride = stride.ok_or(MapError::Empty)?;

			Ok(Map { heights: heights.into(), stride })
		}
	}
}


#[test]
fn tests() {
	const TINY_INPUT: &str = indoc::indoc! {"
		0123
		1234
		8765
		9876
	"};
	assert_eq!(part1_impl(TINY_INPUT.parse().unwrap()), 1);
	const LARGER_INPUT: &str = indoc::indoc! {"
		89010123
		78121874
		87430965
		96549874
		45678903
		32019012
		01329801
		10456732
	"};
	assert_eq!(part1_impl(LARGER_INPUT.parse().unwrap()), 36);
	assert_eq!(part1(), 624);
	assert_eq!(part2_impl(LARGER_INPUT.parse().unwrap()), 81);
	assert_eq!(part2(), 1483);
}
