// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Map {
	plants: Box<[u8]>,
	stride: usize,
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq)]
enum Dir { Up, Down, Left, Right }

impl Map {
	fn adj_idxs(l: usize, s: usize, idx: usize) -> impl Iterator<Item = (Dir, usize)> {
		let mut idxs = [None; 4];
		if idx >= s { idxs[0] = Some((Dir::Up, idx - s)) }
		if idx < l - s { idxs[1] = Some((Dir::Down, idx + s)) }
		let x = idx % s;
		if x > 0 { idxs[2] = Some((Dir::Left, idx - 1)) }
		if x < s - 1 { idxs[3] = Some((Dir::Right, idx + 1)) }
		idxs.into_iter().flatten()
	}

	fn adj_poss(&self, pos: usize) -> impl Iterator<Item = (Dir, usize)> {
		let [l, s] = [self.plants.len(), self.stride];
		Self::adj_idxs(l, s, pos)
	}

	fn adj_verts(&self, corn: usize) -> impl Iterator<Item = (Dir, usize)> {
		let [l, s] = [self.plants.len(), self.stride];
		Self::adj_idxs(l + s + l / s + 1, s + 1, corn)
	}

	fn pos_verts(&self, pos: usize) -> [([Dir; 2], usize); 4] {
		let s = self.stride;
		let [x, y] = [pos % s, pos / s];
		let up_left = y * (s + 1) + x;
		[
			([Dir::Up, Dir::Left], up_left),
			([Dir::Up, Dir::Right], up_left + 1),
			([Dir::Down, Dir::Left], up_left + s + 1),
			([Dir::Down, Dir::Right], up_left + s + 2),
		]
	}

	fn vert_poss(&self, corn: usize) -> [Option<([Dir; 2], usize)>; 4] {
		let [l, s] = [self.plants.len(), self.stride];
		let h = l / s;
		let [x, y] = [corn % (s + 1), corn / (s + 1)];

		let mut poss = [None; 4];
		if x > 0 && y > 0 { poss[0] = Some(([Dir::Up, Dir::Left], (y - 1) * s + x - 1)) }
		if x < s && y > 0 { poss[1] = Some(([Dir::Up, Dir::Right], (y - 1) * s + x)) }
		if x > 0 && y < h { poss[2] = Some(([Dir::Down, Dir::Left], y * s + x - 1)) }
		if x < s && y < h { poss[3] = Some(([Dir::Down, Dir::Right], y * s + x)) }

		poss
	}

	fn is_edge_vert(&self, vert: usize, plant: u8) -> bool {
		self.vert_poss(vert).into_iter().any(|p|
			p.is_none_or(|(_, p)| self.plants[p] != plant))
	}
}


fn input_map() -> Map {
	include_str!("day12.txt").parse().unwrap()
}


pub(crate) fn part1() -> u64 {
	part1_impl(input_map())
}

fn part1_impl(input_map: Map) -> u64 {
	// `None` or the region’s lowest `input_map.plants` index.
	let mut regions = vec![None; input_map.plants.len()].into_boxed_slice();

	let mut queue = std::collections::VecDeque::new();

	let mut score = 0;
	for (first_pos, &plant) in input_map.plants.iter().enumerate() {
		if regions[first_pos].is_some() { continue }

		let mut area = 0;
		let mut circum = 0;

		queue.push_back(first_pos);
		while let Some(pos) = queue.pop_front() {
			if regions[pos].is_some() { continue }
			if input_map.plants[pos] != plant { continue }

			regions[pos] = Some(first_pos);
			area += 1;
			circum += 4;

			for (_, adj_pos) in input_map.adj_poss(pos) {
				if regions[adj_pos] == Some(first_pos) {
					circum -= 2;
				} else {
					queue.push_back(adj_pos)
				}
			}
		}

		score += area * circum;
	}

	score
}


pub(crate) fn part2() -> u64 {
	part2_impl(input_map())
}

fn part2_impl(input_map: Map) -> u64 {
	use std::collections::{BTreeSet, VecDeque};

	let mut poss_visited = vec![false; input_map.plants.len()].into_boxed_slice();
	let mut verts = BTreeSet::new();

	let mut poss_queue = VecDeque::new();

	let mut score = 0;
	for (first_pos, &plant) in input_map.plants.iter().enumerate() {
		if poss_visited[first_pos] { continue }

		let mut area = 0;
		verts.clear();

		poss_queue.push_back(first_pos);
		while let Some(pos) = poss_queue.pop_front() {
			if poss_visited[pos] { continue }
			if input_map.plants[pos] != plant { continue }

			poss_visited[pos] = true;
			area += 1;
			verts.extend(input_map.pos_verts(pos).into_iter().map(|(_, v)| v));

			for (_, adj_pos) in input_map.adj_poss(pos) {
				poss_queue.push_back(adj_pos)
			}
		}

		verts.retain(|&v| input_map.is_edge_vert(v, plant));

		let mut sides = 0;
		while let Some(first_vert) = verts.pop_first() {
			sides += 1;

			let mut dir = Dir::Right;
			let mut vert = first_vert + 1;

			while vert != first_vert {
				let are_poss_region = {
					let poss = input_map.vert_poss(vert);
					std::array::from_fn::<_, 4, _>(|i| poss[i]
						.is_some_and(|(_, p)| input_map.plants[p] == plant))
				};

				verts.remove(&vert);

				for (adj_dir, adj_vert) in input_map.adj_verts(vert) {
					let [ul, ur, dl, dr] = are_poss_region;
					match (dir, adj_dir) {
						(Dir::Up, Dir::Down) => continue,
						(Dir::Down, Dir::Up) => continue,
						(Dir::Left, Dir::Right) => continue,
						(Dir::Right, Dir::Left) => continue,
						(Dir::Up, Dir::Left) if dl && !ul => (),
						(Dir::Up, Dir::Right) if dr && !ur => (),
						(Dir::Down, Dir::Left) if ul && !dl => (),
						(Dir::Down, Dir::Right) if ur && !dr => (),
						(Dir::Left, Dir::Up) if ur && !ul => (),
						(Dir::Left, Dir::Down) if dr && !dl => (),
						(Dir::Right, Dir::Up) if ul && !ur => (),
						(Dir::Right, Dir::Down) if dl && !dr => (),
						(_, Dir::Up) if ul ^ ur && (ul && dl || ur && dr) => (),
						(_, Dir::Down) if dl ^ dr && (dl && ul || dr && ur) => (),
						(_, Dir::Left) if ul ^ dl && (ul && ur || dl && dr) => (),
						(_, Dir::Right) if ur ^ dr && (ur && ul || dr && dl) => (),
						_ => continue,
					}

					if adj_dir != dir { sides += 1 }

					vert = adj_vert;
					dir = adj_dir;
					break
				}
			}
		}

		score += area * sides;
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
		Plant { line: usize, column: usize, found: char },
	}

	impl FromStr for Map {
		type Err = MapError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let mut stride = None;
			let mut plants = Vec::new();
			for (l, line) in s.lines().enumerate() {
				let line_len = line.len();
				if *stride.insert(line_len) != line_len {
					return Err(MapError::Stride { line: l })
				}

				plants.reserve(line_len);
				for (c, byte) in line.bytes().enumerate() {
					match byte {
						b'A'..=b'Z' => plants.push(byte),
						_ => return Err(MapError::Plant { line: l, column: c, found: byte.into() })
					}
				}
			}

			let stride = stride.ok_or(MapError::Empty)?;

			Ok(Map { plants: plants.into(), stride })
		}
	}
}


#[test]
fn tests() {
	const TINY_INPUT: &str = indoc::indoc! {"
		AAAA
		BBCD
		BBCC
		EEEC
	"};
	const OXOXO_INPUT: &str = indoc::indoc! {"
		OOOOO
		OXOXO
		OOOOO
		OXOXO
		OOOOO
	"};
	const LARGER_INPUT: &str = indoc::indoc! {"
		RRRRIICCFF
		RRRRIICCCF
		VVRRRCCFFF
		VVRCCCJFFF
		VVVVCJJCFE
		VVIVCCJJEE
		VVIIICJJEE
		MIIIIIJJEE
		MIIISIJEEE
		MMMISSJEEE
	"};
	const E_INPUT: &str = indoc::indoc! {"
		EEEEE
		EXXXX
		EEEEE
		EXXXX
		EEEEE
	"};
	const ABBA_INPUT: &str = indoc::indoc! {"
		AAAAAA
		AAABBA
		AAABBA
		ABBAAA
		ABBAAA
		AAAAAA
	"};
	assert_eq!(part1_impl(TINY_INPUT.parse::<Map>().unwrap()), 140);
	assert_eq!(part1_impl(OXOXO_INPUT.parse::<Map>().unwrap()), 772);
	assert_eq!(part1_impl(LARGER_INPUT.parse::<Map>().unwrap()), 1930);
	assert_eq!(part1(), 1304764);
	assert_eq!(part2_impl(TINY_INPUT.parse::<Map>().unwrap()), 80);
	assert_eq!(part2_impl(OXOXO_INPUT.parse::<Map>().unwrap()), 436);
	assert_eq!(part2_impl(E_INPUT.parse::<Map>().unwrap()), 236);
	assert_eq!(part2_impl(ABBA_INPUT.parse::<Map>().unwrap()), 368);
	assert_eq!(part2_impl(LARGER_INPUT.parse::<Map>().unwrap()), 1206);
	assert_eq!(part2(), 811148);
}
