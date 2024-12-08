// Copyright (c) 2024 Bastiaan Marinus van de Weerd


enum Space {
	Open,
	Tower(u8),
}

struct Map {
	spaces: Box<[Space]>,
	stride: usize,
}

impl Map {
	fn pos(&self, coords: [isize; 2]) -> Option<usize> {
		let x = (coords[0] >= 0).then_some(coords[0] as usize)?;
		if x >= self.stride { return None }
		let y = (coords[1] >= 0).then_some(coords[1] as usize)?;
		if y * self.stride >= self.spaces.len() { return None }
		Some(y * self.stride + x)
	}

	fn coords(&self, pos: usize) -> [isize; 2] {
		[(pos % self.stride) as _, (pos / self.stride) as _]
	}

	fn freq_towers(&self) -> impl Iterator<Item = (u8, Box<[[isize; 2]]>)> + '_ {
		let mut freq_towers = std::collections::HashMap::<u8, Vec<_>>::new();
		for (pos, space) in self.spaces.iter().enumerate() {
			let Space::Tower(freq) = space else { continue };
			freq_towers.entry(*freq)
				.or_insert_with(Vec::new)
				.push(self.coords(pos));
		}
		
		freq_towers.into_iter()
			.map(|(freq, towers)| (freq, towers.into()))
	}
}


fn input_map() -> Map {
	include_str!("day08.txt").parse().unwrap()
}


pub(crate) fn part1() -> usize {
	part1_impl(input_map())
}

fn part1_impl(input_map: Map) -> usize {
	use std::collections::HashSet;
	use itertools::Itertools as _;

	let mut antinode_poss = HashSet::new();
	for (_freq, tower_coords) in input_map.freq_towers() {
		for ([lx, ly], [rx, ry]) in tower_coords.iter().tuple_combinations() {
			let [dx, dy] = [rx - lx, ry - ly];
			if let Some(pos) = input_map.pos([lx - dx, ly - dy]) {
				antinode_poss.insert(pos);
			}
			if let Some(pos) = input_map.pos([rx + dx, ry + dy]) {
				antinode_poss.insert(pos);
			}
		}
	}

	antinode_poss.len()
}


pub(crate) fn part2() -> usize {
	part2_impl(input_map())
}

fn part2_impl(input_map: Map) -> usize {
	use std::collections::HashSet;
	use itertools::Itertools as _;

	let mut antinode_poss = HashSet::new();
	for (_freq, tower_coords) in input_map.freq_towers() {
		for ([lx, ly], [rx, ry]) in tower_coords.iter().tuple_combinations() {
			let [dx, dy] = [rx - lx, ry - ly];
			for i in 0.. {
				let Some(pos) = input_map.pos([lx - i * dx, ly - i * dy]) else { break };
				antinode_poss.insert(pos);
			}
			for i in 0.. {
				let Some(pos) = input_map.pos([rx + i * dx, ry + i * dy]) else { break };
				antinode_poss.insert(pos);
			}
		}
	}

	#[allow(dead_code)]
	#[cfg(test)]
	struct DisplayMap<'a>(&'a Map, Option<&'a HashSet<usize>>);

	#[cfg(test)]
	impl std::fmt::Display for DisplayMap<'_> {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			use std::fmt::Write as _;
			for y in 0..self.0.spaces.len() / self.0.stride {
				let pos_y = self.0.stride * y;
				for x in 0..self.0.stride {
					let pos = pos_y + x;
					let is_antinode = self.1.is_some_and(|s| s.contains(&pos));
					match (&self.0.spaces[pos_y + x], is_antinode) {
						(&Space::Tower(freq), _) => f.write_char(freq.into())?,
						(_, true) => f.write_char('#')?,
						_ => f.write_char('.')?,
					}
				}
				f.write_char('\n')?;
			}
			Ok(())
		}
	}

	antinode_poss.len()
}


mod parsing {
	use std::str::FromStr;
	use super::{Map, Space};

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum MapError {
		Empty,
		Format { line: usize, },
		Tower { line: usize, column: usize, found: u8 },
	}

	impl FromStr for Map {
		type Err = MapError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let mut stride = None;
			let mut spaces = Vec::new();
			for (l, line) in s.lines().enumerate() {
				let line_len = line.len();
				if *stride.insert(line_len) != line_len {
					return Err(MapError::Format { line: l })
				}

				spaces.reserve(line_len);
				for (c, byte) in line.bytes().enumerate() {
					match byte {
						b'.' => spaces.push(Space::Open),
						b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' => spaces.push(Space::Tower(byte)),
						_ => return Err(MapError::Tower { line: l, column: c, found: byte })
					}
				}
			}
			let Some(stride) = stride else { return Err(MapError::Empty) };

			Ok(Map { spaces: spaces.into(), stride })
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		............
		........0...
		.....0......
		.......0....
		....0.......
		......A.....
		............
		............
		........A...
		.........A..
		............
		............
	"};
	assert_eq!(part1_impl(INPUT.parse::<Map>().unwrap()), 14);
	assert_eq!(part1(), 271);
	assert_eq!(part2_impl(INPUT.parse::<Map>().unwrap()), 34);
	assert_eq!(part2(), 994);
}
