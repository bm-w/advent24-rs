// Copyright (c) 2024 Bastiaan Marinus van de Weerd

use std::collections::HashSet;


enum Space {
	Open,
	Obstr,
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Dir { Up, Down, Left, Right }

impl Dir {
	fn next(&self) -> Self {
		match self {
			Self::Up => Self::Right,
			Self::Down => Self::Left,
			Self::Left => Self::Up,
			Self::Right => Self::Down,
		}
	}

	fn cycle(&mut self) {
		*self = self.next();
	}
}

struct Guard {
	pos: usize,
	dir: Dir,
}

struct Lab {
	spaces: Box<[Space]>,
	stride: usize,
	guard: Option<Guard>,
}

impl Lab {
	fn _next_pos(spaces: &[Space], stride: usize, pos: usize, dir: Dir) -> Option<usize> {
		match dir {
			Dir::Up if pos >= stride => Some(pos - stride),
			Dir::Down if pos < spaces.len() - stride => Some(pos + stride),
			Dir::Left if pos % stride > 0 => Some(pos - 1),
			Dir::Right if pos % stride < stride - 1 => Some(pos + 1),
			_ => None,
		}
	}

	fn next_pos(&self, pos: usize, dir: Dir) -> Option<usize> {
		Self::_next_pos(&self.spaces, self.stride, pos, dir)
	}

	fn _tick<'a>(
		spaces: &[Space],
		stride: usize,
		guard: &'a mut Option<Guard>,
		added_obstr: Option<usize>,
	) -> Option<(&'a Guard, bool)> {
		let Some(Guard { pos: guard_pos, dir: guard_dir }) = guard else { return None };

		match Self::_next_pos(&spaces, stride, *guard_pos, *guard_dir) {
			Some(next_pos)
				if matches!(&spaces[next_pos], Space::Open)
				&& !added_obstr.as_ref().is_some_and(|&p| next_pos == p)
			=> {
				*guard_pos = next_pos;
				return guard.as_ref().map(|g| (g, false))
			}
			Some(_) => {
				guard_dir.cycle();
				return guard.as_ref().map(|g| (g, true))
			}
			None => {
				*guard = None;
				return None;
			}
		}
	}

	/// Returns a reference to the guard if they remain in the lab, and whether
	/// they just turned.
	fn tick(&mut self) -> Option<(&Guard, bool)> {
		Self::_tick(&self.spaces, self.stride, &mut self.guard, None)
	}
}


fn input_lab() -> Lab {
	include_str!("day06.txt").parse().unwrap()
}


pub(crate) fn part1() -> usize {
	part1_impl(input_lab())
}

fn part1_impl(mut input_lab: Lab) -> usize {
	use std::collections::HashSet;

	let mut guard_poss = HashSet::new();
	if let Some(guard) = &input_lab.guard {
		guard_poss.insert(guard.pos);
	}
	while let Some((guard, _)) = input_lab.tick() {
		guard_poss.insert(guard.pos);
	}
	guard_poss.len()
}


pub(crate) fn part2() -> usize {
	part2_impl(input_lab())
}

fn part2_impl(mut input_lab: Lab) -> usize {

	#[derive(Clone, Copy, PartialEq, Eq, Hash)]
	struct Turn {
		pos: usize,
		dir: Dir,
	}

	struct VirtualLab<'a> {
		lab: &'a Lab,
		added_obstr: usize,
		guard: Option<Guard>,
	}

	impl VirtualLab<'_> {
		fn tick(&mut self) -> Option<(&Guard, bool)> {
			Lab::_tick(&self.lab.spaces, self.lab.stride, &mut self.guard, Some(self.added_obstr))
		}
	}

	#[cfg(test)]
	#[allow(non_local_definitions)]
	impl From<Dir> for char {
		fn from(dir: Dir) -> Self {
			match dir {
				Dir::Up => '^',
				Dir::Down => 'v',
				Dir::Left => '<',
				Dir::Right => '>',
			}
		}
	}

	#[cfg(test)]
	impl std::fmt::Display for VirtualLab<'_> {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			use std::fmt::Write as _;
			for y in 0..self.lab.spaces.len() / self.lab.stride {
				for x in 0..self.lab.stride {
					let pos = self.lab.stride * y + x;
					f.write_char(match (self.guard.as_ref(), &self.lab.spaces[pos]) {
						_ if pos == self.added_obstr => 'O',
						(Some(guard), _) if guard.pos == pos => guard.dir.into(),
						(_, Space::Open) => '.',
						(_, Space::Obstr) => '#',
					})?;
				}
				f.write_char('\n')?;
			}
			Ok(())
		}
	}

	let mut guard_poss = HashSet::new();
	if let Some(guard) = &input_lab.guard {
		guard_poss.insert(guard.pos);
	}

	let mut loop_obstrs = HashSet::new();

	let mut turns = HashSet::new();
	while let Some((&Guard { pos: guard_pos, dir: guard_dir }, turned)) = input_lab.tick() {
		if turned {
			turns.insert(Turn { pos: guard_pos, dir: guard_dir });
		} else {
			guard_poss.insert(guard_pos);
		}

		let Some(next_pos) = input_lab.next_pos(guard_pos, guard_dir) else { break };
		if !matches!(input_lab.spaces[next_pos], Space::Open) { continue }
		if guard_poss.contains(&next_pos) || loop_obstrs.contains(&next_pos) { continue }

		let mut virtual_lab = VirtualLab {
			lab: &input_lab,
			added_obstr: next_pos,
			guard: Some(Guard { pos: guard_pos, dir: guard_dir.next() })
		};

		let mut virtual_turns = turns.clone();
		while let Some((virtual_guard, turned)) = virtual_lab.tick() {
			if !turned { continue }

			if !virtual_turns.insert(Turn { pos: virtual_guard.pos, dir: virtual_guard.dir }) {
				loop_obstrs.insert(next_pos);
				break;
			}
		}
	}

	loop_obstrs.len()
}


mod parsing {
	use std::str::FromStr;
	use super::{Dir, Guard, Lab, Space};

	impl TryFrom<u8> for Space {
		type Error = Option<Dir>;
		fn try_from(value: u8) -> Result<Self, Self::Error> {
			match value {
				b'.' => Ok(Self::Open),
				b'#' => Ok(Self::Obstr),
				b'^' => Err(Some(Dir::Up)),
				b'v' => Err(Some(Dir::Down)),
				b'<' => Err(Some(Dir::Left)),
				b'>' => Err(Some(Dir::Right)),
				_ => Err(None),
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum LabError {
		Empty,
		Format { line: usize },
		Space { line: usize, column: usize, found: u8 },
	}

	impl FromStr for Lab {
		type Err = LabError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let mut spaces = Vec::new();
			let mut stride = None;
			let mut guard = None;
			for (l, line) in s.lines().enumerate() {
				let len = line.len();
				if *stride.get_or_insert(len) != len {
					return Err(LabError::Format { line: l })
				}

				spaces.reserve(len);
				for (c, byte) in line.bytes().enumerate() {
					match Space::try_from(byte) {
						Ok(space) => spaces.push(space),
						Err(Some(guard_dir)) => {
							guard = Some(Guard { pos: spaces.len(), dir: guard_dir });
							spaces.push(Space::Open);
						}
						_ => return Err(LabError::Space { line: l, column: c, found: byte })
					}
				}
			}

			let stride = stride.ok_or(LabError::Empty)?;

			Ok(Lab { spaces: spaces.into(), stride, guard })
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		....#.....
		.........#
		..........
		..#.......
		.......#..
		..........
		.#..^.....
		........#.
		#.........
		......#...
	"};
	assert_eq!(part1_impl(INPUT.parse().unwrap()), 41);
	assert_eq!(part1(), 5404);
	assert_eq!(part2_impl(INPUT.parse().unwrap()), 6);
	const ANOTHER_INPUT: &str = indoc::indoc! {"
		...........#.....#......
		...................#....
		...#.....##.............
		......................#.
		..................#.....
		..#.....................
		....................#...
		........................
		.#........^.............
		..........#..........#..
		..#.....#..........#....
		........#.....#..#......
	"};
	assert_eq!(part2_impl(ANOTHER_INPUT.parse().unwrap()), 19);
	assert_eq!(part2(), 1984);
}
