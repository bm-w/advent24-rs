// Copyright (c) 2024 Bastiaan Marinus van de Weerd


enum Space { Wall, Open }

struct Maze {
	spaces: Box<[Space]>,
	stride: usize,
	start: usize,
	end: usize,
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Dir { North, East, South, West }

impl Dir {
	fn is_opposite(&self, other: Dir) -> bool {
		match (self, other) {
			| (Self::North, Self::South)
			| (Self::East, Self::West)
			| (Self::South, Self::North)
			| (Self::West, Self::East) => true,
			_ => false,
		}
	}
}

impl Maze {
	fn adj_poss(&self, pos: usize) -> impl Iterator<Item = (Dir, usize)> {
		let [s, l] = [self.stride, self.spaces.len()];
		let x = pos % s;
		let mut poss = [None; 4];
		if pos >= s { poss[0] = Some((Dir::North, pos - s)) }
		if x < s - 1 { poss[1] = Some((Dir::East, pos + 1)) }
		if pos < l - s { poss[2] = Some((Dir::South, pos + s)) }
		if x > 0 { poss[3] = Some((Dir::West, pos - 1)) }
		poss.into_iter().flatten()
	}
}


mod path {
	use std::{collections::{BinaryHeap, HashSet}, rc::Rc};
	use super::{Dir, Maze, Space};

	#[derive(PartialEq, Eq)]
	pub(super) struct Step {
		pub(super) pos: usize,
		pub(super) dir: Dir,
		pub(super) score: u64,
		pub(super) prev: Option<Rc<Step>>,
	}

	impl PartialOrd for Step {
		fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
			Some(self.cmp(other))
		}
	}

	impl Ord for Step {
		fn cmp(&self, other: &Self) -> std::cmp::Ordering {
			other.score.cmp(&self.score).then_with(|| self.pos.cmp(&other.pos))
		}
	}

	impl Step {
		pub(super) fn prevs(&self) -> impl Iterator<Item = &Step> + '_ {
			let mut prev = &self.prev;
			std::iter::from_fn(move || {
				let step = prev.as_deref()?;
				prev = &step.prev;
				Some(step)
			})
		}
	}

	pub(super) struct BestPartialPaths<'a> {
		maze: &'a Maze,
		end: (usize, Option<Dir>),
		trace: bool,
		heap: BinaryHeap<Step>,
		visited: HashSet<(usize, Dir)>,
		best_score: Option<u64>,
		pub(super) joins: Vec<Step>,
	}

	impl Maze {
		pub(super) fn best_partial_paths(
			&self,
			(start_pos, start_dir): (usize, Dir),
			end: (usize, Option<Dir>),
			best_score: Option<u64>,
			trace: bool,
		) -> BestPartialPaths<'_> {
			let heap = BinaryHeap::from_iter([Step {
				pos: start_pos,
				dir: start_dir,
				score: 0,
				prev: None,
			}]);
			let visited = HashSet::with_capacity(self.spaces.len());
			let joins = Vec::new();
			BestPartialPaths { maze: self, end, trace, heap, visited, best_score, joins }
		}

		pub(super) fn best_paths(&self, trace: bool) -> BestPartialPaths<'_> {
			self.best_partial_paths((self.start, Dir::East), (self.end, None), None, trace)
		}
	}

	impl Iterator for BestPartialPaths<'_> {
		type Item = Step;

		fn next(&mut self) -> Option<Self::Item> {
			while let Some(Step { pos, dir, score, prev }) = self.heap.pop() {
				if self.best_score.is_some_and(|s| s < score) { continue }

				if pos == self.end.0 && self.end.1.map_or(true, |d| dir == d) {
					self.best_score = Some(score);
					return Some(Step { pos, dir, score, prev })
				}

				if !self.visited.insert((pos, dir)) {
					if self.trace { self.joins.push(Step { pos, dir, score, prev });}
					continue
				}

				// #[cfg(test)] eprintln!("{pos} [{},{}], {dir:?}, {score}", pos % self.stride, pos / self.stride);

				let prev = self.trace.then(|| {
					let prev = prev.as_ref().map(|p| p.clone());
					Rc::new(Step { pos, dir, score, prev })
				});

				for (adj_dir, adj_pos) in self.maze.adj_poss(pos) {
					if dir.is_opposite(adj_dir) { continue }
					if matches!(self.maze.spaces[adj_pos], Space::Wall) { continue }
					let prev = prev.as_ref().map(|p| p.clone());
					self.heap.push(if dir == adj_dir {
						Step { pos: adj_pos, dir, score: score + 1, prev }
					} else {
						Step { pos, dir: adj_dir, score: score + 1000, prev }
					});
				}
			}

			return None
		}
	}
}


#[cfg(test)]
mod display {
	pub(super) struct DisplayMaze<'a, F>(pub(super) &'a super::Maze, pub(super) F);

	impl<'a, F> std::fmt::Display for DisplayMaze<'a, F>
	where F: Fn(usize) -> Option<char> {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			use std::fmt::Write as _;

			let [s, h] = [self.0.stride, self.0.spaces.len() / self.0.stride];
			for y in 0..h {
				for x in 0..s {
					let pos = y * s + x;
					f.write_char(if let Some(chr) = self.1(pos) { chr }
					else if pos == self.0.start { 'S' }
					else if pos == self.0.end { 'E' }
					else { match self.0.spaces[pos] {
						super::Space::Wall => '#',
						super::Space::Open => '.',
					} })?;
				}
				if y + 1 < h { f.write_char('\n')? }
			}
			Ok(())
		}
	}

	impl std::fmt::Display for super::Maze {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			DisplayMaze(self, |_| None).fmt(f)
		}
	}
}


fn input_maze() -> Maze {
	include_str!("day16.txt").parse().unwrap()
}


pub(crate) fn part1() -> u64 {
	part1_impl(input_maze())
}

fn part1_impl(input_maze: Maze) -> u64 {
	input_maze.best_paths(false).next().unwrap().score
}


pub(crate) fn part2() -> usize {
	part2_impl(input_maze())
}

fn part2_impl(input_maze: Maze) -> usize {
	let mut counted = vec![false; input_maze.spaces.len()];
	let mut best_paths = input_maze.best_paths(true);
	let mut paths = best_paths.by_ref().collect::<Vec<_>>();

	for path in &paths {
		counted[path.pos] = true;
		for step in path.prevs() { counted[step.pos] = true }
	}

	loop {
		let mut did_join = false;

		best_paths.joins.retain(|join| {
			if !counted[join.pos] { return true }
			let Some(joined) = paths.iter()
				.flat_map(|p| p.prevs())
				.find(|p| p.pos == join.pos && p.dir == join.dir)
			else { return true };
			if joined.score < join.score { return true }

			for step in join.prevs() { counted[step.pos] = true }

			let prev = join.prev.as_ref().map(|p| p.clone());
			paths.push(path::Step { pos: join.pos, dir: join.dir, score: join.score, prev });
			did_join = true;

			false
		});

		if !did_join { break }
	}

	counted.into_iter().filter(|&c| c).count()
}


mod parsing {
	use std::str::FromStr;
	use super::{Space, Maze};

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum MazeError {
		Empty,
		Stride { line: usize },
		Space { line: usize, column: usize, found: u8 },
		NoStart,
		NoEnd,
	}

	impl FromStr for Maze {
		type Err = MazeError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let mut spaces = Vec::new();
			let mut stride = None;
			let mut start = None;
			let mut end = None;
			for (l, line) in s.lines().enumerate() {
				let line_len = line.len();
				if stride.replace(line_len).is_some_and(|l| l != line_len) {
					return Err(Self::Err::Stride { line: l })
				}

				spaces.reserve(line_len);
				for (c, byte) in line.bytes().enumerate() {
					match byte {
						b'#' => spaces.push(Space::Wall),
						b'.' => spaces.push(Space::Open),
						b'S' => { start = Some(spaces.len()); spaces.push(Space::Open) }
						b'E' => { end = Some(spaces.len()); spaces.push(Space::Open) }
						_ => return Err(Self::Err::Space { line: l, column: c, found: byte })
					}
				}
			}
			let stride = stride.ok_or(Self::Err::Empty)?;
			let start = start.ok_or(Self::Err::NoStart)?;
			let end = end.ok_or(Self::Err::NoEnd)?;

			Ok(Maze { spaces: spaces.into(), stride, start, end })
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		###############
		#.......#....E#
		#.#.###.#.###.#
		#.....#.#...#.#
		#.###.#####.#.#
		#.#.#.......#.#
		#.#.#####.###.#
		#...........#.#
		###.#.#####.#.#
		#...#.....#.#.#
		#.#.#.###.#.#.#
		#.....#...#.#.#
		#.###.#.#.#.#.#
		#S..#.....#...#
		###############
	"};
	const SECOND_INPUT: &str = indoc::indoc! {"
		#################
		#...#...#...#..E#
		#.#.#.#.#.#.#.#.#
		#.#.#.#...#...#.#
		#.#.#.#.###.#.#.#
		#...#.#.#.....#.#
		#.#.#.#.#.#####.#
		#.#...#.#.#.....#
		#.#.#####.#.###.#
		#.#.#.......#...#
		#.#.###.#####.###
		#.#.#...#.....#.#
		#.#.#.#####.###.#
		#.#.#.........#.#
		#.#.#.#########.#
		#S#.............#
		#################
	"};
	assert_eq!(part1_impl(INPUT.parse::<Maze>().unwrap()), 7036);
	assert_eq!(part1_impl(SECOND_INPUT.parse::<Maze>().unwrap()), 11048);
	assert_eq!(part1(), 85420);
	assert_eq!(part2_impl(INPUT.parse::<Maze>().unwrap()), 45);
	assert_eq!(part2_impl(SECOND_INPUT.parse::<Maze>().unwrap()), 64);
	assert_eq!(part2(), 492);
}
