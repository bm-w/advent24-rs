// Copyright (c) 2024 Bastiaan Marinus van de Weerd


enum Space { Wall, Track }

struct Racetrack {
	spaces: Box<[Space]>,
	stride: usize,
	start: usize,
	end: usize,
}

impl Racetrack {
	fn adj_poss(&self, pos: usize, dist: usize) -> impl Iterator<Item = usize> {
		let [s, l] = [self.stride, self.spaces.len()];
		let x = pos % s;
		let mut poss = [None; 4];
		if pos >= dist * s { poss[0] = Some(pos - dist * s) }
		if pos + dist * s < l { poss[1] = Some(pos + dist * s) }
		if x >= dist { poss[2] = Some(pos - dist) }
		if x + dist < s { poss[3] = Some(pos + dist) }
		poss.into_iter().flatten()
	}

	/// Returns tuples of `(near pos., near dist.)`.
	fn near_poss(&self, pos: usize, dist: usize) -> impl Iterator<Item = (usize, usize)> {
		let [s, l] = [self.stride, self.spaces.len()];
		let [x, y] = [pos % s, pos / s];
		let min = if y >= dist { pos - dist * s } else { x };
		let min = if x >= dist { min - dist } else { min - x };
		let max = if pos + dist * s < l { pos + dist * s } else { l - s + x };
		let max = if x + dist < s { max + dist } else { max - x + s - 1 };
		let [dx, dy] = [(max % s) - (min % s), (max - min) / s];
		(0..=dy).flat_map(move |dy| {
			let near = min + dy * s;
			(0..=dx).filter_map(move |dx| {
				let near = near + dx;
				let [nx, ny] = [near % s, near / s];
				let [dx, dy] = [nx.abs_diff(x), ny.abs_diff(y)];
				(dx + dy <= dist).then_some((near, dx + dy))
			})
		})
	}
}


fn input_racetracks() -> Racetrack {
	include_str!("day20.txt").parse().unwrap()
}

pub(crate) fn part1() -> usize {
	part1and2_impl::<2, 100>(input_racetracks())
}

pub(crate) fn part2() -> usize {
	part1and2_impl::<20, 100>(input_racetracks())
}

fn part1and2_impl<const DIST: usize, const SAVE: usize>(input_racetrack: Racetrack) -> usize {
	let mut dists = vec![usize::MAX; input_racetrack.spaces.len()].into_boxed_slice();
	let mut cheats = Vec::new();

	let [mut pos, mut dist] = [input_racetrack.start, 0];
	loop {
		dists[pos] = dist;

		for (near_pos, near_dist) in input_racetrack.near_poss(pos, DIST) {
			if near_dist < 2 { continue }
			if dist.checked_sub(dists[near_pos]).is_none() { continue }
			cheats.push((near_pos, pos, near_dist));
		}

		use itertools::Itertools as _;
		let adj_pos = input_racetrack.adj_poss(pos, 1)
			.filter(|&p| matches!(input_racetrack.spaces[p], Space::Track)
				&& dists[p] == usize::MAX)
			.exactly_one();
		let adj_pos = match adj_pos {
			Ok(adj_pos) => adj_pos,
			Err(mut err) => {
				if err.next().is_none() { break } // This must be the end
				panic!("expected simple track")
			}
		};

		pos = adj_pos;
		dist += 1;
	}
	assert_eq!(pos, input_racetrack.end);

	// dbg!(input_racetrack.start);
	// dbg!(input_racetrack.end - 2);
	// dbg!(&cheats);
	// dbg!(dists[input_racetrack.start]);
	// dbg!(dists[input_racetrack.end - 2]);
	// dbg!(dists[input_racetrack.end - 2] - dists[input_racetrack.start]);

	// let mut dbg_saves = [0; 500];

	let count = cheats.into_iter()
		.map(|(from, to, dist)| dists[to] - dists[from] - dist)
		.filter(|&s| s >= SAVE)
		// .inspect(|&s| dbg_saves[s] += 1)
		.count();

	// for (s, &c) in dbg_saves.iter().enumerate() {
	// 	if s > SAVE && c > 0 { eprintln!("{}: {}", c, s) }
	// }

	count
}


mod parsing {
	use std::str::FromStr;
	use super::{Racetrack, Space};

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum RacetrackError {
		Empty,
		Stride { line: usize },
		Space { line: usize, column: usize, found: u8 },
		NoStart,
		NoEnd,
	}

	impl FromStr for Racetrack {
		type Err = RacetrackError;
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
						b'.' => spaces.push(Space::Track),
						b'S' => { start = Some(spaces.len()); spaces.push(Space::Track) }
						b'E' => { end = Some(spaces.len()); spaces.push(Space::Track) }
						_ => return Err(Self::Err::Space { line: l, column: c, found: byte })
					}
				}
			}
			let stride = stride.ok_or(Self::Err::Empty)?;
			let start = start.ok_or(Self::Err::NoStart)?;
			let end = end.ok_or(Self::Err::NoEnd)?;

			Ok(Self { spaces: spaces.into(), stride, start, end })
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		###############
		#...#...#.....#
		#.#.#.#.#.###.#
		#S#...#.#.#...#
		#######.#.#.###
		#######.#.#...#
		#######.#.###.#
		###..E#...#...#
		###.#######.###
		#...###...#...#
		#.#####.#.###.#
		#.#...#.#.#...#
		#.#.#.#.#.#.###
		#...#...#...###
		###############
	"};
	// assert_eq!(part1and2_impl::<2, 12>(INPUT.parse().unwrap()), 8);
	// assert_eq!(part1(), 1452);
	assert_eq!(part1and2_impl::<20, 50>(INPUT.parse().unwrap()), 285);
	assert_eq!(part2(), 999556);
}
