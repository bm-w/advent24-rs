// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[derive(Clone, Copy)]
#[cfg_attr(never, derive(Debug))]
enum Dir { Up = 0, Down, Left, Right }

use Dir::*;

const A_DOOR: usize = 10;
const A_DIR: usize = 4;
const UP: usize = Up as usize;
const DOWN: usize = Down as usize;
const LEFT: usize = Left as usize;
const RIGHT: usize = Right as usize;

/// ```nocompile
/// +---+---+---+
/// | 7 | 8 | 9 |
/// +---+---+---+
/// | 4 | 5 | 6 |
/// +---+---+---+
/// | 1 | 2 | 3 |
/// +---+---+---+
///     | 0 | A |
///     +---+---+
/// ```
const DOORPAD_ADJ_POSS: [&[(usize, Dir)]; 11] = [
	&[(2, Up), (A_DOOR, Right)], // 0
	&[(2, Right), (4, Up)], // 1
	&[(0, Down), (1, Left), (3, Right), (5, Up)], // 2
	&[(2, Left), (6, Up), (A_DOOR, Down)], // 3
	&[(1, Down), (5, Right), (7, Up)], // 4
	&[(2, Down), (4, Left), (6, Right), (8, Up)], // 5
	&[(3, Down), (5, Left), (9, Up)], // 6
	&[(4, Down), (8, Right)], // 7
	&[(5, Down), (7, Left), (9, Right)], // 8
	&[(6, Down), (8, Left)], // 9
	&[(0, Left), (3, Up)], // A
];

/// ```nocompile
///     +---+---+
///     | ^ | A |
/// +---+---+---+
/// | < | v | > |
/// +---+---+---+
/// ```
const DIR_PAD_ADJ_POSS: [&[(usize, Dir)]; 5] = [
	&[(DOWN, Down), (A_DIR, Right)], // ^
	&[(UP, Up), (LEFT, Left), (RIGHT, Right)], // v
	&[(DOWN, Right)], // <
	&[(DOWN, Left), (A_DIR, Up)], // >
	&[(UP, Left), (RIGHT, Down)], // A
];


struct Code(Box<[usize]>);

impl Code {
	fn num_part(&self) -> u64 {
		self.0[..3].iter().fold(0, |acc, &d| acc * 10 + d) as u64
	}
}


#[derive(Clone)]
struct Step {
	pos: usize,
	/// `Dir` is the direction from the previous step to this one.
	prev: Option<(Dir, std::rc::Rc<Self>)>,
}

impl Step {
	fn iter_rev(&self) -> impl Iterator<Item = &Self> {
		std::iter::successors(Some(self), |s| s.prev.as_ref().map(|(_, p)| &**p))
	}

	fn poss_rev(&self) -> impl Iterator<Item = usize> + '_ {
		self.iter_rev().map(|s| s.pos)
	}

	fn dirs_rev(&self) -> impl Iterator<Item = Dir> + '_ {
		self.iter_rev().filter_map(|s| s.prev.as_ref().map(|(d, _)| *d))
	}

	fn find_extended_rc<'a>(self: std::rc::Rc<Self>, end: usize, adj_poss: &'a [&[(usize, Dir)]])
	-> impl Iterator<Item = std::rc::Rc<Self>> + 'a {
		use {std::collections::VecDeque, itertools::Itertools as _};
		let mut queue = VecDeque::from_iter([(self, 0)]);
		let mut max_steps = None;
		std::iter::from_fn(move || {
			while let Some((step, steps)) = queue.pop_front() {
				if max_steps.is_some_and(|max| steps > max) { break }
				
				if step.pos == end {
					max_steps = Some(steps);
					return Some(step)
				}

				if step.poss_rev().take(steps).skip(1).contains(&step.pos) { continue }

				for &(adj_pos, adj_dir) in adj_poss[step.pos] {
					queue.push_back((
						Self { pos: adj_pos, prev: Some((adj_dir, step.clone())) }.into(),
						steps + 1,
					));
				}
			}

			None
		})
	}

	fn find_extended<'a>(self, end: usize, adj_poss: &'a [&[(usize, Dir)]])
	-> impl Iterator<Item = Self> + 'a {
		std::rc::Rc::new(self)
			.find_extended_rc(end, adj_poss)
			.map(|s| std::rc::Rc::try_unwrap(s).ok().unwrap())
	}

	fn find_paths<'a>(start: usize, end: usize, adj_poss: &'a [&[(usize, Dir)]])
	-> impl Iterator<Item = Self> + 'a {
		Self { pos: start, prev: None }.find_extended(end, adj_poss)
	}

	fn shortest_input_len(
		start: usize,
		buttons: &[usize],
		devices: &[&[&[(usize, Dir)]]],
		cache: &mut std::collections::HashMap<(usize, usize, usize), usize>,
	) -> usize {
		let Some(&button) = buttons.first() else { return 0 };
		let cache_key = (devices.len(), start, button);


		let shortest = if let Some(&cached) = cache.get(&cache_key) {
			#[cfg(never)] eprintln!("{}:{start}-{button}\nCACHED: {cached}\n", devices.len());
			cached
		} else {
			let mut shortest = usize::MAX;
			for path in Self::find_paths(start, button, devices[0]) {
	
				let mut buttons = std::collections::VecDeque::new();
				buttons.push_front(A_DIR);
				for dir in path.dirs_rev() {
					buttons.push_front(dir as usize);
				}
	
				let subshortest = if devices.len() > 1 {
					let buttons = &*buttons.make_contiguous();
					Self::shortest_input_len(A_DIR, buttons, &devices[1..], cache)
				} else {
					buttons.len()
				};
				
				#[cfg(never)] {
					let dir_chars = &['^', 'v', '<', '>', 'A'][..];
					let chars = if devices.len() < 3 { dir_chars }
						else { &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A'][..] };
					use itertools::Itertools as _;
					eprintln!(
						"{}:{start}-{button}\n\
							PATH: {}\n\
							BUTTONS: {}\n\
							SHORTEST: {subshortest}\n",
						devices.len(),
						path.iter_rev().map(|s| chars[s.pos]).collect::<Vec<_>>()
							.into_iter().rev().join(""),
						buttons.iter().map(|&b| dir_chars[b]).collect::<String>(),
					);
				}
	
				shortest = shortest.min(subshortest);
			}
			shortest
		};

		cache.insert(cache_key, shortest);

		shortest + Self::shortest_input_len(button, &buttons[1..], devices, cache)
	}
}


fn input_codes() -> impl Iterator<Item = Code> {
	parsing::try_codes(include_str!("day21.txt")).map(Result::unwrap)
}

pub(crate) fn part1() -> u64 {
	part1and2_impl::<2>(input_codes())
}

pub(crate) fn part2() -> u64 {
	part1and2_impl::<25>(input_codes())
}

fn part1and2_impl<const N: usize>(input_codes: impl Iterator<Item = Code>) -> u64 {

	let devices = std::iter::once(&DOORPAD_ADJ_POSS[..])
		.chain(std::iter::repeat_n(&DIR_PAD_ADJ_POSS[..], N))
		.collect::<Box<[_]>>();

	let mut complexity = 0;

	let mut cache = std::collections::HashMap::new();
	for code in input_codes {
		let shortest_input_len = Step::shortest_input_len(A_DOOR, &code.0, &devices, &mut cache);
		complexity += code.num_part() * shortest_input_len as u64;
	}

	complexity
}


mod parsing {
	use std::str::FromStr;
	use super::Code;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum CodeError {
		Char { offset: usize, found: char },
	}

	impl FromStr for Code {
		type Err = CodeError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			Ok(Self(s.chars().enumerate().map(|(i, c)| match (i, c) {
				(0..3, '0'..='9') => Ok(c as usize - '0' as usize),
				(3, 'A') => Ok(10),
				_ => Err(CodeError::Char { offset: i, found: c }),
			}).collect::<Result<_, _>>()?))
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct CodesError { line: usize, source: CodeError }

	pub(super) fn try_codes(s: &str) -> impl Iterator<Item = Result<Code, CodesError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(i, l)| l.parse()
				.map_err(|e| CodesError { line: i, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		029A
		980A
		179A
		456A
		379A
	"};
	assert_eq!(part1and2_impl::<2>(parsing::try_codes(INPUT).map(Result::unwrap)), 126384);
	assert_eq!(part1(), 182844);
	assert_eq!(part2(), 226179529377982);
}
