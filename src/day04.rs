// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Puzzle {
	letters: Box<[u8]>,
	stride: usize,
}

impl Puzzle {
	fn dir_strides(&self) -> impl Iterator<Item = isize> {
		let s = self.stride as isize;
		[-s - 1, -s, -s + 1, -1, 1, s - 1, s, s + 1].into_iter()
	}

	fn adj_pos(&self, pos: usize, dir_stride: isize) -> Option<usize> {
		let s = self.stride as isize;
		let x = pos % self.stride;
		let can_left = x > 0;
		let can_right = x < self.stride - 1;
		let y = pos / self.stride;
		let can_up = y > 0;
		let can_down = y < self.letters.len() / self.stride - 1;
		let valid = match dir_stride {
			up_left if up_left == -s - 1 => can_up && can_left,
			up if up == -s => can_up,
			up_right if up_right == -s + 1 => can_up && can_right,
			-1 => can_left,
			1 => can_right,
			down_left if down_left == s - 1 => can_down && can_left,
			down if down == s => can_down,
			down_right if down_right == s + 1 => can_down && can_right,
			_ => panic!("invalid `dir_stride`"),
		};
		valid.then(|| pos.checked_add_signed(dir_stride).unwrap())
	}

	fn is_mas_unchecked(&self, pos: usize, dir_stride: isize) -> bool {
		let end = match self.letters[pos] {
			b'M' => b'S',
			b'S' => b'M',
			_ => return false,
		};

		self.letters[(pos as isize + dir_stride) as usize] == b'A'
			&& self.letters[(pos as isize + 2 * dir_stride) as usize] == end
	}
}


fn input_puzzle() -> Puzzle {
	include_str!("day04.txt").parse().unwrap()
}


pub(crate) fn part1() -> usize {
	part1_impl(input_puzzle())
}

fn part1_impl(input_puzzle: Puzzle) -> usize {
	let mut count = 0;
	for (orig_pos, &letter) in input_puzzle.letters.iter().enumerate() {
		if letter != b'X' { continue }
		'dir: for dir_stride in input_puzzle.dir_strides() {
			let mut pos = orig_pos;
			for &letter in b"MAS" {
				pos = match input_puzzle.adj_pos(pos, dir_stride) {
					Some(adj_pos) if input_puzzle.letters[adj_pos] == letter => adj_pos,
					_ => continue 'dir
				}
			}
			count += 1
		}
	}
	count
}


pub(crate) fn part2() -> usize {
	part2_impl(input_puzzle())
}

fn part2_impl(input_puzzle: Puzzle) -> usize {
	let s = input_puzzle.stride;
	if s < 3 || input_puzzle.letters.len() / s < 3 {
		panic!("puzzle too small");
	}

	let poss = (0..s - 2)
		.flat_map(|x| (0..input_puzzle.letters.len() / s - 2)
			.map(move |y| s * y + x));

	let mut count = 0;
	for pos in poss {
		if !input_puzzle.is_mas_unchecked(pos, s as isize + 1) { continue }
		if !input_puzzle.is_mas_unchecked(pos + 2 * s, -(s as isize) + 1) { continue }
		count += 1;
	}
	count
}


mod parsing {
	use std::str::FromStr;
	use super::Puzzle;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum PuzzleError {
		Empty,
		Format { line: usize },
	}

	impl FromStr for Puzzle {
		type Err = PuzzleError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let mut stride = None;
			let mut letters = Vec::new();
			for (l, line) in s.lines().enumerate() {
				let len = line.len();
				if *stride.get_or_insert(len) != len {
					return Err(PuzzleError::Format { line: l })
				}

				letters.extend(line.as_bytes());
			}

			let stride = stride.ok_or(PuzzleError::Empty)?;

			Ok(Puzzle { letters: letters.into_boxed_slice(), stride })
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		MMMSXXMASM
		MSAMXMSMSA
		AMXSXMAAMM
		MSAMASMSMX
		XMASAMXAMM
		XXAMMXXAMA
		SMSMSASXSS
		SAXAMASAAA
		MAMMMXMMMM
		MXMXAXMASX
	"};
	assert_eq!(part1_impl(INPUT.parse().unwrap()), 18);
	assert_eq!(part1(), 2517);
	assert_eq!(part2_impl(INPUT.parse().unwrap()), 9);
	assert_eq!(part2(), 1960);
}
