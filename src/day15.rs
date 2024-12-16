// Copyright (c) 2024 Bastiaan Marinus van de Weerd


enum Space {
	Wall,
	Open { has_box: bool }
}

struct Map {
	spaces: Box<[Space]>,
	stride: usize,
}

#[derive(Clone, Copy)]
enum Dir { Up, Down, Left, Right }

#[cfg(test)]
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
impl std::fmt::Display for Dir {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use std::fmt::Write as _;
		f.write_char(char::from(*self))
	}
}

impl Map {
	fn adj_pos(&self, pos: usize, dir: Dir) -> Option<usize> {
		let [s, l] = [self.stride, self.spaces.len()];
		match dir {
			Dir::Up if pos >= s => Some(pos - s),
			Dir::Down if pos < l - s  => Some(pos + s),
			Dir::Left if pos % s > 0 => Some(pos - 1),
			Dir::Right if pos % s < s - 1 => Some(pos + 1),
			_ => None,
		}
	}
	
	fn _clear_space(&mut self, pos: usize, dir: Dir) -> bool {
		debug_assert!(matches!(self.spaces[pos], Space::Open { has_box: true }));

		let Some(adj_pos) = self.adj_pos(pos, dir) else { return false };
		match self.spaces[adj_pos] {
			Space::Wall => false,
			Space::Open { has_box: false } => {
				self.spaces.swap(pos, adj_pos);
				true
			}
			Space::Open { has_box: true } => {
				if !self._clear_space(adj_pos, dir) { return false }
				self.spaces.swap(pos, adj_pos);
				true
			}
		}
	}

	fn clear_space(&mut self, pos: usize, dir: Dir) -> bool {
		match self.spaces[pos] {
			Space::Wall => false,
			Space::Open { has_box: false } => true,
			Space::Open { has_box: true } => self._clear_space(pos, dir)
		}
	}
}

struct Robot<I> {
	pos: usize,
	moves: I
}

struct Warehouse<I> {
	map: Map,
	robot: Robot<I>,
}


fn unwrap_warehouse<I>(warehouse: Result<Warehouse<I>, parsing::WarehouseError>)
-> Warehouse<impl Iterator<Item = Dir>>
where I: Iterator<Item = Result<Dir, parsing::WarehouseError>> {
	let Warehouse { map, robot } = warehouse.unwrap();
	let Robot { pos: robot_pos, moves: robo_moves } = robot;
	let robot = Robot { pos: robot_pos, moves: robo_moves.map(Result::unwrap) };
	Warehouse { map, robot }
}

fn input_warehouse() -> Warehouse<impl Iterator<Item = Dir>> {
	unwrap_warehouse(parsing::try_warehouse(include_str!("day15.txt")))
}


pub(crate) fn part1() -> u64 {
	part1_impl(input_warehouse())
}

fn part1_impl(input_warehouse: Warehouse<impl Iterator<Item = Dir>>) -> u64 {
	let Warehouse { mut map, mut robot } = input_warehouse;

	for move_dir in robot.moves {
		let Some(move_pos) = map.adj_pos(robot.pos, move_dir) else { continue };
		if !map.clear_space(move_pos, move_dir) { continue }
		robot.pos = move_pos;
	}

	let mut sum = 0;
	for (pos, space) in map.spaces.iter().enumerate() {
		if !matches!(space, Space::Open { has_box: true }) { continue }
		let [x, y] = [pos % map.stride, pos / map.stride];
		sum += (x + 100 * y) as u64;
	}
	sum
}


struct WideMap {
	orig: Map,
	boxes: Box<[usize]>,
}

impl From<Map> for WideMap {
	fn from(map: Map) -> Self {
		let mut boxes = Vec::new();
		for (pos, space) in map.spaces.iter().enumerate() {
			if !matches!(space, Space::Open { has_box: true }) { continue }
			boxes.push(2 * pos);
		}
		Self { orig: map, boxes: boxes.into() }
	}
}

impl WideMap {
	fn stride(&self) -> usize {
		self.orig.stride * 2
	}

	fn spaces_len(&self) -> usize {
		self.orig.spaces.len() * 2
	}

	fn adj_pos(&self, pos: usize, dir: Dir) -> Option<usize> {
		let [s, l] = [self.stride(), self.spaces_len()];
		match dir {
			Dir::Up if pos >= s => Some(pos - s),
			Dir::Down if pos < l - s  => Some(pos + s),
			Dir::Left if pos % s > 0 => Some(pos - 1),
			Dir::Right if pos % s < s - 1 => Some(pos + 1),
			_ => None,
		}
	}
	
	fn _find_box(&self, pos: usize) -> Option<usize> {
		self.boxes.iter().position(|&b| {
			let Some(br) = self.adj_pos(b, Dir::Right) else { return false };
			(b..=br).contains(&pos)
		})
	}

	fn _stage_space_clearing(
		&mut self,
		r#box: usize,
		dir: Dir,
		into: &mut Vec<(usize, usize)>, // Box index and target pos.
	) -> bool {
		let box_pos = self.boxes[r#box];
		if matches!(dir, Dir::Up | Dir::Down) {
			let Some(adj_pos_l) = self.adj_pos(box_pos, dir) else { return false };
			let Some(adj_pos_r) = self.adj_pos(box_pos + 1, dir) else { return false };
			match [&self.orig.spaces[adj_pos_l / 2], &self.orig.spaces[adj_pos_r / 2]] {
				[Space::Wall, _] => false, 
				[_, Space::Wall] => false,
				_ => {
					if let Some(adj_box_l) = self._find_box(adj_pos_l) {
						if !self._stage_space_clearing(adj_box_l, dir, into) { return false }
					}
					if let Some(adj_box_r) = self._find_box(adj_pos_r) {
						if !self._stage_space_clearing(adj_box_r, dir, into) { return false }
					}
					into.push((r#box, adj_pos_l));
					true
				}
			}
		} else {
			let box_pos = match dir { Dir::Right => box_pos + 1, _ => box_pos };
			let Some(adj_pos) = self.adj_pos(box_pos, dir) else { return false };
			match self.orig.spaces[adj_pos / 2] {
				Space::Wall => false,
				Space::Open { .. } => {
					if let Some(adj_box) = self._find_box(adj_pos) {
						if !self._stage_space_clearing(adj_box, dir, into) { return false }
					}
					into.push((r#box, match dir { Dir::Right => adj_pos - 1, _ => adj_pos }));
					true
				}
			}
		}
	}

	fn clear_space(&mut self, pos: usize, dir: Dir) -> bool {
		match self.orig.spaces[pos / 2] {
			Space::Wall => false,
			Space::Open { .. } => {
				let Some(adj_box) = self._find_box(pos) else { return true };
				let mut staged = Vec::new();
				if !self._stage_space_clearing(adj_box, dir, &mut staged) { return false }
				for (r#box, pos) in staged {
					self.boxes[r#box] = pos;
				}
				true
			}
		}
	}
}

struct WideWarehouse<I> {
	map: WideMap,
	robot: Robot<I>,
}

impl<I> From<Warehouse<I>> for WideWarehouse<I> {
	fn from(Warehouse { map, robot }: Warehouse<I>) -> Self {
		Self { map: map.into(), robot: Robot { pos: 2 * robot.pos, ..robot } }
	}
}

#[cfg(test)]
struct DisplayWideWarehouse<'a> {
	map: &'a WideMap,
	robot_pos: usize,
}

#[cfg(test)]
impl std::fmt::Display for DisplayWideWarehouse<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use std::fmt::Write as _;

		let map_stride = self.map.stride();
		let height = self.map.spaces_len() / map_stride;
		for y in 0..height {
			let mut x = 0;
			while x < map_stride {
				let pos = map_stride * y + x;
				match &self.map.orig.spaces[pos / 2] {
					Space::Wall => f.write_char('#')?,
					Space::Open { .. } => {
						if pos == self.robot_pos {
							f.write_char('@')?;
						} else if let Some(_) = self.map._find_box(pos) {
							f.write_str("[]")?;
							x += 1;
						} else {
							f.write_char('.')?;
						}
					}
				}
				x += 1;
			}
			if y + 1 != height { f.write_char('\n')? }
		}
		Ok(())
	}
}

#[cfg(test)]
impl<I> std::fmt::Display for WideWarehouse<I> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		DisplayWideWarehouse { map: &self.map, robot_pos: self.robot.pos }.fmt(f)
	}
}


pub(crate) fn part2() -> u64 {
	part2_impl(input_warehouse())
}

fn part2_impl(input_warehouse: Warehouse<impl Iterator<Item = Dir>>) -> u64 {
	let WideWarehouse { mut map, mut robot } = WideWarehouse::from(input_warehouse);

	for move_dir in robot.moves {
		let Some(move_pos) = map.adj_pos(robot.pos, move_dir) else { continue };
		if !map.clear_space(move_pos, move_dir) { continue }
		robot.pos = move_pos;
	}

	let mut sum = 0;
	let map_stride = map.stride();
	for box_pos in &map.boxes {
		let [x, y] = [box_pos % map_stride, box_pos / map_stride];
		sum += (x + 100 * y) as u64;
	}
	sum
}


mod parsing {
	use std::str::FromStr;
	use super::{Dir, Map, Robot, Space, Warehouse};

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct DirError(u8);

	impl TryFrom<u8> for Dir {
		type Error = DirError;
		fn try_from(byte: u8) -> Result<Self, Self::Error> {
			match byte {
				b'^' => Ok(Dir::Up),
				b'v' => Ok(Dir::Down),
				b'<' => Ok(Dir::Left),
				b'>' => Ok(Dir::Right),
				_ => Err(DirError(byte))
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum WarehouseError {
		Format,
		Empty,
		Stride { line: usize },
		Space { line: usize, column: usize, found: u8 },
		DuplicateRobot { line: usize, column: usize },
		NoRobot,
		RobotMove { line: usize, column: usize, source: DirError },
	}

	impl FromStr for Warehouse<std::iter::Empty<Dir>> {
		type Err = WarehouseError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let mut stride = None;
			let mut spaces = Vec::new();
			let mut robot_pos = None;
			for (l, line) in s.lines().enumerate() {
				let line_len = line.len();
				if *stride.insert(line_len) != line_len {
					return Err(Self::Err::Stride { line: l })
				}

				spaces.reserve(line_len);
				for (c, byte) in line.bytes().enumerate() {
					match byte {
						b'#' => spaces.push(Space::Wall),
						b'.' => spaces.push(Space::Open { has_box: false }),
						b'O' => spaces.push(Space::Open { has_box: true }),
						b'@' => {
							if robot_pos.replace(spaces.len()).is_some() {
								return Err(Self::Err::DuplicateRobot { line: l, column: c});
							}
							spaces.push(Space::Open { has_box: false });
						}
						_ => return Err(Self::Err::Space { line: l, column: c, found: byte })
					}
				}
			}
			let stride = stride.ok_or(Self::Err::Empty)?;
			let robot_pos = robot_pos.ok_or(Self::Err::NoRobot)?;

			Ok(Warehouse {
				map: Map { spaces: spaces.into(), stride},
				robot: Robot { pos: robot_pos, moves: std::iter::empty() },
			})
		}
	}

	pub(super) fn try_warehouse(s: &str)
	-> Result<Warehouse<impl Iterator<Item = Result<Dir, WarehouseError>> + '_>, WarehouseError> {
		let (warehouse, robot_moves) = s.split_once("\n\n").ok_or(WarehouseError::Format)?;
		let Warehouse { map, robot: Robot { pos: robot_pos, .. } } = warehouse.parse()?;

		let robot_moves = robot_moves.lines()
			.enumerate()
			.flat_map(|(l, line)| line.bytes()
				.enumerate()
				.map(move |(c, byte)| byte.try_into()
					.map_err(|e| WarehouseError::RobotMove { line: l, column: c, source: e })));

		Ok(Warehouse { map, robot: Robot { pos: robot_pos, moves: robot_moves } })
	}
}


#[test]
fn tests() {
	const SMALLER_INPUT: &str = indoc::indoc! {"
		########
		#..O.O.#
		##@.O..#
		#...O..#
		#.#.O..#
		#...O..#
		#......#
		########

		<^^>>>vv<v>>v<<
	"};
	const INPUT: &str = indoc::indoc! {"
		##########
		#..O..O.O#
		#......O.#
		#.OO..O.O#
		#..O@..O.#
		#O#..O...#
		#O..O..O.#
		#.OO.O.OO#
		#....O...#
		##########

		<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
		vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
		><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
		<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
		^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
		^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
		>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
		<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
		^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
		v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
	"};
	assert_eq!(part1_impl(unwrap_warehouse(parsing::try_warehouse(SMALLER_INPUT))), 2028);
	assert_eq!(part1_impl(unwrap_warehouse(parsing::try_warehouse(INPUT))), 10092);
	assert_eq!(part1(), 1442192);
	assert_eq!(part2_impl(unwrap_warehouse(parsing::try_warehouse(INPUT))), 9021);
	assert_eq!(part2(), 1448458);
}
