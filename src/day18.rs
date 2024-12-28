// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Step {
	pos: usize,
	steps: usize,

	#[cfg(test)]
	#[allow(dead_code)]
	prev: Option<std::rc::Rc<Step>>,
}

impl Step {
	fn find_path(steps: &mut [usize]) -> Option<Self> {
		use std::collections::VecDeque;
		#[cfg(test)] use std::rc::Rc;

		let mut size = 1;
		while steps.len() > size * size { size += 1; }

		let first_step = Step { pos: 0, steps: 0, #[cfg(test)] prev: None };
		let mut queue = VecDeque::from_iter([first_step]);
		while let Some(step) = queue.pop_front() {
			if steps[step.pos] <= step.steps { continue; }
			steps[step.pos] = step.steps;

			if step.pos == size * size - 1 {
				return Some(step)
			}

			let Step { pos, steps, .. } = step;
			#[cfg(test)]
			let prev = Rc::new(step);

			fn adj_poss(pos: usize, size: usize) -> impl Iterator<Item = usize> {
				let mut poss = [None; 4];
				if pos >= size { poss[0] = Some(pos - size); }
				if pos < (size - 1) * size { poss[1] = Some(pos + size); }
				let x = pos % size;
				if x > 0 { poss[2] = Some(pos - 1); }
				if x < size - 1 { poss[3] = Some(pos + 1); }
				poss.into_iter().flatten()
			}

			for adj_pos in adj_poss(pos, size) {
				queue.push_back(Step {
					pos: adj_pos,
					steps: steps + 1,
					#[cfg(test)]
					prev: Some(prev.clone())
				});
			}
		}

		None
	}
}

#[cfg(test)]
#[allow(dead_code)]
impl Step {
	fn prevs(&self) -> impl Iterator<Item = &Step> + '_ {
		let mut prev = self.prev.as_deref();
		std::iter::from_fn(move || {
			let step = prev?;
			prev = step.prev.as_deref();
			Some(step)
		})
	}

	fn poss(&self) -> impl Iterator<Item = usize> + '_ {
		std::iter::once(self.pos).chain(self.prevs().map(|s| s.pos))
	}
}

#[cfg(test)]
#[allow(dead_code)]
struct DisplayMap<'a, F>(&'a [usize], F);

#[cfg(test)]
impl<F> std::fmt::Display for DisplayMap<'_, F> where F: Fn(usize, usize) -> Option<char> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use std::fmt::Write as _;
		let mut size = 1;
		while self.0.len() > size * size { size += 1; }
		for y in 0..size {
			for x in 0..size {
				let pos = size * y + x;
				let steps = self.0[pos];
				f.write_char(if let Some(chr) = self.1(pos, steps) {
					chr
				} else { match self.0[pos] {
					usize::MAX => '.',
					_ => ':',
				} })?;
			}
			f.write_char('\n')?;
		}
		Ok(())
	}
}


fn input_coords() -> impl Iterator<Item = [usize; 2]> {
	parsing::try_coords(include_str!("day18.txt")).map(Result::unwrap)
}


pub(crate) fn part1() -> usize {
	part1_impl::<71, 1024>(input_coords())
}

fn part1_impl<const SIZE: usize, const BYTES: usize>(
	input_coords: impl Iterator<Item = [usize; 2]>,
) -> usize {
	let mut steps = vec![usize::MAX; SIZE * SIZE].into_boxed_slice();
	for [x, y] in input_coords.take(BYTES) {
		steps[SIZE * y + x] = 0;
	}

	let Some(step) = Step::find_path(&mut steps) else { panic!("path not found") };
	// #[cfg(test)] eprintln!("END:\n{}", DisplayMap(&steps, |p, s|
	// 	if p == 0 { Some('O') }
	// 	else if p == step.pos { Some('O') }
	// 	else if s == 0 { Some('#') }
	// 	else if step.poss().any(|pp| pp == p) { Some('O') }
	// 	else { None }));

	step.steps
}


pub(crate) fn part2() -> String {
	part2_impl::<71>(input_coords())
}

fn part2_impl<const SIZE: usize>(
	input_coords: impl Iterator<Item = [usize; 2]>,
) -> String {
	let input_coords = input_coords.collect::<Box<[_]>>();
	let mut steps = vec![usize::MAX; SIZE * SIZE].into_boxed_slice();

	let [mut low, mut high] = [0, input_coords.len()];
	while low + 1 < high {
		let mid = (low + high) / 2;
		for [x, y] in &input_coords[..mid] {
			steps[SIZE * y + x] = 0;
		}

		match Step::find_path(&mut steps) {
			Some(_path) => {
				low = mid;
				// #[cfg(test)] eprintln!("Reachable (mid: {mid}):\n{}", DisplayMap(&steps, |p, s|
				// 	if p == 0 { Some('O') }
				// 	else if p == _path.pos { Some('O') }
				// 	else if s == 0 { Some('#') }
				// 	else if _path.poss().any(|pp| pp == p) { Some('O') }
				// 	else { None }));
			}
			None => {
				high = mid;
				// #[cfg(test)] eprintln!("NOT reachable (mid: {mid}):\n{}", DisplayMap(&steps, |p, s|
				// 	if p == 0 { Some('0') }
				// 	else if s == 0 { Some('#') }
				// 	else { None }));
			}
		}

		steps.fill(usize::MAX);
	}

	let coord = input_coords[low];
	format!("{},{}", coord[0], coord[1])
}


mod parsing {
	use std::num::ParseIntError;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum PosError {
		Format,
		X(ParseIntError),
		Y(ParseIntError),
	}

	fn try_coord(s: &str) -> Result<[usize; 2], PosError> {
		let (x, y) = s.split_once(',').ok_or(PosError::Format)?;
		let x = x.parse().map_err(PosError::X)?;
		let y = y.parse().map_err(PosError::Y)?;
		Ok([x, y])
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct PossError { line: usize, source: PosError }

	pub(super) fn try_coords(s: &str) -> impl Iterator<Item = Result<[usize; 2], PossError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| try_coord(line)
				.map_err(|e| PossError { line: l, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		5,4
		4,2
		4,5
		3,0
		2,1
		6,3
		2,4
		1,5
		0,6
		3,3
		2,6
		5,1
		1,2
		5,5
		2,5
		6,5
		1,4
		0,4
		6,4
		1,1
		6,1
		1,0
		0,5
		1,6
		2,0
	"};
	assert_eq!(part1_impl::<7, 12>(parsing::try_coords(INPUT).map(Result::unwrap)), 22);
	assert_eq!(part1(), 282);
	assert_eq!(part2_impl::<7>(parsing::try_coords(INPUT).map(Result::unwrap)), "6,1");
	assert_eq!(part2(), "64,29");
}
