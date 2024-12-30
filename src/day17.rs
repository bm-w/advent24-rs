// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Computer {
	registers: [u64; 3],
	program: Box<[u64]>,
}

// #[cfg_attr(test, derive(Debug))]
#[derive(Debug)]
enum Instr {
	Adv = 0,
	Bxl = 1,
	Bst = 2,
	Jnz = 3,
	Bxc = 4,
	Out = 5,
	Bdv = 6,
	Cdv = 7,
}

impl TryFrom<u64> for Instr {
	type Error = ();
	fn try_from(value: u64) -> Result<Self, Self::Error> {
		match value {
			0 => Ok(Self::Adv),
			1 => Ok(Self::Bxl),
			2 => Ok(Self::Bst),
			3 => Ok(Self::Jnz),
			4 => Ok(Self::Bxc),
			5 => Ok(Self::Out),
			6 => Ok(Self::Bdv),
			7 => Ok(Self::Cdv),
			_ => Err(()),
		}
	}
}

impl Instr {
	fn combo(operand: u64, registers: &[u64; 3]) -> Result<u64, ()> {
		match operand {
			0..=3 => Ok(operand),
			4 => Ok(registers[0]),
			5 => Ok(registers[1]),
			6 => Ok(registers[2]),
			7 => panic!("reserved"),
			_ => Err(()),
		}
	}

	fn execute(&self, computer: &mut Computer, instr_ptr: &mut usize) -> Result<Option<u64>, ()> {
		let operand = computer.program[*instr_ptr + 1];
		let mut out = None;
		match self {
			Self::Adv => {
				let a = computer.registers[0];
				computer.registers[0]
					= a / 2u64.pow(Self::combo(operand, &computer.registers)? as u32);
			}
			Self::Bxl => {
				let b = computer.registers[1];
				computer.registers[1] = b ^ operand;
			}
			Self::Bst => {
				computer.registers[1] = Self::combo(operand, &computer.registers)? % 8;
			}
			Self::Jnz => 'jnz: {
				let a = computer.registers[0];
				if a == 0 { break 'jnz }
				*instr_ptr = operand as usize;
				return Ok(None)
			}
			Self::Bxc => {
				_ = operand; // Reads but ignores
				let b = computer.registers[1];
				let c = computer.registers[2];
				computer.registers[1] = b ^ c;
			}
			Self::Out => {
				out = Some(Self::combo(operand, &computer.registers)? % 8);
			}
			Self::Bdv => {
				let a = computer.registers[0];
				let denom = 2u64.pow(Self::combo(operand, &computer.registers)? as u32);
				computer.registers[1] = a / denom;
			}
			Self::Cdv => {
				let a = computer.registers[0];
				let denom = 2u64.pow(Self::combo(operand, &computer.registers)? as u32);
				computer.registers[2] = a / denom;
			}
		}
		*instr_ptr += 2;
		Ok(out)
	}
}

impl Computer {
	fn tick(&mut self, instr_ptr: &mut usize) -> Result<Option<u64>, ()> {
		Instr::try_from(self.program[*instr_ptr])?.execute(self, instr_ptr)
	}
}


fn input_computer() -> Computer {
	include_str!("day17.txt").parse().unwrap()
}


pub(crate) fn part1() -> String {
	part1_impl(input_computer())
}

fn part1_impl(mut input_computer: Computer) -> String {
	let mut instr_ptr = 0;
	let mut out = String::new();
	while instr_ptr < input_computer.program.len() {
		if let Some(val) = input_computer.tick(&mut instr_ptr).unwrap() {
			if !out.is_empty() { out.push(',');}
			out.push_str(&val.to_string());
		}
	}
	out
}


pub(crate) fn part2() -> u64 {
	part2_impl(input_computer())
}

fn part2_impl(input_computer: Computer) -> u64 {

	// This is not a general solution, but tailored to my specific input.
	assert_eq!(input_computer.program[..], [2, 4, 1, 3, 7, 5, 4, 1, 1, 3, 0, 3, 5, 5, 3, 0][..]);

	fn step(out: &[u64], mask: u64, check: u64) -> Option<u64> {
		if out.is_empty() { return Some(0) };
		assert!(out[0] < 8);

		let mut lowest = None;

		for low in 0..8 {
			if low & mask != check & 0b111 { continue }

			let b = low ^ 3;
			let c = (out[0] ^ 3) ^ b;

			let new_mask = 0b111 << b;
			let new_check = c << b;

			if low & (new_mask & 0b111) != new_check & 0b111 { continue }

			if check & (mask & new_mask) != new_check & (mask & new_mask) { continue }

			if let Some(high) = step(&out[1..], (mask | new_mask) >> 3, (check | new_check) >> 3) {
				if high == 0 && out.len() > 1 {
					continue;
				}

				let a = (high << 3) | low;
				if lowest.map_or(true, |lowest| a < lowest) {
					lowest = Some(a);
				}
			}
		}

		lowest
	}

	step(&input_computer.program, 0, 0).unwrap()
}


// I originally started out with a “naive” approach targeting a general
// solution, which works for the example input. However, it required far more
// complexity for the real input, and after checking online I gathered that
// a general solution is impractical or even impossible.
mod part2_naive {
	use super::{Computer, Instr};

	#[cfg_attr(test, derive(Debug))]
	#[derive(Default, Clone)]
	enum MaybeValue {
		#[default]
		Unknown,
		Range(std::ops::Range<u64>),
		Known(u64),
	}

	impl MaybeValue {
		fn is_known_and(&self, f: impl FnOnce(u64) -> bool) -> bool {
			match self {
				Self::Known(val) => f(*val),
				_ => false,
			}
		}

		fn combo(operand: u64, registers: &[Self; 3]) -> Result<Self, ()> {
			match operand {
				0..=3 => Ok(Self::Known(operand)),
				4 => Ok(registers[0].clone()),
				5 => Ok(registers[1].clone()),
				6 => Ok(registers[2].clone()),
				7 => panic!("reserved"),
				_ => Err(()),
			}
		}

		fn inv_int_div(&self, rhs: u64) -> Self {
			match self {
				Self::Unknown => Self::Unknown,
				Self::Range(range) => Self::Range(range.start * rhs..range.end * rhs),
				Self::Known(val) => Self::Range(val * rhs..(val + 1) * rhs),
			}
		}

		fn lowest(&self) -> Option<u64> {
			match self {
				Self::Known(val) => Some(*val),
				Self::Range(range) => Some(range.start),
				Self::Unknown => None,
			}
		}
	}

	#[cfg_attr(test, derive(Debug))]
	struct MaybeTick {
		registers: [MaybeValue; 3],
		instr_ptr: usize,
		outputs: usize,
		_next: Option<std::rc::Rc<MaybeTick>>,
	}

	impl MaybeTick {
		fn prevs(
			self: std::rc::Rc<Self>,
			program: &[u64],
			into: &mut Vec<MaybeTick>,
		) -> Result<(), ()> {
			use MaybeValue as Val;

			into.extend(program.iter().enumerate().rev().filter_map(|(i, &instr)| {
				if Instr::Jnz as u64 != instr { return None }
				if self.registers[0].is_known_and(|a| a == 0) { return None }
				if i + 1 >= program.len() { return None }
				if program[i + 1] != self.instr_ptr as u64 { return None }
				Some(MaybeTick {
					registers: self.registers.clone(),
					instr_ptr: i,
					outputs: self.outputs,
					_next: Some(self.clone()),
				})
			}));

			if self.instr_ptr < 2 { return Ok(()) }

			let instr_ptr = self.instr_ptr - 2;
			if instr_ptr + 1 >= program.len() { return Err(()) }

			let mut prev = MaybeTick {
				registers: self.registers.clone(),
				instr_ptr,
				outputs: self.outputs,
				_next: Some(self.clone())
			};

			let operand = program[instr_ptr + 1];
			let combo = Val::combo(operand, &self.registers)?;
			match (Instr::try_from(program[instr_ptr])?, &self.registers, combo) {
				(Instr::Adv, [a, _, _], Val::Known(combo)) =>
					prev.registers[0] = a.inv_int_div(2u64.pow(combo as u32)),
				(Instr::Adv, _, _) => prev.registers[0] = Val::Unknown,

				(Instr::Bxl, [_, Val::Known(b), _], _) => prev.registers[1] = Val::Known(b ^ operand),
				(Instr::Bxl, _, _) => prev.registers[1] = Val::Unknown,

				(Instr::Bst, _, Val::Known(combo)) => prev.registers[1] = Val::Known(combo % 8),
				(Instr::Bst, _, _) => prev.registers[1] = Val::Unknown,

				(Instr::Jnz, _, _) => prev.registers[0] = Val::Known(0),

				(Instr::Bxc, [_, Val::Known(b), Val::Known(c)], _) => {
					_ = operand; // Reads but ignores
					prev.registers[1] = Val::Known(b ^ c);
				}
				(Instr::Bxc, _, _) => prev.registers[1] = Val::Unknown,

				(Instr::Out, _, Val::Known(combo))
					if self.outputs > 1
					&& program[self.outputs - 1] == combo % 8 => prev.outputs -= 1,
				(Instr::Out, _, Val::Range(range))
					if self.outputs > 0 && range.end - range.start == 8
				=> {
					prev.registers[operand as usize - 4]
						= Val::Known(range.start + program[self.outputs - 1]);
					prev.outputs -= 1;
				}
				(Instr::Out, _, _) => return Ok(()),

				(Instr::Bdv, [_, b, _], Val::Known(combo)) =>
					prev.registers[0] = b.inv_int_div(2u64.pow(combo as u32)),
				(Instr::Bdv, _, _) => prev.registers[0] = Val::Unknown,

				(Instr::Cdv, [_, _, c], Val::Known(combo)) =>
					prev.registers[0] = c.inv_int_div(2u64.pow(combo as u32)),
				(Instr::Cdv, _, _) => prev.registers[0] = Val::Unknown,
			}

			match (instr_ptr, &prev.registers[1], &prev.registers[2]) {
				(0, Val::Known(b), _) if *b != 0 => return Ok(()),
				(0, _, Val::Known(c)) if *c != 0 => return Ok(()),
				(0, _, _) => {
					prev.registers[1] = Val::Known(0);
					prev.registers[2] = Val::Known(0);
				}
				_ => (),
			}

			into.push(prev);

			Ok(())
		}
	}

	#[allow(dead_code)]
	pub(super) fn r#impl(input_computer: Computer) -> u64 {
		use std::rc::Rc;

		let mut maybe_ticks = vec![MaybeTick {
			registers: Default::default(),
			instr_ptr: input_computer.program.len(),
			outputs: input_computer.program.len(),
			_next: None,
		}];
		let mut maybe_tocks = Vec::new();

		for tick_tock in 0usize.. {
			let [from, to] = if tick_tock % 2 == 0 { [&mut maybe_ticks, &mut maybe_tocks] }
				else  { [&mut maybe_tocks, &mut maybe_ticks] };
			for maybe_tick in from.drain(..) {
				if matches!(&maybe_tick, MaybeTick { instr_ptr: 0, outputs: 0, .. }) {
					let Some(lowest) = maybe_tick.registers[0].lowest()
					else { panic!("lowest value not found") };
					return lowest;
				}

				Rc::new(maybe_tick).prevs(&input_computer.program, to).unwrap()
			}

			if to.is_empty() { panic!("value not found") }
		}

		unreachable!()
	}
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::Computer;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum ComputerError {
		Format,
		RegisterFormat { offset: usize },
		Register { offset: usize, source: ParseIntError },
		ProgramFormat,
		ProgramInstruction { offset: usize, source: ParseIntError },
	}

	fn parse_register(s: &str, offset: usize) -> Result<u64, ComputerError> {
		let s = s.strip_prefix("Register ")
			.and_then(|s| s.strip_prefix(['A', 'B', 'C'][offset]))
			.and_then(|s| s.strip_prefix(": "))
			.ok_or(ComputerError::RegisterFormat { offset })?;
		s.parse().map_err(|e| ComputerError::Register { offset, source: e })
	}

	impl FromStr for Computer {
		type Err = ComputerError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let (registers, program) = s.trim_end_matches('\n')
				.split_once("\n\n").ok_or(Self::Err::Format)?;
			let mut registers = registers.lines()
				.enumerate()
				.map(|(l, line)| parse_register(line, l));
			let registers = [
				registers.next().ok_or(Self::Err::Format)??,
				registers.next().ok_or(Self::Err::Format)??,
				registers.next().ok_or(Self::Err::Format)??,
			];
			let program = program
				.strip_prefix("Program: ")
				.ok_or(Self::Err::ProgramFormat)?
				.split(',')
				.enumerate()
				.map(|(i, instr)| instr.parse::<u64>()
					.map_err(|e| Self::Err::ProgramInstruction { offset: i, source: e }))
				.collect::<Result<_, _>>()?;
			Ok(Computer { registers, program })
		}
	}
}


#[test]
fn tests() {
	const INPUT1: &str = indoc::indoc! {"
		Register A: 729
		Register B: 0
		Register C: 0

		Program: 0,1,5,4,3,0
	"};
	const INPUT2: &str = indoc::indoc! {"
		Register A: 2024
		Register B: 0
		Register C: 0

		Program: 0,3,5,4,3,0
	"};
	assert_eq!(part1_impl(INPUT1.parse::<Computer>().unwrap()), "4,6,3,5,6,3,5,2,1,0");
	assert_eq!(part1(), "1,5,3,0,2,5,2,5,3");
	assert_eq!(part2_naive::r#impl(INPUT2.parse::<Computer>().unwrap()), 117440);
	assert_eq!(part2(), 108107566389757);
}
