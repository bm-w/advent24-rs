// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Computer {
	registers: [u64; 3],
	program: Box<[u64]>,
}

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


pub(crate) fn part1() -> String {
	part1_impl(include_str!("day17.txt").parse().unwrap())
}

fn part1_impl(input_computer: Computer) -> String {
	let mut computer = input_computer;
	let mut instr_ptr = 0;
	let mut out = String::new();
	while instr_ptr < computer.program.len() {
		if let Some(val) = computer.tick(&mut instr_ptr).unwrap() {
			if !out.is_empty() { out.push(',');}
			out.push_str(&val.to_string());
		}
	}
	out
}


pub(crate) fn part2() -> &'static str {
	"WIP"
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
	const INPUT: &str = indoc::indoc! {"
		Register A: 729
		Register B: 0
		Register C: 0

		Program: 0,1,5,4,3,0
	"};
	assert_eq!(part1_impl(INPUT.parse::<Computer>().unwrap()), "4,6,3,5,6,3,5,2,1,0");
	assert_eq!(part1(), "1,5,3,0,2,5,2,5,3");
}
