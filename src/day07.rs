// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Equation {
	result: u64,
	operands: Box<[u64]>,
}

trait Op: Default {
	fn execute(&self, lhs: u64, rhs: u64) -> u64;
	fn next(&self) -> Option<Self>;
}

impl Equation {
	fn is_true<O: Op>(&self) -> bool {
		match self.operands.len() {
			0 => false,
			1 => self.result == self.operands[0],
			_ => {
				let mut stack = vec![(self.operands[0], Some(O::default()))];
				loop {
					let stack_len = stack.len();
					match (stack.last_mut(), self.operands.get(stack_len)) {
						(None, _) => break,
						(Some((_, None)), _) => _ = stack.pop(),
						(Some((lhs, op @ Some(_))), Some(rhs)) => {
							let result = op.as_ref().unwrap().execute(*lhs, *rhs);
							*op = op.as_ref().unwrap().next();
							stack.push((result, Some(O::default())));
						}
						(Some((ref result, _)), None) => {
							if *result == self.result {
								return true
							}
							_ = stack.pop();
						}
					}
				}

				false
			}
		}
	}

	fn sum_true_results<O: Op>(equations: impl Iterator<Item = Self>) -> u64 {
		let mut results_sum = 0;

		for equation in equations {
			if equation.is_true::<O>() {
				results_sum += equation.result;
			}
		}

		results_sum
	}
}


fn input_equations() -> impl Iterator<Item = Equation> {
	parsing::try_equations(include_str!("day07.txt")).map(Result::unwrap)
}


pub(crate) fn part1() -> u64 {
	part1_impl(input_equations())
}

fn part1_impl(input_equations: impl Iterator<Item = Equation>) -> u64 {

	#[derive(Default)]
	enum _Op { #[default] Add, Mul }

	impl Op for _Op {
		fn execute(&self, lhs: u64, rhs: u64) -> u64 {
			match self {
				Self::Add => lhs + rhs,
				Self::Mul => lhs * rhs,
			}
		}

		fn next(&self) -> Option<Self> {
			match self {
				Self::Add => Some(Self::Mul),
				Self::Mul => None,
			}
		}
	}

	Equation::sum_true_results::<_Op>(input_equations)
}


pub(crate) fn part2() -> u64 {
	part2_impl(input_equations())
}

fn part2_impl(input_equations: impl Iterator<Item = Equation>) -> u64 {

	fn next_pow<const BASE: u64>(val: u64) -> u64 {
		if val == 0 { return 1 }
		let mut result = BASE;
		while val >= result {
			result *= BASE;
		}
		result
	}

	assert_eq!(next_pow::<10>(0), 1);
	assert_eq!(next_pow::<10>(7), 10);
	assert_eq!(next_pow::<10>(10), 100);
	assert_eq!(next_pow::<10>(13), 100);
	assert_eq!(next_pow::<10>(420), 1000);
	assert_eq!(next_pow::<10>(1337), 10_000);
	assert_eq!(next_pow::<10>(69420), 100_000);

	#[derive(Default)]
	enum _Op { #[default] Add, Mul, Concat }

	impl Op for _Op {
		fn execute(&self, lhs: u64, rhs: u64) -> u64 {
			match self {
				Self::Add => lhs + rhs,
				Self::Mul => lhs * rhs,
				Self::Concat => lhs * next_pow::<10>(rhs) + rhs,
			}
		}

		fn next(&self) -> Option<Self> {
			match self {
				Self::Add => Some(Self::Mul),
				Self::Mul => Some(Self::Concat),
				Self::Concat => None,
			}
		}
	}

	Equation::sum_true_results::<_Op>(input_equations)
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::Equation;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum EquationError {
		Format,
		Result(ParseIntError),
		Operand { offset: usize, source: ParseIntError },
	}

	impl FromStr for Equation {
		type Err = EquationError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let (result, operands) = s.split_once(": ").ok_or(EquationError::Format)?;
			let result = result.parse().map_err(EquationError::Result)?;
			let operands = operands.split(' ')
				.enumerate()
				.map(|(i, operand)| operand.parse()
					.map_err(|e| EquationError::Operand { offset: i, source: e }))
				.collect::<Result<_, _>>()?;
			Ok(Equation { result, operands })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct EquationsError {
		line: usize,
		source: EquationError,
	}

	pub(super) fn try_equations(s: &str)
	-> impl Iterator<Item = Result<Equation, EquationsError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| line.parse()
				.map_err(|e| EquationsError { line: l, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		190: 10 19
		3267: 81 40 27
		83: 17 5
		156: 15 6
		7290: 6 8 6 15
		161011: 16 10 13
		192: 17 8 14
		21037: 9 7 18 13
		292: 11 6 16 20
	"};
	assert_eq!(part1_impl(parsing::try_equations(INPUT).map(Result::unwrap)), 3749);
	assert_eq!(part1(), 465126289353);
	assert_eq!(part2_impl(parsing::try_equations(INPUT).map(Result::unwrap)), 11387);
	assert_eq!(part2(), 70597497486371);
}
