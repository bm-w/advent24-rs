// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[cfg_attr(test, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Wire([u8; 3]);

impl Wire {
	fn num(&self, prefix: u8) -> Option<u64> {
		(self.0[0] == prefix)
			.then(|| 10 * (self.0[1] - b'0') as u64 + (self.0[2] - b'0') as u64)
	}

	fn bits_num<'a>(wires: impl Iterator<Item = (&'a Self, &'a bool)>, prefix: u8) -> u64 {
		wires
			.filter_map(|(wire, value)| if *value { wire.num(prefix) } else { None })
			.map(|exp| 1 << exp)
			.sum()
	}
}

impl std::fmt::Display for Wire {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}{}{}", self.0[0] as char, self.0[1] as char, self.0[2] as char)
	}
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum GateKind { And, Or, Xor }

impl GateKind {
	fn eval(&self, lhs: bool, rhs: bool) -> bool {
		match self {
			Self::And => lhs && rhs,
			Self::Or => lhs || rhs,
			Self::Xor => lhs ^ rhs,
		}
	}
}

#[cfg(test)]
impl std::fmt::Display for GateKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use std::fmt::Write as _;

		let filled = match self {
			Self::And => { write!(f, "AND")?; 3 }
			Self::Or => { write!(f, "OR")?; 2 }
			Self::Xor => { write!(f, "XOR")?; 3 }
		};
		if let Some(width) = f.width() {
			for _ in 0..width.saturating_sub(filled) {
				f.write_char(f.fill())?
			}
		}
		Ok(())
	}
}

#[derive(Clone)]
struct Gate {
	ins: [Wire; 2],
	kind: GateKind,
	out: Wire,
}

#[cfg(test)]
struct DisplayGate<'a>(&'a Gate, Option<[bool; 3]>);

#[cfg(test)]
impl std::fmt::Display for DisplayGate<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use std::fmt::Write as _;

		fn fmt_wire(wire: &Wire, val: Option<bool>, f: &mut std::fmt::Formatter<'_>)
		-> std::fmt::Result {
			write!(f, "{}", wire)?;
			if let Some(val) = val { write!(f, "({})", u8::from(val))? }
			Ok(())
		}
		fmt_wire(&self.0.ins[0], self.1.map(|v| v[0]), f)?;
		if f.alternate() {
			write!(f, "\n |--{}{:-<3}--> ",
				if self.1.is_some() { "---" } else { "" },
				self.0.kind)?;
			fmt_wire(&self.0.out, self.1.map(|v| v[2]), f)?;
			f.write_char('\n')?
		} else {
			write!(f, " {} ", self.0.kind)?
		}
		fmt_wire(&self.0.ins[1], self.1.map(|v| v[1]), f)?;
		if f.alternate() {
			Ok(())
		} else {
			f.write_str(" -> ")?;
			fmt_wire(&self.0.out, self.1.map(|v| v[2]), f)
		} 
	}
}

#[cfg(test)]
impl std::fmt::Display for Gate {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		DisplayGate(self, None).fmt(f)
	}
}


fn device(s: &str) -> (Box<[(Wire, bool)]>, impl Iterator<Item = Gate> + '_) {
	let (wires, gates) = parsing::try_device(s).unwrap();
	(wires, gates.map(Result::unwrap))
}

fn input_device() -> (Box<[(Wire, bool)]>, impl Iterator<Item = Gate> + 'static) {
	device(include_str!("day24.txt"))
}


pub(crate) fn part1() -> u64 {
	part1_impl(input_device()) 
}

fn part1_impl(input_device: (Box<[(Wire, bool)]>, impl Iterator<Item = Gate>)) -> u64 {
	let (input_wires, input_gates)= input_device;
	let mut wires = input_wires.into_vec() // TODO: Why is this needed?
		.into_iter()
		.collect::<std::collections::HashMap<_, _>>();

	let mut gates = std::collections::VecDeque::from_iter(input_gates);
	while let Some(gate) = gates.pop_front() {
		match (wires.get(&gate.ins[0]), wires.get(&gate.ins[1])) {
			(Some(&lhs), Some(&rhs)) => {
				let val = gate.kind.eval(lhs, rhs);
				#[cfg(never)] eprintln!("{}", DisplayGate(&gate, Some([lhs, rhs, val])));
				_ = wires.insert(gate.out, val)
			}
			_ => gates.push_back(gate),
		}
	}

	Wire::bits_num(wires.iter(), b'z')
}


struct HalfAdder<'g> {
	xor: &'g Gate,
	and: &'g Gate,
}

impl<'g> HalfAdder<'g> {
	fn find(bit: u64, gates: &'g [Gate]) -> Result<Self, String> {
		let [mut xor, mut and] = [None, None];

		let mut gates = gates.iter()
			.filter(|g| g.ins[0].num(b'x') == Some(bit) && g.ins[1].num(b'y') == Some(bit)
				|| g.ins[0].num(b'y') == Some(bit) && g.ins[1].num(b'x') == Some(bit));
		for gate in gates.by_ref().take(2) {
			if matches!(gate.kind, GateKind::Xor) { xor = Some(gate) }
			if matches!(gate.kind, GateKind::And) { and = Some(gate) }
		}
		if gates.next().is_some() {
			return Err(format!("too many 'x{bit:02}'-'y{bit:02}' gates"));
		}

		Ok(HalfAdder {
			xor: xor.ok_or_else(|| format!("missing 'x{bit:02}'-'y{bit:02}' XOR gate"))?,
			and: and.ok_or_else(|| format!("missing 'x{bit:02}'-'y{bit:02}' AND gate"))?,
		})
	}

	#[allow(dead_code)]
	/// Returns sum and carry bits. Panics if any of the gates are invalid.
	fn exec(&self, x: bool, y: bool) -> (bool, bool) {
		assert!(matches!(self.xor.kind, GateKind::Xor));
		let xor = self.xor.kind.eval(x, y);
		assert!(matches!(self.and.kind, GateKind::And));
		(xor, self.and.kind.eval(x, y))
	}
}

struct FullAdder<'g> {
	xy_xor: &'g Gate,
	xy_and: &'g Gate,
	xyc_xor: &'g Gate,
	xyc_and: &'g Gate,
	or: &'g Gate,
}

impl<'g> FullAdder<'g> {
	fn find_xyc_xor(
		bit: u64,
		gates: &'g [Gate],
		carry: Option<&Gate>,
	) -> Result<&'g Gate, String> {
		let xyc_xor = gates.iter()
			.find(|g| g.out.num(b'z') == Some(bit))
			.ok_or_else(|| format!("missing 'z{bit:02}' carry-XOR gate"))?;

		if !matches!(xyc_xor.kind, GateKind::Xor) {
			return Err(format!("bad 'z{bit:02}' carry-XOR gate"));
		}

		if let Some(carry_gate) = carry {
			if xyc_xor.ins[0] != carry_gate.out && xyc_xor.ins[1] != carry_gate.out {
				return Err(format!("carry gate mismatch for 'z{bit:02}'"));
			}
		}

		Ok(xyc_xor)
	}

	fn find_xyc_xor_from_carry(
		bit: u64,
		gates: &'g [Gate],
		carry: &'g Gate,
	) -> Result<&'g Gate, String> {
		let xyc_xor = gates.iter()
			.find(|g| g.ins[0] == carry.out || g.ins[1] == carry.out)
			.ok_or_else(|| format!("missing 'z{bit:02}' carry-XOR gate from carry"))?;

		if !matches!(xyc_xor.kind, GateKind::Xor) {
			return Err(format!("bad 'z{bit:02}' carry-XOR gate from carry"));
		}

		Ok(xyc_xor)
	}

	fn find_xyc_and(
		bit: u64,
		gates: &'g [Gate],
		xyc_xor: &'g Gate,
	) -> Result<&'g Gate, String> {
		let xyc_and = gates.iter()
			.filter(|&g| !std::ptr::addr_eq(g as _, xyc_xor as _))
			.find(|g| g.ins == xyc_xor.ins
				|| g.ins[0] == xyc_xor.ins[1] && g.ins[1] == xyc_xor.ins[0])
			.ok_or_else(|| format!("missing 'z{bit:02}' carry-AND gate"))?;

		if !matches!(xyc_and.kind, GateKind::And) {
			return Err(format!("bad 'z{bit:02}' carry-AND gate"));
		}

		Ok(xyc_and)
	}

	fn find_or(
		bit: u64,
		gates: &'g [Gate],
		xy_and: &'g Gate,
		xyc_and: &'g Gate,
	) -> Result<&'g Gate, String> {
		let or = gates.iter()
			.find(|g| g.ins[0] == xyc_and.out && g.ins[1] == xy_and.out
				|| g.ins[0] == xy_and.out && g.ins[1] == xyc_and.out)
			.ok_or_else(|| format!("missing 'z{bit:02}' OR gate"))?;

		if !matches!(or.kind, GateKind::Or) {
			return Err(format!("bad 'z{bit:02}' OR gate"));
		}

		Ok(or)
	}

	fn find(bit: u64, gates: &'g [Gate], carry_gate: Option<&Gate>) -> Result<Self, String> {
		let xyc_xor = Self::find_xyc_xor(bit, gates, carry_gate)?;
		let xyc_and = Self::find_xyc_and(bit, gates, xyc_xor)?;
		let HalfAdder { xor: xy_xor, and: xy_and } = HalfAdder::find(bit, gates)?;
		let or = Self::find_or(bit, gates, xy_and, xyc_and)?;
		Ok(Self { xy_xor, xy_and, xyc_xor, xyc_and, or})
	}

	#[allow(dead_code)]
	/// Returns sum and carry bits. Panics if any of the gates are invalid.
	fn exec(&self, x: bool, y: bool, carry: bool) -> (bool, bool) {
		assert!(matches!(self.xy_xor.kind, GateKind::Xor));
		let xy_xor = self.xy_xor.kind.eval(x, y);
		assert!(matches!(self.xy_and.kind, GateKind::And));
		let xy_and = self.xy_and.kind.eval(x, y);
		assert!(matches!(self.xyc_xor.kind, GateKind::Xor));
		let xyc_xor = self.xyc_xor.kind.eval(xy_xor, carry);
		assert!(matches!(self.xyc_and.kind, GateKind::And));
		let xyc_and = self.xyc_and.kind.eval(xy_xor, carry);
		assert!(matches!(self.or.kind, GateKind::Or));
		(xyc_xor, self.or.kind.eval(xyc_and, xy_and))
	}

	#[allow(dead_code)]
	fn exec_seq<I: Iterator<Item = &'g Self>>(
		start: either::Either<HalfAdder, bool>,
		seq: I,
		x: u64,
		y: u64,
	) -> u64 {
		let (mut i, (val, mut carry)) = match start {
			either::Either::Left(half_adder) => (1, half_adder.exec(x & 1 == 1, y & 1 == 1)),
			either::Either::Right(carry) => (1, (false, carry))
		};
		let mut val = val as u64;
		for full_adder in seq {
			let bit = 1 << i;
			let (s, c) = full_adder.exec(x & bit == 1, y & bit == 1, carry);
			val |= (s as u64) << i;
			carry = c;
			i += 1;
		}
		val | ((carry as u64) << i)
	}
}


pub(crate) fn part2() -> String {
	part2_impl(input_device())
}

fn part2_impl((_, input_gates): (Box<[(Wire, bool)]>, impl Iterator<Item = Gate>)) -> String {
	use itertools::Itertools as _;

	let input_gates = input_gates.collect::<Box<[_]>>();

	let z_high = input_gates.iter()
		.filter_map(|gate| gate.out.num(b'z'))
		.max().unwrap();

	// The system is a ripple-carry adder with `z_high` input bits and as many
	// plus one (overflow) output bits. At the low end there’s a half adder
	// with 2 gates (XOR & AND), and the rest of the system is a sequence of
	// full adders with 5 gates each (2*XOR, 2*AND, & OR).
	assert_eq!(input_gates.len(), 2 + (z_high as usize - 1) * 5);

	let xy0_half_adder = HalfAdder::find(0, &input_gates)
		.expect("bad first half adder");

	let mut carry_gate = Some(xy0_half_adder.and);

	let mut good_gates = std::collections::HashSet::with_capacity(input_gates.len());
	good_gates.insert(xy0_half_adder.xor as *const _);
	good_gates.insert(xy0_half_adder.and as *const _);

	let mut bad_bits = Vec::new();
	for bit in 1..z_high {
		match FullAdder::find(bit, &input_gates, carry_gate) {
			Ok(full_adder) => {
				if let Some(carry_gate) = carry_gate { good_gates.insert(carry_gate as *const _); }
				good_gates.extend([
					full_adder.xy_xor as *const _,
					full_adder.xy_and as *const _,
					full_adder.xyc_xor as *const _,
					full_adder.xyc_and as *const _,
				]);
				carry_gate = Some(full_adder.or);
			}
			Err(_err) => {
				let xyc_xor = match (
					FullAdder::find_xyc_xor(bit, &input_gates, carry_gate),
					carry_gate,
				) {
					(ok @ Ok(_), _) => ok,
					(Err(_), Some(carry_gate)) =>
						FullAdder::find_xyc_xor_from_carry(bit, &input_gates, carry_gate),
					(err, _) => err,
				};

				let xyc_and = match &xyc_xor {
					Ok(xyc_xor) => FullAdder::find_xyc_and(bit, &input_gates, xyc_xor),
					Err(err) => Err(err.clone()),
				};

				let [_xy_xor, xy_and] = match HalfAdder::find(bit, &input_gates) {
					Ok(HalfAdder { xor, and }) => [Ok(xor), Ok(and)],
					Err(err) => [Err(err.clone()), Err(err)],
				};

				let or = match (&xy_and, &xyc_and) {
					(Ok(xy_and), Ok(xyc_and)) =>
						FullAdder::find_or(bit, &input_gates, xy_and, xyc_and),
					_ =>
						Err(format!("failed to find OR gate")),
				};

				#[cfg(never)] eprintln!("{_err}; found:");
				#[cfg(never)] if let Some(carry) = &carry_gate { eprintln!("  carry: {carry}") }
				#[cfg(never)] if let Ok(xy_xor) = &_xy_xor { eprintln!("  XOR: {xy_xor}") }
				#[cfg(never)] if let Ok(xy_and) = &xy_and { eprintln!("  AND: {xy_and}") }
				#[cfg(never)] if let Ok(xyc_xor) = &xyc_xor { eprintln!("  carry-XOR: {xyc_xor}") }
				#[cfg(never)] if let Ok(xyc_and) = &xyc_and { eprintln!("  carry-AND: {xyc_and}") }
				#[cfg(never)] if let Ok(or) = &or { eprintln!("  OR: {or}") }
				#[cfg(never)] eprintln!("");

				// Fixing consecutive bad bits is not supported
				if bad_bits.last().is_none_or(|&b| b + 1 != bit) { bad_bits.push(bit); }
				carry_gate = or.ok();
			}
		} 
	}

	if let Some(carry_gate) = carry_gate {
		if carry_gate.out.num(b'z') == Some(z_high) {
			good_gates.insert(carry_gate as *const _);
		}
	}

	let mut candidate_gates = input_gates.iter()
		.filter(|&g| !good_gates.contains(&(g as *const _)))
		.cloned()
		.collect::<Vec<_>>();

	let mut swaps = Vec::with_capacity(4);

	// It is assumed that each pair of swapped wires can be found within
	// a single bit’s full adder. This may not work for all inputs.
	for bit in bad_bits.into_iter().rev() {
		let mut fixed = false;

		for (lhs, rhs) in (0..candidate_gates.len()).tuple_combinations() {
			let (lhs_slice, rhs_slice) = candidate_gates.split_at_mut(rhs);
			std::mem::swap(&mut lhs_slice[lhs].out, &mut rhs_slice[0].out);

			match FullAdder::find(bit, &candidate_gates, None) {
				Err(_) => (),
				Ok(full_adder) => {
					let carry = if bit == 1 {
						xy0_half_adder.and.out
					} else if let Ok(prev_full_adder)
						= FullAdder::find(bit - 1, &input_gates, None)
					{
						prev_full_adder.or.out
					} else {
						continue
					};

					if carry != full_adder.xyc_xor.ins[0]
						&& carry != full_adder.xyc_xor.ins[1] { continue }

					if bit + 1 == z_high {
						let z_high = Wire([b'z', z_high as u8 / 10, z_high as u8 % 10]);
						if full_adder.or.out != z_high { continue }
					} else if let Ok(next_full_adder)
						= FullAdder::find(bit + 1, &input_gates, None)
					{
						if full_adder.or.out != next_full_adder.xyc_xor.ins[0]
							&& full_adder.or.out != next_full_adder.xyc_xor.ins[1] { continue }
					} else {
						continue
					}

					fixed = true;
				}
			}

			let (lhs_slice, rhs_slice) = candidate_gates.split_at_mut(rhs);
			if fixed {
				swaps.push([lhs_slice[lhs].out, rhs_slice[0].out]);
				candidate_gates.remove(rhs);
				candidate_gates.remove(lhs);
				break;
			} else {
				std::mem::swap(&mut lhs_slice[lhs].out, &mut rhs_slice[0].out) // Swap back
			}
		}

		if !fixed {
			#[cfg(test)] dbg!(swaps);
			panic!("failed to fix bad bit {bit}...");
		};
	}

	swaps.into_iter().flat_map(|arr| arr).sorted().join(",")
}


mod parsing {
	use std::str::FromStr;
	use super::{Gate, GateKind, Wire};

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum WireError {
		Len { found: usize },
		Char { offset: usize, found: char },
	}

	impl FromStr for Wire {
		type Err = WireError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			if s.len() != 3 {
				return Err(Self::Err::Len { found: s.len() })
			}

			if let Some(offset) = s.bytes()
				.position(|b| !b.is_ascii_lowercase() && !b.is_ascii_digit())
			{
				return Err(Self::Err::Char { offset, found: char::from(s.as_bytes()[offset]) })
			}

			Ok(Self([s.as_bytes()[0], s.as_bytes()[1], s.as_bytes()[2]]))
		}
	}

	impl FromStr for GateKind {
		type Err = ();
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			match s {
				"AND" => Ok(Self::And),
				"OR" => Ok(Self::Or),
				"XOR" => Ok(Self::Xor),
				_ => Err(()),
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum GateError {
		Format,
		InWire { offset: usize, source: WireError },
		Kind,
		OutWire(WireError),
	}

	impl FromStr for Gate {
		type Err = GateError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let (s, out) = s.split_once(" -> ").ok_or(Self::Err::Format)?;
			let (in0, s) = s.split_once(' ').ok_or(Self::Err::Format)?;
			let (kind, in1) = s.split_once(' ').ok_or(Self::Err::Format)?;

			let in0 = in0.parse().map_err(|source| Self::Err::InWire { offset: 0, source })?;
			let kind = kind.parse().map_err(|()| Self::Err::Kind)?;
			let in1 = in1.parse().map_err(|source| Self::Err::InWire { offset: 1, source })?;
			let out = out.parse().map_err(|source| Self::Err::OutWire(source))?;

			Ok(Self { ins: [in0, in1], kind, out })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum WiresErrorKind {
		Format,
		Wire(WireError),
		Value,
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct WiresError { line: usize, kind: WiresErrorKind }

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct GatesError { line: usize, source: GateError }

	pub(super) fn try_device(s: &str) -> Result<(
		Box<[(Wire, bool)]>,
		impl Iterator<Item = Result<Gate, GatesError>> + '_,
	),WiresError> {
		let mut lines = s.lines().enumerate();
		let wires = lines
			.by_ref()
			.take_while(|(_, line)| !line.is_empty())
			.map(|(l, line)| {
				let (wire, value) = line.split_once(": ")
					.ok_or(WiresError { line: l, kind: WiresErrorKind::Format })?;
				let wire = wire.parse()
					.map_err(|e| WiresError { line: l, kind: WiresErrorKind::Wire(e) })?;
				let value = match value {
					"0" => false,
					"1" => true,
					_ => return Err(WiresError { line: l, kind: WiresErrorKind::Value }),
				};
				Ok((wire, value))
			})
			.collect::<Result<_, WiresError>>()?;

		let gates = lines
			.map(|(l, line)| line.parse()
				.map_err(|source| GatesError { line: l, source }));

		Ok((wires, gates))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		x00: 1
		x01: 1
		x02: 1
		y00: 0
		y01: 1
		y02: 0

		x00 AND y00 -> z00
		x01 XOR y01 -> z01
		x02 OR y02 -> z02
	"};
	assert_eq!(part1_impl(device(INPUT)), 4);
	const LARGER_INPUT: &str = indoc::indoc! {"
		x00: 1
		x01: 0
		x02: 1
		x03: 1
		x04: 0
		y00: 1
		y01: 1
		y02: 1
		y03: 1
		y04: 1

		ntg XOR fgs -> mjb
		y02 OR x01 -> tnw
		kwq OR kpj -> z05
		x00 OR x03 -> fst
		tgd XOR rvg -> z01
		vdt OR tnw -> bfw
		bfw AND frj -> z10
		ffh OR nrd -> bqk
		y00 AND y03 -> djm
		y03 OR y00 -> psh
		bqk OR frj -> z08
		tnw OR fst -> frj
		gnj AND tgd -> z11
		bfw XOR mjb -> z00
		x03 OR x00 -> vdt
		gnj AND wpb -> z02
		x04 AND y00 -> kjc
		djm OR pbm -> qhw
		nrd AND vdt -> hwm
		kjc AND fst -> rvg
		y04 OR y02 -> fgs
		y01 AND x02 -> pbm
		ntg OR kjc -> kwq
		psh XOR fgs -> tgd
		qhw XOR tgd -> z09
		pbm OR djm -> kpj
		x03 XOR y03 -> ffh
		x00 XOR y04 -> ntg
		bfw OR bqk -> z06
		nrd XOR fgs -> wpb
		frj XOR qhw -> z04
		bqk OR frj -> z07
		y03 OR x01 -> nrd
		hwm AND bqk -> z03
		tgd XOR rvg -> z12
		tnw OR pbm -> gnj
	"};
	assert_eq!(part1_impl(device(LARGER_INPUT)), 0b11111101000); // 2024
	assert_eq!(part1(), 0b1101011111110111000100110001011010101101001110); // 59364044286798
	assert_eq!(part2(), "cbj,cfk,dmn,gmt,qjj,z07,z18,z35");
}
