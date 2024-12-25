// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Computer([u8; 2]);

impl std::fmt::Display for Computer {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}{}", self.0[0] as char, self.0[1] as char)
	}
}

struct Connection([Computer; 2]);


fn input_connections() -> impl Iterator<Item = Connection> {
	parsing::try_connections(include_str!("day23.txt")).map(Result::unwrap)
}


pub(crate) fn part1() -> usize {
	part1_impl(input_connections())
}

fn part1_impl(input_connections: impl Iterator<Item = Connection>) -> usize {
	use itertools::Itertools as _;
	let connections = input_connections
		.flat_map(|Connection([a, b])| [(a, b), (b, a)])
		.into_grouping_map()
		.collect::<std::collections::HashSet<_>>();

	let mut sets = std::collections::HashSet::new();

	for (a, b) in &connections {
		for b in b {
			let Some(c) = connections.get(b) else { continue }; 
			for c in c {
				let Some(c_a) = connections.get(c) else { continue }; 
				if !c_a.contains(a) { continue }
				let mut abc = [a, b, c];
				abc.sort();
				sets.insert(abc);
			}
		}
	}

	sets.into_iter()
		.filter(|[a, b, c]| a.0[0] == b't' || b.0[0] == b't' || c.0[0] == b't')
		.count()
}


pub(crate) fn part2() -> String {
	part2_impl(input_connections())
}

// This works but it runs pretty slow… after completing the puzzle I foundg
// others talking the more efficient about Bron-Kerbosch, implemented below.
#[allow(dead_code)]
fn part2_naive(input_connections: impl Iterator<Item = Connection>) -> String {
	use itertools::Itertools as _;

	let connections = input_connections
		.flat_map(|Connection([a, b])| [(a, b), (b, a)])
		.into_grouping_map()
		.collect::<std::collections::HashSet<_>>();

	let mut sets = std::collections::HashSet::new();

	#[derive(PartialEq, Eq, Hash)]
	struct Set {
		computer: Computer,
		prev: Option<std::rc::Rc<Set>>,
	}

	impl Set {
		fn iter_rev(&self) -> impl Iterator<Item = &Set> {
			std::iter::successors(Some(self), |set| set.prev.as_deref())
		}

		fn with_inserted(self: std::rc::Rc<Self>, computer: Computer) -> std::rc::Rc<Self> {
			if self.computer < computer {
				std::rc::Rc::new(Set { computer, prev: Some(self) })
			} else {
				let prev = self.prev.as_ref().map(|p| p.clone().with_inserted(computer))
					.unwrap_or_else(|| std::rc::Rc::new(Set { computer, prev: None }));
				std::rc::Rc::new(Set { computer: self.computer, prev: Some(prev) })
			}
		}
	}

	#[cfg(test)]
	impl std::fmt::Display for Set {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			for (i, node) in self.iter_rev().enumerate() {
				if i > 0 { f.write_str("->")? }
				write!(f, "{}", node.computer)?
			}
			Ok(())
		}
	}

	let mut largest_set: Option<std::rc::Rc<Set>> = None;

	for (&a, b) in &connections {

		let prev_a = std::rc::Rc::new(Set { computer: a, prev: None });
		let mut queue = std::collections::VecDeque::from_iter(
			b.iter().map(|&b| prev_a.clone().with_inserted(b) ));
		
		while let Some(set) = queue.pop_front() {
			if !sets.insert(set.clone()) { continue }

			if let Some(largest_set) = &mut largest_set {
				if set.iter_rev().count() > largest_set.iter_rev().count() {
					*largest_set = set.clone()
				}
			} else {
				largest_set = Some(set.clone())
			}

			'to: for &c in &connections[&set.computer] {
				for prev in set.iter_rev() {
					if prev.computer == c { continue 'to }
					if !connections[&prev.computer].contains(&c) { continue 'to }
				}
				queue.push_back(set.clone().with_inserted(c));
			}
		}
	}

	let Some(largest_set) = largest_set else { panic!("no sets") };
	let password = largest_set.iter_rev().map(|p| p.computer.to_string()).sorted().join(",");
	password
}

fn part2_impl(input_connections: impl Iterator<Item = Connection>) -> String {
	use {std::collections::{HashMap, HashSet}, itertools::Itertools as _};

	let connections = input_connections
		.flat_map(|Connection([a, b])| [(a, b), (b, a)])
		.into_grouping_map()
		.collect::<std::collections::HashSet<_>>();

	fn bron_kerbosch(
		connections: &HashMap<Computer, HashSet<Computer>>,
		set: &mut HashSet<Computer>,
		mut candidates: HashSet<Computer>,
		mut visited: HashSet<Computer>,
	) -> Option<HashSet<Computer>> {
		if candidates.is_empty() && visited.is_empty() {
			return Some(set.clone())
		}

		let pivot_computer = *candidates.union(&visited)
			.max_by_key(|&c| connections[c].len())
			.unwrap();
		let computers = candidates.iter()
			.filter(|&c| !connections[&pivot_computer].contains(c))
			.copied()
			.collect::<Vec<_>>();

		let mut largest_set: Option<HashSet<Computer>> = None;
		for computer in computers {
			set.insert(computer);
			if let Some(found_set) = bron_kerbosch(
				connections,
				set,
				candidates.intersection(&connections[&computer]).copied().collect(),
				visited.intersection(&connections[&computer]).copied().collect(),
			) {
				if largest_set.as_ref().is_none_or(|s| found_set.len() > s.len()) {
					largest_set = Some(found_set);
				};
			}
			set.remove(&computer);

			candidates.remove(&computer);
			visited.insert(computer);
		}

		largest_set
	}

	let largest_set = bron_kerbosch(
		&connections,
		&mut HashSet::new(),
		connections.keys().copied().collect(),
		HashSet::new()
	);

	largest_set.unwrap()
		.into_iter()
		.map(|c| c.to_string())
		.sorted()
		.join(",")
}


mod parsing {
	use std::str::FromStr;
	use super::{Computer, Connection};

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum ComputerError {
		Len { found: usize },
		Char { offset: usize, found: char },
	}

	impl FromStr for Computer {
		type Err = ComputerError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			if s.len() != 2 {
				return Err(ComputerError::Len { found: s.len() });
			}

			if let Some(offset) = s.bytes().position(|b| !b.is_ascii_lowercase()) {
				return Err(ComputerError::Char { offset, found: char::from(s.as_bytes()[offset]) });
			}

			Ok(Computer([s.as_bytes()[0], s.as_bytes()[1]]))
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum ConnectionError {
		Format,
		Computer { offset: usize, source: ComputerError },
	}

	impl FromStr for Connection {
		type Err = ConnectionError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let (l, r) = s.split_once('-').ok_or(ConnectionError::Format)?;
			let l = l.parse().map_err(|e| ConnectionError::Computer { offset: 0, source: e })?;
			let r = r.parse().map_err(|e| ConnectionError::Computer { offset: 0, source: e })?;
			Ok(Connection([l, r]))
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct ConnectionsError { line: usize, source: ConnectionError }

	pub(super) fn try_connections(s: &str)
	-> impl Iterator<Item = Result<Connection, ConnectionsError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| line.parse()
				.map_err(|e| ConnectionsError { line: l, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		kh-tc
		qp-kh
		de-cg
		ka-co
		yn-aq
		qp-ub
		cg-tb
		vc-aq
		tb-ka
		wh-tc
		yn-cg
		kh-ub
		ta-co
		de-co
		tc-td
		tb-wq
		wh-td
		ta-ka
		td-qp
		aq-cg
		wq-ub
		ub-vc
		de-ta
		wq-aq
		wq-vc
		wh-yn
		ka-de
		kh-ta
		co-tc
		wh-qp
		tb-vc
		td-yn
	"};
	assert_eq!(part1_impl(parsing::try_connections(INPUT).map(Result::unwrap)), 7);
	assert_eq!(part1(), 1062);
	assert_eq!(part2_impl(parsing::try_connections(INPUT).map(Result::unwrap)), "co,de,ka,ta");
	assert_eq!(part2(), "bz,cs,fx,ms,oz,po,sy,uh,uv,vw,xu,zj,zm");
}
