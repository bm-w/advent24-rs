// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Towel<'a>(&'a [u8]);

#[cfg(test)]
impl std::fmt::Display for Towel<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(std::str::from_utf8(self.0).map_err(|_| std::fmt::Error)?)
	}
}

struct Pattern<'a>(&'a [u8]);

#[cfg(test)]
impl std::fmt::Display for Pattern<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(std::str::from_utf8(self.0).map_err(|_| std::fmt::Error)?)
	}
}

impl Pattern<'_> {
	fn _count_arrangements<'a>(
		pattern: &'a [u8],
		towels: &[Towel<'_>],
		cache: &mut std::collections::HashMap<&'a [u8], usize>,
	) -> usize {
		if pattern.is_empty() { return 1 }
		if let Some(&count) = cache.get(pattern) { return count }

		let mut count = 0;
		for towel in towels {
			if !pattern.starts_with(towel.0) { continue }
			count += Self::_count_arrangements(&pattern[towel.0.len()..], towels, cache)
		}
		cache.insert(pattern, count);
		count
	}

	fn count_arrangements<'a>(
		&'a self,
		towels: &[Towel<'_>],
		cache: &mut std::collections::HashMap<&'a [u8], usize>,
	) -> usize {
		Self::_count_arrangements(self.0, towels, cache)
	}
}


fn towels_and_patterns(s: &str) -> (Box<[Towel<'_>]>, impl Iterator<Item = Pattern<'_>>) {
	let (towels, patterns) = parsing::try_towels_and_patterns(s).unwrap();
	(towels, patterns.map(Result::unwrap))
}

fn input_towels_and_patterns() -> (Box<[Towel<'static>]>, impl Iterator<Item = Pattern<'static>>) {
	towels_and_patterns(include_str!("day19.txt"))
}


pub(crate) fn part1() -> usize {
	part1_impl(input_towels_and_patterns())
}

/// Tried DFS first but it’s far too slow.
#[allow(dead_code)]
fn part1_dfs<'a>((input_towels, input_patterns): (
	Box<[Towel<'a>]>,
	impl Iterator<Item = Pattern<'a>>,
)) -> usize {
	let mut count = 0;

	type Level<'a> = (usize, usize, &'a [u8], bool);

	let mut cache = std::collections::BTreeSet::new();
	fn add_to_cache(cache: &mut std::collections::BTreeSet<Box<[u8]>>, stack: &[Level]) {
		let mut pattern_suffix = Vec::with_capacity(64);
		for (_, _, suffix, _) in stack {
			pattern_suffix.extend(suffix.iter().copied());
		}
		for (_, acc_len, _, _) in stack.iter().rev() {
			if !cache.insert(Box::from(&pattern_suffix[..*acc_len])) { break }
			// #[cfg(test)] eprintln!("CACHED: …{}", Pattern(&pattern_suffix[..*acc_len]));
		}
	}

	let mut stack = Vec::new();
	// let mut towels_pattern = Vec::new();
	'patterns: for pattern in input_patterns {
		stack.clear();

		// #[cfg(test)] eprintln!("PATTERN: {pattern}");

		let towel = &input_towels[0];
		stack.push((0, towel.0.len(), towel.0, false));
		add_to_cache(&mut cache, &stack);

		while let Some((towel_idx, acc_len, suffix, bad)) = stack.last_mut() {
		// loop {
		// 	#[cfg(test)] let dbg_stack_len = stack.len();
		// 	let Some((towel_idx, acc_len, suffix, bad)) = stack.last_mut() else { break };
		// 	#[cfg(test)] eprintln!("  TOWEL[{dbg_stack_len}:{}]: {}{}:{acc_len}", *towel_idx, if *acc_len > suffix.len() { "…" } else { "" }, Towel(suffix));

			let ends_with = *acc_len <= pattern.0.len() && pattern.0[..*acc_len].ends_with(suffix);
			if *bad || !ends_with {
				*acc_len -= suffix.len();
				if *towel_idx < input_towels.len() - 1 {
					*towel_idx += 1;
					let towel = &input_towels[*towel_idx];
					*acc_len += towel.0.len();
					*suffix = towel.0;
					*bad = false;
					add_to_cache(&mut cache, &stack);
				} else {
					stack.pop();
					if let Some((_, _, _, bad)) = stack.last_mut() { *bad = true }
				}
			} else if cache.contains(&pattern.0[*acc_len..]) {
				count += 1;
				// #[cfg(test)] eprintln!("    COUNTED (CACHED: …{})!", Pattern(&pattern.0[*acc_len..]));
				continue 'patterns
			} else if *acc_len < pattern.0.len() {
				let towel = &input_towels[0];
				let acc_len = *acc_len + towel.0.len();
				stack.push((0, acc_len, towel.0, false));
				add_to_cache(&mut cache, &stack);
			} else {
				count += 1;
				// #[cfg(test)] eprintln!("    COUNTED!");
				continue 'patterns
			}
		}
	}

	count
}

/// Then tried Dijkstra, which is plenty fast for part 1 but still far too slow
/// for part 2.
#[allow(dead_code)]
fn part1_dijkstra<'a>((input_towels, input_patterns): (
	Box<[Towel<'a>]>,
	impl Iterator<Item = Pattern<'a>>,
)) -> usize {
	// Dijkstra

	#[derive(PartialEq, Eq)]
	struct Step {
		towel_idx: usize,
		rem_len: usize,
	}

	impl std::cmp::PartialOrd for Step {
		fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
			Some(self.cmp(other))
		}
	}
	
	impl std::cmp::Ord for Step {
		fn cmp(&self, other: &Self) -> std::cmp::Ordering {
			other.rem_len.cmp(&self.rem_len)
				.then_with(|| self.towel_idx.cmp(&other.towel_idx))
		}
	}

	let mut count = 0;

	// #[cfg(test)] use itertools::Itertools as _;
	// #[cfg(test)] eprintln!("ALL TOWELS: {}", input_towels.iter().map(Towel::to_string).join(", "));

	'patterns: for pattern in input_patterns {

		// #[cfg(test)] eprintln!("PATTERN: {pattern}");

		let pattern_len = pattern.0.len();

		let mut heap = std::collections::BinaryHeap::new();
		for (idx, towel) in input_towels.iter().enumerate() {
			if !pattern.0.starts_with(towel.0) { continue }
			heap.push(Step { towel_idx: idx, rem_len: pattern_len - towel.0.len() });
		}

		let mut prefixes = std::collections::BTreeSet::new();

		while let Some(step) = heap.pop() {
			// #[cfg(test)] eprintln!("  TOWELS: {}{}", Pattern(&pattern.0[..pattern_len - step.rem_len]).to_string(), if step.rem_len > 0 { "…" } else { "" });

			if step.rem_len	== 0 {
				count += 1;
				// #[cfg(test)] eprintln!("    COUNTED!");
				continue 'patterns
			}

			if !prefixes.insert(&pattern.0[..pattern_len - step.rem_len]) { continue }

			// #[cfg(test)] eprintln!("  PUSHING TOWARDS …{}", Pattern(&pattern.0[pattern_len - step.rem_len..]));
			for (idx, towel) in input_towels.iter().enumerate() {
				if !pattern.0[pattern_len - step.rem_len..].starts_with(towel.0) { continue }
				if towel.0.len() > step.rem_len { continue }
				// #[cfg(test)] eprintln!("    PUSH TOWEL[{}]: {}", idx, towel);
				heap.push(Step { towel_idx: idx, rem_len: step.rem_len - towel.0.len() });
			}
		}
	}

	count
}

/// Then finally I cheated and looked at what other people did… :-/
fn part1_impl<'a>((input_towels, input_patterns): (
	Box<[Towel<'a>]>,
	impl Iterator<Item = Pattern<'a>>,
)) -> usize {
	let mut count = 0;
	let mut cache = std::collections::HashMap::new();
	let input_patterns = input_patterns.collect::<Box<[_]>>();
	for pattern in &input_patterns {
		if pattern.count_arrangements(&input_towels, &mut cache) > 0 {
			count += 1;
		}
	}
	count
}


pub(crate) fn part2() -> usize {
	part2_impl(input_towels_and_patterns())
}

fn part2_impl<'a>((input_towels, input_patterns): (
	Box<[Towel<'a>]>,
	impl Iterator<Item = Pattern<'a>>,
)) -> usize {
	let mut count = 0;
	let mut cache = std::collections::HashMap::new();
	let input_patterns = input_patterns.collect::<Box<[_]>>();
	for pattern in &input_patterns {
		count += pattern.count_arrangements(&input_towels, &mut cache);
	}
	count
}


mod parsing {
	use super::{Pattern, Towel};

	fn try_colors(s: &str) -> Result<&[u8], u8> {
		for chr in s.chars() {
			if !matches!(chr, 'w'|'u'|'b'|'r'|'g') {
				return Err(chr as u8);
			}
		}
		Ok(s.as_bytes())
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct TowelError { offset: usize, found: u8 }

	impl<'a> TryFrom<&'a str> for Towel<'a> {
		type Error = TowelError;
		fn try_from(s: &'a str) -> Result<Self, Self::Error> {
			Ok(Self(try_colors(s).map_err(|e| TowelError { offset: 0, found: e })?))
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct TowelsError { offset: usize, source: TowelError }

	fn try_towels(s: &str) -> impl Iterator<Item = Result<Towel<'_>, TowelsError>> {
		s.split(", ")
			.enumerate()
			.map(|(i, towel)| Towel::try_from(towel)
				.map_err(|e| TowelsError { offset: i, source: e }))
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct PatternError { offset: usize, found: u8 }

	impl<'a> TryFrom<&'a str> for Pattern<'a> {
		type Error = PatternError;
		fn try_from(s: &'a str) -> Result<Self, Self::Error> {
			Ok(Self(try_colors(s).map_err(|e| PatternError { offset: 0, found: e })?))
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct PatternsError { offset: usize, source: PatternError }

	fn try_patterns(s: &str) -> impl Iterator<Item = Result<Pattern<'_>, PatternsError>> {
		s.lines()
			.enumerate()
			.map(|(i, towel)| Pattern::try_from(towel)
				.map_err(|e| PatternsError { offset: i, source: e }))
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum Error { Format, Towels(TowelsError) }

	pub(super) fn try_towels_and_patterns(s: &str) -> Result<(
		Box<[Towel<'_>]>,
		impl Iterator<Item = Result<Pattern<'_>, PatternsError>>,
	), Error> {
		let (towels, patterns) = s.split_once("\n\n").ok_or(Error::Format)?;
		let towels = try_towels(towels).collect::<Result<_, _>>().map_err(Error::Towels)?;
		let patterns = try_patterns(patterns);
		Ok((towels, patterns))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		r, wr, b, g, bwu, rb, gb, br

		brwrr
		bggr
		gbbr
		rrbgbr
		ubwu
		bwurrg
		brgr
		bbrgwb
	"};
	assert_eq!(part1_impl(towels_and_patterns(INPUT)), 6);
	assert_eq!(part1(), 306);
	assert_eq!(part2_impl(towels_and_patterns(INPUT)), 16);
	assert_eq!(part2(), 604622004681855);
}
