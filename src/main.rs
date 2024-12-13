// Copyright (c) 2024 Bastiaan Marinus van de Weerd

mod util;
util::mod_days![01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13];

fn main() {
	println!("Day 1; part 1: {}, part 2: {}", day01::part1(), day01::part2());
	println!("Day 2; part 1: {}, part 2: {}", day02::part1(), day02::part2());
	println!("Day 3; part 1: {}, part 2: {}", day03::part1(), day03::part2());
	println!("Day 4; part 1: {}, part 2: {}", day04::part1(), day04::part2());
	println!("Day 5; part 1: {}, part 2: {}", day05::part1(), day05::part2());
	println!("Day 6; part 1: {}, part 2: {}", day06::part1(), day06::part2());
	println!("Day 7; part 1: {}, part 2: {}", day07::part1(), day07::part2());
	println!("Day 8; part 1: {}, part 2: {}", day08::part1(), day08::part2());
	println!("Day 9; part 1: {}, part 2: {}", day09::part1(), day09::part2());
	println!("Day 10; part 1: {}, part 2: {}", day10::part1(), day10::part2());
	println!("Day 11; part 1: {}, part 2: {}", day11::part1(), day11::part2());
	println!("Day 12; part 1: {}, part 2: {}", day12::part1(), day12::part2());
	println!("Day 13; part 1: {}, part 2: {}", day13::part1(), day13::part2());
}
