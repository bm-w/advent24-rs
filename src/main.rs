// Copyright (c) 2024 Bastiaan Marinus van de Weerd

mod util;
util::mod_days![01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12,
	13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25];

fn main() -> std::io::Result<()> {
	#[cfg(feature = "day14-cli")]
	{
		let mut args = std::env::args();
		if args.by_ref().nth(1).as_deref() == Some("day14") {
			return day14::cli::run(&mut args)
		}
	}

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
	println!("Day 14; part 1: {}, part 2: {}", day14::part1(), day14::part2());
	println!("Day 15; part 1: {}, part 2: {}", day15::part1(), day15::part2());
	println!("Day 16; part 1: {}, part 2: {}", day16::part1(), day16::part2());
	println!("Day 17; part 1: {}, part 2: {}", day17::part1(), day17::part2());
	println!("Day 18; part 1: {}, part 2: {}", day18::part1(), day18::part2());
	println!("Day 19; part 1: {}, part 2: {}", day19::part1(), day19::part2());
	println!("Day 20; part 1: {}, part 2: {}", day20::part1(), day20::part2());
	println!("Day 21; part 1: {}, part 2: {}", day21::part1(), day21::part2());
	println!("Day 22; part 1: {}, part 2: {}", day22::part1(), day22::part2());
	println!("Day 23; part 1: {}, part 2: {}", day23::part1(), day23::part2());
	println!("Day 24; part 1: {}, part 2: {}", day24::part1(), day24::part2());
	println!("Day 25; part 1: {}, part 2: {}", day25::part1(), day25::part2());

	Ok(())
}
