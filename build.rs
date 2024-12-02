use std::fs;

fn main() -> std::io::Result<()> {
	for entry in fs::read_dir("src")? {
		let entry = entry?;
		let name = entry.file_name();
		let Some(name) = name.to_str() else { continue };
		let Some(number) = name.strip_prefix("day") else { continue };
		let Some(number) = number.strip_suffix(".rs") else { continue };
		let Ok(number) = number.parse::<u64>() else { continue };

		if !fs::exists(format!("src/day{number:02}.txt"))? {
			panic!("No input file for day {number}!");
		}
	}

	Ok(())
}
