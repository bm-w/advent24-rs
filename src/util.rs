// Copyright (c) 2024 Bastiaan Marinus van de Weerd


macro_rules! mod_days {
	( $( $num:literal ),+ ) => {
		paste::paste! { $( mod [<day $num>]; )+ }
	}
}
pub(crate) use mod_days;


pub(crate) fn ulog10(mut val: u64) -> u64 {
	assert_ne!(val, 0, "expected non-zero");
	let mut res = 0;
	while val >= 10 {
		val /= 10;
		res += 1;
	}
	res
}
