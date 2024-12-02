// Copyright (c) 2024 Bastiaan Marinus van de Weerd


macro_rules! mod_days {
	( $( $num:literal ),+ ) => {
		paste::paste! { $( mod [<day $num>]; )+ }
	}
}
pub(crate) use mod_days;
