//! Turns out this was a bit of a fool’s errand:
//!
//!  * it takes pretty long (almost 5 minutes at 30 ticks per second) for
//!    the christmas tree to become visible, and;
//!
//!  * the christmas tree only flashes for a single tick among otherwise
//!    homogenously noisy ticks, making it hard to spot.
//!
//! Still, this was fun.
//!
//! Run by passing the argument `day14` to the main executable, optionally
//! followed by the start tick; i.e. usage:
//!
//! ```sh
//! advent24 day14 [START_TICK?]
//! ```

use std::{collections::BTreeMap, io::{self, Write}, time::{Duration, Instant}};

use crossterm::{self as ct, cursor as ctc, event as cte};


const FRAMES_PER_SEC: usize = 30;
const SEC: Duration = Duration::from_secs(1);

const STAGE_WIDTH: usize = 101;
const STAGE_HEIGHT: usize = 103;


pub(crate) fn run(args: &mut std::env::Args) -> io::Result<()> {
	let robots_start_tick = if let Some(robots_start_tick) = args.next() {
		robots_start_tick.parse::<usize>()
			.map_err(|_| io::Error::other("invalid robot start tick"))?
	} else {
		0
	};

	let state = terminal::set_up()?;
	let res = r#loop(robots_start_tick);
	terminal::tear_down(state, res)
}


impl super::Robot {
	fn untick<const W: usize, const H: usize>(&mut self) {
		self.pos[0] = (self.pos[0] + W).checked_add_signed(-self.vel[0]).unwrap() % W;
		self.pos[1] = (self.pos[1] + H).checked_add_signed(-self.vel[1]).unwrap() % H;
	}
}

#[derive(Clone, Default)]
struct Paused {
	anim_state: u8,
}

impl Paused {
	fn draw(
		mut dest: impl Write,
		curr: Option<&Self>,
		prev: Option<&Self>,
	) -> io::Result<()> {
		ct::execute!(dest, ctc::MoveTo(0, 0))?;
		if curr.is_some() && prev.is_none() {
			println!("PAUSED    (press Q to quit)");
		} else if let Some(paused) = &curr {
			let prev_anim_state = prev.as_ref().map(|p| p.anim_state);
			ct::execute!(dest, ctc::MoveTo(6, 0))?;
			match (paused.anim_state, prev_anim_state) {
				(a, Some(pa)) if a == pa => (),
				(0, _) => print!("   "),
				(1, _) => print!(".  "),
				(2, _) => print!(".. "),
				_ => print!("..."),
			}
		} else if curr.is_none() && prev.is_some() {
			println!("                           ");
		}
		Ok(())
	}

	fn tick(&mut self, frame_idx: usize) {
		self.anim_state = ((4 * (frame_idx % FRAMES_PER_SEC)) / FRAMES_PER_SEC) as u8;
	}
}

#[derive(Clone, Default)]
struct Stage {
	flash: bool,
}

impl Stage {
	fn _draw_pos(
		mut dest: impl Write,
		[x, y]: [usize; 2],
		flash: bool,
	) -> io::Result<()> {
		ct::execute!(dest, ctc::MoveTo(x as u16, 1 + y as u16))?;
		print!("{}", if flash { ' ' } else { '.' });
		Ok(())
	}

	fn draw(&self, mut dest: impl Write, prev: Option<&Self>) -> io::Result<()> {
		let prev_flash = prev.is_some_and(|p| p.flash);
		for y in 0..STAGE_HEIGHT {
			for x in 0..STAGE_WIDTH {
				let is_flash_pos = x == STAGE_WIDTH / 2 || y == STAGE_HEIGHT / 2;
				if is_flash_pos && self.flash != prev_flash || prev.is_none() {
					Self::_draw_pos(&mut dest, [x, y], is_flash_pos && self.flash)?;
				}
			}
		}

		Ok(())
	}

	fn draw_pos(&self, mut dest: impl Write, [x, y]: [usize; 2]) -> io::Result<()> {
		let is_flash_pos = x == STAGE_WIDTH / 2 || y == STAGE_HEIGHT / 2;
		Self::_draw_pos(&mut dest, [x, y], is_flash_pos && self.flash)
	}

	fn tick(&mut self, frame_idx: usize) {
		self.flash = ((4 * (frame_idx % FRAMES_PER_SEC)) / FRAMES_PER_SEC) == 3;
	}
}

#[derive(Clone, Default)]
struct State {
	frame_idx: usize,
	run_frame_idx: usize,
	paused: Option<Paused>,
	stage: Stage,
	run_frames_per_robots_tick: usize,
	run_frames_till_next_robots_tick: usize,
	robots_ticks_elapsed: usize,
}

impl State {
	fn is_paused(&self) -> bool {
		self.paused.is_some()
	}

	fn pause(&mut self) {
		if self.paused.is_none() {
			self.paused = Some(Paused::default())
		}
	}

	fn play_pause(&mut self) {
		self.paused = self.paused.is_none().then(Paused::default)
	}

	fn draw_paused(&self, dest: impl Write, prev: Option<&Self>) -> io::Result<()> {
		Paused::draw(dest, self.paused.as_ref(), prev.and_then(|p| p.paused.as_ref()))
	}

	fn draw_stage(&self, dest: impl Write, prev: Option<&Self>) -> io::Result<()> {
		self.stage.draw(dest, prev.map(|p| &p.stage))
	}
	
	fn draw_robots(
		&self,
		mut dest: impl Write,
		robots: &[super::Robot],
		prev: Option<&Self>,
		prev_robot_nums: &mut  BTreeMap<[usize; 2], usize>,
	) -> io::Result<()> {
		if prev.as_ref().is_some_and(|p| p.robots_ticks_elapsed == self.robots_ticks_elapsed) {
			return Ok(())
		}

		let mut robot_nums = BTreeMap::new();
		for robot in robots {
			*robot_nums.entry(robot.pos).or_insert(0_usize) += 1
		}
		for (&[x, y], &num) in &robot_nums {
			let prev_num = prev_robot_nums.remove(&[x, y]).unwrap_or(0);
			if num != prev_num {
				ct::execute!(&mut dest, ctc::MoveTo(x as u16, 1 + y as u16))?;
				print!("{num}")
			}
		}
		for &pos in prev_robot_nums.keys() {
			self.stage.draw_pos(&mut dest, pos)?;
		}
		_ = std::mem::replace(prev_robot_nums, robot_nums);

		ct::execute!(dest, ctc::MoveTo(STAGE_WIDTH as u16 - 10, 0))?;
		print!("{:>10}", self.robots_ticks_elapsed);

		Ok(())
	}

	fn tick(
		&mut self,
		robots: &mut [super::Robot],
	) -> Self {
		let clone = self.clone();

		self.frame_idx += 1;
		if !self.is_paused() { self.run_frame_idx += 1 }

		if let Some(paused) = &mut self.paused { paused.tick(self.frame_idx); }
		self.stage.tick(self.run_frame_idx);

		if self.run_frames_till_next_robots_tick == 0 {
			self.run_frames_till_next_robots_tick = self.run_frames_per_robots_tick
		} else {
			if !self.is_paused() {
				if self.run_frames_till_next_robots_tick == 1 {
					self.tick_robots(robots);
				} else {
					self.run_frames_till_next_robots_tick -= 1;
				}
			}
		}

		clone
	}

	fn tick_robots(&mut self, robots: &mut [super::Robot]) {
		for robot in robots {
			robot.tick::<STAGE_WIDTH, STAGE_HEIGHT>();
		}	
		self.robots_ticks_elapsed += 1;
		self.run_frames_till_next_robots_tick = 0;
	}

	fn untick_robots(&mut self, robots: &mut [super::Robot]) {
		if self.robots_ticks_elapsed == 0 { return }

		for robot in robots {
			robot.untick::<STAGE_WIDTH, STAGE_HEIGHT>();
		}	
		self.robots_ticks_elapsed -= 1;
		self.run_frames_till_next_robots_tick = 0;
	}
}


fn r#loop(robots_start_tick: usize) -> io::Result<()> {
	let start = Instant::now();

	let mut robots = super::try_input_robots().collect::<Result<Vec<_>, _>>()
		.map_err(|_| io::Error::other("failed to parse input robots"))?
		.into_boxed_slice();
	let mut prev_robot_nums = BTreeMap::new();

	let mut state = State { run_frames_per_robots_tick: 2, ..State::default() };
	state.pause();
	for _ in 0..robots_start_tick { state.tick_robots(&mut robots) }
	let mut prev_state = None;

	'run_loop: loop {
		let mut stdout = io::stdout().lock();

		state.draw_paused(&mut stdout, prev_state.as_ref())?;
		state.draw_stage(&mut stdout, prev_state.as_ref())?;
		state.draw_robots(&mut stdout, &robots, prev_state.as_ref(), &mut prev_robot_nums)?;

		stdout.flush()?;

		prev_state = Some(state.tick(&mut robots));

		let next = start + SEC.mul_f64(state.frame_idx as f64 / FRAMES_PER_SEC as f64);
		for input in enqueue_input_keys(next)? {
			match input {
				Input::Pause => state.pause(),
				Input::PausePlay => state.play_pause(),
				Input::Prev => if state.is_paused() {
					state.untick_robots(&mut robots);
				}
				Input::Next => if state.is_paused() {
					state.tick_robots(&mut robots);
				}
				Input::Quit if !state.is_paused() => state.pause(),
				Input::Quit => break 'run_loop
			}
		}
	}

	Ok(())
}


enum Input {
	Pause,
	PausePlay,
	Prev,
	Next,
	Quit,
}

fn enqueue_input_keys(until: Instant) -> io::Result<impl Iterator<Item = Input>> {
	use cte::{KeyCode as C, KeyEventKind as K};

	let mut queue = Vec::new();

	loop {
		let now = Instant::now();
		if now < until && cte::poll(until - now)? {
			if let cte::Event::Key(event) = cte::read()? {
				assert!(matches!(event.kind, cte::KeyEventKind::Press));
				match (event.kind, event.code) {
					(K::Press, C::Esc) => queue.push(Input::Pause),
					(K::Press, C::Char(' ')) => queue.push(Input::PausePlay),
					(K::Press, C::Char('q')) => queue.push(Input::Quit),
					(K::Press, C::Left) => queue.push(Input::Prev),
					(K::Press, C::Right) => queue.push(Input::Next),
					_ => (),
				}
			}
		} else {
			return Ok(queue.into_iter())
		}
	}
}


mod terminal {
	use std::io;
	use crossterm::{self as ct, cursor as ctc, execute, terminal as ctt};

	pub(super) struct State {
		is_raw_mode_enabled: bool,
	}

	pub(super) fn set_up() -> io::Result<State> {
		ct::execute!(
			io::stdout(),
			ctc::Hide,
			ctc::SavePosition,
			ctt::EnterAlternateScreen,
		)?;
		let res = ctt::enable_raw_mode();
		Ok(State {
			is_raw_mode_enabled: res.is_ok(),
		})
	}

	pub(super) 
	fn tear_down(state: State, res: io::Result<()>) -> io::Result<()> {
		let ress = [
			res,
			if state.is_raw_mode_enabled {
				ctt::disable_raw_mode()
			} else {
				Ok(())
			},
			execute!(
				io::stdout(),
				ctt::LeaveAlternateScreen,
				ctc::RestorePosition,
				ctc::Show,
			),
		];
		for res in ress {
			res?;
		}
		Ok(())
	}
}
