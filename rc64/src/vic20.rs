mod mem;
mod registers;

use self::mem::MemoryView;
use self::registers::{ControlRegister1, ControlRegister2, InterruptEnabled, InterruptRegister};
use crate::color_ram::ColorRAM;
use crate::ram::RAM;
use crate::rom::ROM;
use crate::utils::R2C;
use crate::vic20::registers::Registers;
use bit_vec::BitVec;
use std::collections::HashSet;
use std::convert::TryFrom;
use std::iter;

pub use self::mem::BankingState;

use num_enum::TryFromPrimitive;

/// https://www.c64-wiki.com/wiki/Color
#[derive(Debug, Eq, PartialEq, TryFromPrimitive, Copy, Clone)]
#[repr(u8)]
pub enum Color {
    Black = 0,
    White = 1,
    Red = 2,
    Cyan = 3,
    Violet = 4,
    Green = 5,
    Blue = 6,
    Yellow = 7,
    Orange = 8,
    Brown = 9,
    LightRed = 10,
    DarkGrey1 = 11,
    Grey2 = 12,
    LightGreen = 13,
    LightBlue = 14,
    LightGrey3 = 15,
}

impl From<self::mem::U4> for Color {
    fn from(four: self::mem::U4) -> Self {
        Color::try_from(u8::from(four)).expect("U4 is always a valid color")
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Point(pub usize, pub usize);

pub trait ScreenBackend {
    fn set_px(&mut self, p: Point, col: Color);
}

pub struct VIC20<T> {
    mem: MemoryView<T>,
    screen: R2C<dyn ScreenBackend>,
    regs: Registers,
    x: isize,
    // No explicit `y: usize`, stored in VIC-registers, see functions VIC20::{y,inc_y,reset_y}
    y: usize,
    raster_breakpoints: HashSet<usize>,
    raster_break_all: bool,
    highlight_raster_beam: bool,
}

// 9.5cycles * 8px
pub const HBLANK_LEFT_PX: isize = 76;
// BORDER+CONTENT+BORDER
pub const VISIBLE_HORIZONTAL_PX: isize = 48 + 320 + 36;
// 3cycles * 8px
pub const HBLANK_RIGHT_PX: isize = 3 * 8;
// WIDTH=HBLANK_LEFT+VISIBLE+HBLANK_RIGHT
pub const SCREEN_WIDTH: usize = (HBLANK_LEFT_PX + VISIBLE_HORIZONTAL_PX + HBLANK_RIGHT_PX) as usize;

const MAX_X: isize = SCREEN_WIDTH as isize + X_START;
const X_START: isize = -(HBLANK_LEFT_PX + 48 / 2);

pub const PIXELS_PER_CYCLE: usize = 8;

pub const VISIBLE_VERTICAL_PX: usize = 284;
pub const VBLANK_TOP_PX: usize = 14;
pub const VBLANK_BTM_PX: usize = 14;
pub const SCREEN_HEIGHT: usize = VBLANK_TOP_PX + VISIBLE_VERTICAL_PX + VBLANK_BTM_PX;

impl<T: AsRef<[u8]>> VIC20<T> {
    pub fn new(
        char_rom: ROM<T>,
        ram: R2C<RAM>,
        color_ram: R2C<ColorRAM>,
        screen: R2C<dyn ScreenBackend>,
    ) -> Self {
        VIC20 {
            mem: MemoryView::new(char_rom, ram, color_ram),
            screen,
            regs: Registers::default(),
            x: X_START,
            y: 0,
            raster_breakpoints: HashSet::new(),
            raster_break_all: false,
            highlight_raster_beam: false,
        }
    }

    pub fn cycle(
        &mut self,
        cycles: u64,
        mut dbg: std::cell::RefMut<'_, super::mos6510::Debugger>,
    ) -> Option<crate::interrupt::Interrupt> {
        assert_eq!(VISIBLE_HORIZONTAL_PX, 404);
        assert_eq!(SCREEN_WIDTH, 504);
        assert_eq!(SCREEN_WIDTH / PIXELS_PER_CYCLE, 63);
        assert_eq!(SCREEN_HEIGHT, 312);

        let border_start_x = X_START + HBLANK_LEFT_PX + 1;
        let border_end_x = MAX_X - HBLANK_RIGHT_PX;

        let border_start_y = VBLANK_TOP_PX;
        let border_end_y = SCREEN_HEIGHT - VBLANK_BTM_PX;

        let (content_start_x, content_end_x) =
            if self.regs.control_register_2.contains(ControlRegister2::CSEL) {
                (24, 343)
            } else {
                (31, 334)
            };

        let (content_start_y, content_end_y) =
            if self.regs.control_register_1.contains(ControlRegister1::RSEL) {
                (51, 250)
            } else {
                (55, 246)
            };

        let inside_border_zone = self.x >= border_start_x
            && self.x < border_end_x
            && self.y() >= border_start_y
            && self.y() <= border_end_y;

        let inside_content_zone = self.x >= content_start_x
            && self.x <= content_end_x
            && self.y() >= content_start_y
            && self.y() <= content_end_y;

        assert!(!inside_content_zone || inside_border_zone);

        if inside_content_zone {
            // Coordinate transformation
            let x = (self.x - content_start_x) as usize;
            let y = self.y() - content_start_y;

            match (
                self.regs.control_register_1.contains(ControlRegister1::BMM),
                self.regs.control_register_1.contains(ControlRegister1::ECM),
                self.regs.control_register_2.contains(ControlRegister2::MCM),
            ) {
                (false, false, false) => {
                    let char_row = y / 8;
                    let char_col = x / 8;

                    // c-access
                    let vm = self.regs.memory_pointers.video_matrix_base();
                    let vc = char_row * 40 + char_col;
                    let (color, ch) = self.mem.read(vm + vc).into();

                    // g-access
                    let cb = self.regs.memory_pointers.character_generator_base();
                    let d = (ch as usize) << 3;
                    let rc = y & 0b111;
                    let bm = BitVec::from_bytes(&[self.mem.read_data(cb + d + rc)]);

                    let fg = Color::try_from(color).unwrap();
                    let bg = Color::try_from(self.regs.background_color[0] % 16).unwrap(); // text mode bg color

                    self.draw_horizontal(bm.iter().map(|is_fg| match is_fg {
                        true => fg,
                        false => bg,
                    }))
                }
                _ => unimplemented!("Only support standard text mode for now"),
            }
        } else if inside_border_zone {
            self.draw_horizontal(
                iter::repeat(Color::try_from(self.regs.border_color.bits()).unwrap()).take(8),
            )
        } else {
            self.draw_horizontal(iter::repeat(Color::White).take(8))
        }

        assert!(self.x != X_START || cycles % 63 == 0);
        assert!(
            !(self.y() == 0 && self.x == X_START) || (cycles % (63 * 312)) == 0,
            "cycles={} y={}",
            cycles,
            self.y()
        );

        self.x += PIXELS_PER_CYCLE as isize;
        if self.x >= SCREEN_WIDTH as isize + X_START {
            assert_eq!(self.x, SCREEN_WIDTH as isize + X_START);
            self.x = X_START;
            self.inc_y();
            if self.y() >= SCREEN_HEIGHT {
                assert_eq!(self.y(), SCREEN_HEIGHT);
                self.reset_y();
            }
            if self.y() == self.regs.raster_interrupt_line {
                self.regs.interrupt_register.insert(InterruptRegister::IRST); // CPU must clear it manually
            }
        }

        if self.highlight_raster_beam {
            self.highlight_next_beam_position();
        }

        if self.x == X_START && (self.raster_break_all || self.raster_breakpoints.contains(&self.y())) {
            dbg.break_after_next_decode();
        }

        // deliver irq if appropriate
        if self.regs.interrupt_enabled.contains(InterruptEnabled::ERST)
            && self.regs.interrupt_register.contains(InterruptRegister::IRST)
        {
            Some(crate::interrupt::Interrupt)
        } else {
            None
        }
    }
}

impl<T> VIC20<T> {
    pub fn update_banking(&mut self, state: mem::BankingState) {
        if self.mem.banking_state != state {
            println!("VIC banking update {:?} -> {:?}", self.mem.banking_state, state);
        }
        self.mem.banking_state = state;
    }

    pub fn get_banking(&self) -> mem::BankingState {
        self.mem.banking_state
    }

    fn y(&self) -> usize {
        let y = (self.regs.raster_counter as usize)
            | if self.regs.control_register_1.contains(ControlRegister1::RST8) { 0b1_0000_0000 } else { 0 };

        assert_eq!(self.y, y, "self.y={:#b}, y={:#b}", self.y, y);

        y
    }

    fn inc_y(&mut self) {
        let mut cur = self.y();
        assert!(cur < 1 << 9);
        cur += 1;
        self.regs.raster_counter = (cur & 0xff) as u8;
        self.regs.control_register_1.set(ControlRegister1::RST8, (cur & 0x100) != 0);

        self.y = self.y.overflowing_add(1).0;
        assert_eq!(self.y(), self.y);
    }

    fn reset_y(&mut self) {
        self.regs.raster_counter = 0;
        self.regs.control_register_1.remove(ControlRegister1::RST8);

        self.y = 0;
        assert_eq!(self.y(), self.y);
    }

    fn highlight_next_beam_position(&self) {
        self.draw_horizontal(iter::repeat(Color::Yellow).take(8))
    }

    fn draw_horizontal<I: Iterator<Item = Color>>(&self, cols: I) {
        let mut screen = self.screen.borrow_mut();
        let starting_point = Point((self.x - X_START) as usize, self.y());
        iter::successors(Some(starting_point), |p| Some(Point(p.0 + 1, p.1)))
            .zip(cols.into_iter())
            .for_each(|(point, col)| screen.set_px(point, col))
    }
}

pub trait RasterBreakpointBackend {
    fn status_dump(&self) -> String;
    fn add_raster_breakpoint(&mut self, line: usize);
    fn remove_raster_breakpoint(&mut self, line: usize);
    fn break_on_every_raster_line(&mut self, brk: bool);
    fn highlight_raster_beam(&mut self, beam: bool);
}

impl<T> RasterBreakpointBackend for VIC20<T> {
    fn status_dump(&self) -> String {
        format!("x={} regs={{{:?}}}", self.x, self.regs)
    }

    fn add_raster_breakpoint(&mut self, line: usize) {
        self.raster_breakpoints.insert(line);
    }

    fn remove_raster_breakpoint(&mut self, line: usize) {
        self.raster_breakpoints.remove(&line);
    }

    fn break_on_every_raster_line(&mut self, brk: bool) {
        self.raster_break_all = brk;
    }

    fn highlight_raster_beam(&mut self, beam: bool) {
        self.highlight_raster_beam = beam;
        if beam {
            self.highlight_next_beam_position();
        }
    }
}
