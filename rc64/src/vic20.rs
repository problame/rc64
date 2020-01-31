mod mem;
mod registers;

use self::mem::MemoryView;
use self::mem::U14;
use self::registers::{ControlRegister1, ControlRegister2, InterruptEnabled, InterruptRegister};
use crate::color_ram::ColorRAM;
use crate::ram::RAM;
use crate::rom::ROM;
use crate::utils::R2C;
use crate::vic20::registers::Registers;
use std::collections::HashSet;
use std::convert::TryFrom;

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
    stall_cycles_remaining: usize,
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

pub struct Stallcycle;

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
            stall_cycles_remaining: 0,
        }
    }

    pub fn cycle(
        &mut self,
        cycles: u64,
        mut dbg: std::cell::RefMut<'_, super::mos6510::Debugger>,
    ) -> (Option<Stallcycle>, Option<crate::interrupt::Interrupt>) {
        assert_eq!(VISIBLE_HORIZONTAL_PX, 404);
        assert_eq!(SCREEN_WIDTH, 504);
        assert_eq!(SCREEN_WIDTH / PIXELS_PER_CYCLE, 63);
        assert_eq!(SCREEN_HEIGHT, 312);

        self.stall_cycles_remaining = self.stall_cycles_remaining.saturating_sub(1);

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

        {
            let mut screen = self.screen.borrow_mut();
            if inside_content_zone {
                // Coordinate transformation to content coordinates
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
                        let (color, ch) = self
                            .mem
                            .read(U14::try_from(0x400 + (char_row * 40 + char_col)).unwrap())
                            .into();
                        // find ch in char rom
                        let bm = self
                            .mem
                            .read_data(U14::try_from(0x1000 + (8 * (ch as usize)) + (y % 8)).unwrap());

                        let fg = Color::try_from(color).unwrap();
                        let bg = Color::try_from(self.regs.background_color[0] & 0b0000_1111).unwrap(); // text mode bg color

                        for px_pos in 0..8 {
                            let bitpos = (8 - px_pos) - 1;
                            let is_fg = bm & (1 << bitpos) != 0;
                            let color = if is_fg { fg } else { bg };
                            screen.set_px(Point((self.x - X_START) as usize + px_pos, self.y()), color);
                        }
                    }
                    _ => unimplemented!("Only support standard text mode for now"),
                }
            } else if inside_border_zone {
                for px_pos in 0..8 {
                    let point = Point((self.x - X_START) as usize + px_pos, self.y());
                    let border_color = Color::try_from(self.regs.border_color.bits()).unwrap();
                    screen.set_px(point, border_color)
                }
            } else {
                for px_pos in 0..8 {
                    let point = Point((self.x - X_START) as usize + px_pos, self.y());
                    let color = Color::White;
                    screen.set_px(point, color);
                }
            }
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

            // A Bad Line Condition is given at any arbitrary clock cycle, if at the
            // negative edge of ø0 at the beginning of the cycle RASTER >= $30 and RASTER
            // <= $f7 and the lower three bits of RASTER are equal to YSCROLL and if the
            // DEN bit was set during an arbitrary cycle of raster line $30.
            // TODO: DEN bit and YSCROLL
            if self.y() >= 0x30 && self.y() <= 0xf7 && self.y() & 0b111 == 0 {
                assert_eq!(self.stall_cycles_remaining, 0);
                self.stall_cycles_remaining = 40;
            }
        }

        let stallcycle = if self.stall_cycles_remaining != 0 { Some(Stallcycle) } else { None };
        if self.highlight_raster_beam {
            self.highlight_next_beam_position();
        }

        if self.x == X_START && (self.raster_break_all || self.raster_breakpoints.contains(&self.y())) {
            dbg.break_after_next_decode();
        }

        // deliver irq if appropriate
        let irq = if self.regs.interrupt_enabled.contains(InterruptEnabled::ERST)
            && self.regs.interrupt_register.contains(InterruptRegister::IRST)
        {
            Some(crate::interrupt::Interrupt)
        } else {
            None
        };

        (stallcycle, irq)
    }
}

impl<T> VIC20<T> {
    pub fn update_banking(&mut self, state: mem::BankingState) {
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
        for px_pos in 0..8 {
            let point = Point((self.x - X_START) as usize + px_pos, self.y());
            let color = Color::Yellow;
            self.screen.borrow_mut().set_px(point, color);
        }
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
