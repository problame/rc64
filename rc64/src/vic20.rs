mod mem;

use self::mem::MemoryView;
use crate::color_ram::ColorRAM;
use crate::ram::RAM;
use crate::rom::ROM;
use crate::utils::R2C;
use std::convert::TryFrom;

pub const SCREEN_WIDTH: usize = 40 * 8;
pub const SCREEN_HEIGHT: usize = 25 * 8;

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

pub struct Point(pub usize, pub usize);

pub trait ScreenBackend {
    fn set_char_line(&mut self, p: Point, fg: Color, bg: Color, word: u8);
}

pub struct VIC20<T> {
    mem: MemoryView<T>,
    screen: R2C<dyn ScreenBackend>,
}

impl<T: AsRef<[u8]>> VIC20<T> {
    pub fn new(
        char_rom: ROM<T>,
        ram: R2C<RAM>,
        color_ram: R2C<ColorRAM>,
        screen: R2C<dyn ScreenBackend>,
    ) -> Self {
        VIC20 { mem: MemoryView::new(char_rom, ram, color_ram), screen }
    }

    pub fn cycle(&mut self) {
        // FIXME actual cycle-based impl
        // We just render the whole video matrix every cycle for now

        assert_eq!(self.mem.banking_state, self::mem::BankingState::Bank0);
        use self::mem::*;
        // TODO add border

        for y in 0..SCREEN_HEIGHT {
            let char_row = y / 8;
            for x in (0..SCREEN_WIDTH).step_by(8) {
                let char_col = x / 8;
                let (color, ch) =
                    self.mem.read(U14::try_from(0x400 + (char_row * 40 + char_col)).unwrap()).into();
                // find ch in char rom
                let bm = self.mem.read_data(U14::try_from(0x1000 + (8 * (ch as usize)) + (y % 8)).unwrap());
                self.screen.borrow_mut().set_char_line(
                    Point(x, y),
                    Color::try_from(color).unwrap(),
                    Color::try_from(0).unwrap(),
                    bm,
                );
            }
        }
    }
}

impl<T> VIC20<T> {
    pub fn update_banking(&mut self, state: mem::BankingState) {
        self.mem.banking_state = state;
    }
}

use super::mos6510::{MemoryArea, WriteResult};

/// These map the VIC20 Control Registers
impl<T> MemoryArea for VIC20<T> {
    fn read(&self, _addr: u16) -> u8 {
        0
        // unimplemented!()
    }
    fn write(&mut self, _addr: u16, _v: u8) -> WriteResult {
        WriteResult::Wrote
        // unimplemented!()
    }
}
