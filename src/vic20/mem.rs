use crate::color_ram::ColorRAM;
use crate::mos6510::MemoryArea;
use crate::ram::RAM;
use crate::rom::ROM;
use crate::utils::R2C;
use std::ops::Add;

use derive_more::{Add, Into, Sub};
use enum_map::{enum_map, EnumMap};
use lazy_static::lazy_static;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Add, Sub, Into)]
pub struct U14(u16);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Add, Sub, Into)]
pub struct U12(u16);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Add, Sub, Into)]
pub struct U4(u8);

const U14_MAX: usize = 1 << 14 - 1;

impl Add<u16> for U14 {
    type Output = u16;

    fn add(self, rhs: u16) -> Self::Output {
        let U14(lhs) = self;

        lhs + rhs
    }
}

impl Add<usize> for U14 {
    type Output = U14;

    fn add(self, rhs: usize) -> Self::Output {
        let U14(lhs) = self;
        let U14(rhs) = U14::try_from(rhs).unwrap();
        U14(lhs + rhs)
    }
}

impl From<(U4, u8)> for U12 {
    fn from(pair: (U4, u8)) -> Self {
        let (U4(left), right) = pair;
        U12((left as u16) << 8 | right as u16)
    }
}

impl Into<(U4, u8)> for U12 {
    fn into(self) -> (U4, u8) {
        let left = self.0 >> 8;
        let right = self.0 & ((1 << 8) - 1);
        (U4(left as u8), right as u8)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn u4u8_u12() {
        let input = (U4(23), 254);
        let u12: U12 = input.into();
        let output: (U4, u8) = u12.into();
        assert_eq!(input, output);
    }
}

use std::convert::TryFrom;

impl TryFrom<u16> for U14 {
    type Error = ();
    fn try_from(input: u16) -> Result<Self, Self::Error> {
        if input as usize <= U14_MAX {
            Ok(U14(input))
        } else {
            Err(())
        }
    }
}

impl From<u8> for U14 {
    fn from(input: u8) -> Self {
        U14(input as u16)
    }
}

impl TryFrom<usize> for U14 {
    type Error = ();
    fn try_from(input: usize) -> Result<Self, Self::Error> {
        if input <= U14_MAX {
            Ok(U14(input as u16))
        } else {
            Err(())
        }
    }
}

pub const CHAR_ROM_BEGIN: U14 = U14(0x1000);
pub const CHAR_ROM_LEN: U14 = U14(0x1000);
lazy_static! {
    static ref RAM_OFFSET_MAP: EnumMap<BankingState, u16> = enum_map! {
        BankingState::Bank0 => 0x0000,
        BankingState::Bank1 => 0x4000,
        BankingState::Bank2 => 0x8000,
        BankingState::Bank3 => 0xc000,
    };
}

pub struct MemoryView<T> {
    pub(super) banking_state: BankingState,
    char_rom: ROM<T>,
    ram: R2C<RAM>,
    color_ram: R2C<ColorRAM>,
}

#[derive(Clone, Copy, Debug, enum_map::Enum, PartialEq, Eq)]
pub(super) enum BankingState {
    Bank0,
    Bank1,
    Bank2,
    Bank3,
}

impl<T: AsRef<[u8]>> MemoryView<T> {
    pub fn new(char_rom: ROM<T>, ram: R2C<RAM>, color_ram: R2C<ColorRAM>) -> Self {
        MemoryView {
            banking_state: BankingState::Bank0,
            char_rom,
            ram,
            color_ram,
        }
    }

    #[inline]
    pub fn read_data_into(&self, addr: U14, dst: &mut [u8]) {
        dst.iter_mut()
            .enumerate()
            .for_each(|(idx, e)| *e = self.read_data(addr + idx))
    }

    pub fn read(&self, addr: U14) -> U12 {
        let color = {
            let color_idx = u16::from(addr) & ((1 << 8) - 1);
            U4(self.color_ram.borrow().read(color_idx) & ((1 << 4) - 1))
        };
        let data = self.read_data(addr);
        U12::from((color, data))
    }

    #[inline]
    pub fn read_data(&self, addr: U14) -> u8 {
        if self.char_rom_is_mapped()
            && CHAR_ROM_BEGIN <= addr
            && addr - CHAR_ROM_BEGIN < CHAR_ROM_LEN
        {
            self.char_rom.read(u16::from(addr - CHAR_ROM_BEGIN))
        } else {
            self.ram
                .borrow()
                .read(addr + RAM_OFFSET_MAP[self.banking_state])
        }
    }

    fn char_rom_is_mapped(&self) -> bool {
        use BankingState::*;
        self.banking_state == Bank0 || self.banking_state == Bank1
    }
}
