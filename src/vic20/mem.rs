use crate::mos6510::MemoryArea;
use crate::ram::RAM;
use crate::rom::ROM;
use std::ops::Add;

use derive_more::{Add, Into, Sub};
use enum_map::{enum_map, EnumMap};
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Add, Sub, Into)]
pub struct U14(u16);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Add, Sub, Into)]
pub struct U12(u16);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Add, Sub, Into)]
pub struct U4(u8);

impl Add<u16> for U14 {
    type Output = u16;

    fn add(self, rhs: u16) -> Self::Output {
        let U14(lhs) = self;

        lhs + rhs
    }
}

impl From<(U4, u8)> for U12 {
    fn from(pair: (U4, u8)) -> Self {
        let (U4(left), right) = pair;
        U12((left as u16) << 8 | right as u16)
    }
}

static CHAR_ROM_BEGIN: U14 = U14(0x1000);
static CHAR_ROM_LEN: U14 = U14(0x1000);
lazy_static! {
    static ref RAM_OFFSET_MAP: EnumMap<BankingState, u16> = enum_map! {
        Bank0 => 0x0000,
        Bank1 => 0x4000,
        Bank2 => 0x8000,
        Bank3 => 0xc000,
    };
}

pub struct MemoryView<'r> {
    banking_state: BankingState,
    char_rom: &'r ROM,
    ram: Rc<RefCell<RAM>>,
}

#[derive(Clone, Copy, Debug, enum_map::Enum, PartialEq, Eq)]
enum BankingState {
    Bank0,
    Bank1,
    Bank2,
    Bank3,
}

impl<'r> MemoryView<'r> {
    pub fn new(char_rom: &'r ROM, ram: Rc<RefCell<RAM>>) -> Self {
        MemoryView {
            banking_state: BankingState::Bank0,
            char_rom,
            ram,
        }
    }

    pub fn read(&self, addr: U14) -> U12 {
        let color: U4 = U4(0); // TODO Read color RAM

        let data = if self.char_rom_is_mapped()
            && CHAR_ROM_BEGIN <= addr
            && addr - CHAR_ROM_BEGIN < CHAR_ROM_LEN
        {
            self.char_rom.read(u16::from(addr - CHAR_ROM_BEGIN))
        } else {
            self.ram
                .borrow()
                .read(addr + RAM_OFFSET_MAP[self.banking_state])
        };

        U12::from((color, data))
    }

    fn char_rom_is_mapped(&self) -> bool {
        use BankingState::*;
        self.banking_state == Bank0 || self.banking_state == Bank1
    }
}
