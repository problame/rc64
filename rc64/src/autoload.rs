use crate::cia::keyboard::C64Key;
use crate::cia::keyboard::EmulatedKeyboard;
use crate::ram::RAM;
use crate::utils::R2C;

pub struct PRG {
    lo: u8,
    hi: u8,
    start_addr: u16,
    text: Vec<u8>,
}

use std::convert::TryFrom;

#[derive(Debug, Eq, PartialEq)]
pub enum PRGDecodeErr {
    TooShort,
    TooLong,
}

impl TryFrom<Vec<u8>> for PRG {
    type Error = PRGDecodeErr;
    fn try_from(prg: Vec<u8>) -> Result<Self, Self::Error> {
        if prg.len() < 3 {
            return Err(PRGDecodeErr::TooShort);
        }
        let lo: u8 = prg[0];
        let hi: u8 = prg[1];
        let start_addr = ((hi as u16) << 8) | (lo as u16);
        let text = prg[2..].to_vec();
        let end = (start_addr as u64) + (text.len() as u64);
        if end >= (std::u16::MAX as u64) {
            return Err(PRGDecodeErr::TooLong);
        }
        Ok(PRG { lo, hi, start_addr, text })
    }
}

impl PRG {
    pub fn write_to_ram(&self, ram: &mut RAM) {
        ram.write(0x002b, self.lo);
        ram.write(0x002c, self.hi);
        for (i, b) in self.text.iter().enumerate() {
            ram.write(self.start_addr + (i as u16), *b);
        }
    }
}

use std::fmt::{self, Formatter};

impl fmt::Debug for PRG {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        let PRG { start_addr, text, .. } = self;
        formatter
            .debug_struct("PRG")
            .field("start_addr", &format!("0x{:04x}", start_addr))
            .field("len", &text.len())
            .finish()
    }
}

use std::time::{Duration, Instant};

pub struct AutloadState {
    st: State,
    prg: PRG,
    ram: R2C<RAM>,
    keyboard_emulator: R2C<EmulatedKeyboard>,
}

impl AutloadState {
    const CHAR_BUFFER_START: u16 = 0x0400;
    const NUM_CHARS: usize = 40 * 6; // first 6 lines
    const CHECK_INTERVAL: Duration = Duration::from_millis(100);

    const INITIAL_BOOT_PROMPT: [u8; Self::NUM_CHARS] = [
        32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
        32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 42, 42, 42, 42, 32, 3,
        15, 13, 13, 15, 4, 15, 18, 5, 32, 54, 52, 32, 2, 1, 19, 9, 3, 32, 22, 50, 32, 42, 42, 42, 42, 32,
        32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
        32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 54, 52, 11, 32, 18,
        1, 13, 32, 19, 25, 19, 20, 5, 13, 32, 32, 51, 56, 57, 49, 49, 32, 2, 1, 19, 9, 3, 32, 2, 25, 20, 5,
        19, 32, 6, 18, 5, 5, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
        32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 18, 5, 1,
        4, 25, 46, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
        32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
    ];
}

enum State {
    Init { last_check_at: Instant },
    LoadedAndRunInjected,
}

impl AutloadState {
    pub fn new(prg: PRG, ram: R2C<RAM>, keyboard_emulator: R2C<EmulatedKeyboard>) -> Self {
        AutloadState { st: State::Init { last_check_at: Instant::now() }, ram, prg, keyboard_emulator }
    }

    pub fn cycle(&mut self) {
        let new_state = match &mut self.st {
            State::LoadedAndRunInjected => return,
            State::Init { last_check_at } => {
                let now = std::time::Instant::now();
                if !now
                    .checked_duration_since(*last_check_at)
                    .map(|d| d > Self::CHECK_INTERVAL)
                    .unwrap_or(false)
                {
                    return;
                }

                let current = {
                    let mut v = Vec::new();
                    let (start, len) = (Self::CHAR_BUFFER_START, Self::NUM_CHARS as u16);
                    let ram = self.ram.borrow();
                    for addr in start..(start + len) {
                        v.push(ram.read(addr));
                    }
                    v
                };

                if !current[..].eq(&Self::INITIAL_BOOT_PROMPT[..]) {
                    return;
                }

                println!("c64 bootup prompt detected");
                self.prg.write_to_ram(&mut self.ram.borrow_mut());
                println!("loaded prg to ram: {:?}", self.prg);

                for key in vec![C64Key::R, C64Key::U, C64Key::N, C64Key::Return] {
                    self.keyboard_emulator
                        .borrow_mut()
                        .enqueue_key_event(Duration::from_millis(250), vec![key]);
                }

                State::LoadedAndRunInjected
            }
        };
        self.st = new_state;
    }
}
