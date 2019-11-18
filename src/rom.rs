
use super::mos6510::{MemoryArea, WriteResult};

struct ROM {
    content: Vec<u8>,
}

impl MemoryArea for ROM {
    fn read(&self, addr: u16) -> u8 {
        self.content[addr as usize]
    }

    fn write(&mut self, addr: u16, val: u8) -> WriteResult {
        WriteResult::Ignored
    }
}

impl From<&[u8]> for ROM {
    fn from(bytes: &[u8]) -> ROM {
        ROM {content:  bytes.to_owned() }
    }
}

// FIXME include_bytes
const TODO_INCLUDE_BYTES: &[u8; 0] = &[];

pub fn stock_basic_rom() -> impl MemoryArea {
    ROM::from(&TODO_INCLUDE_BYTES[0..0])
}

pub fn stock_kernal() -> impl MemoryArea {
    ROM::from(&TODO_INCLUDE_BYTES[0..0])
}

pub fn stock_char_rom()  -> impl MemoryArea {
    ROM::from(&TODO_INCLUDE_BYTES[0..0])
}