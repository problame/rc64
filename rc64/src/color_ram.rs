pub struct ColorRAM {
    content: [u8; 0x400],
}
impl Default for ColorRAM {
    fn default() -> Self {
        ColorRAM { content: [0xff; 0x400] }
    }
}

impl ColorRAM {
    pub fn read(&self, addr: u16) -> u8 {
        self.content[addr as usize]
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        self.content[addr as usize] = val
    }
}

use crate::mos6510::{MemoryArea, WriteResult};

impl MemoryArea for ColorRAM {
    fn read(&self, addr: u16) -> u8 {
        ColorRAM::read(self, addr)
    }
    fn write(&mut self, addr: u16, val: u8) -> WriteResult {
        ColorRAM::write(self, addr, val);
        WriteResult::Wrote
    }
}
