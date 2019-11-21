use super::mos6510::{MemoryArea, WriteResult};

pub struct ROM {
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

impl MemoryArea for &'_ ROM {
    fn read(&self, addr: u16) -> u8 {
        (*self).read(addr)
    }

    fn write(&mut self, addr: u16, val: u8) -> WriteResult {
        (*self).write(addr, val)
    }
}

impl From<&[u8]> for ROM {
    fn from(bytes: &[u8]) -> ROM {
        ROM {
            content: bytes.to_owned(),
        }
    }
}

pub mod stock {
    use super::*;
    use lazy_static::lazy_static;

    lazy_static! {
        // FIXME include_bytes
        pub static ref BASIC_ROM: ROM = ROM::from(include_bytes!("../rsrc/basic_rom.img") as &[u8]);
        pub static ref KERNAL: ROM = ROM::from(include_bytes!("../rsrc/kernal.img") as &[u8]);
        pub static ref CHAR_ROM: ROM = ROM::from(include_bytes!("../rsrc/char_rom.img") as &[u8]);
    }
}
