use super::mos6510::{MemoryArea, WriteResult};

pub struct ROM<T> {
    content: T,
}

impl<T: AsRef<[u8]>> MemoryArea for ROM<T> {
    fn read(&self, addr: u16) -> u8 {
        self.content.as_ref()[addr as usize]
    }

    fn write(&mut self, _addr: u16, _val: u8) -> WriteResult {
        WriteResult::Ignored
    }
}

impl<T: AsRef<[u8]>> MemoryArea for &'_ mut ROM<T> {
    fn read(&self, addr: u16) -> u8 {
        (**self).read(addr)
    }

    fn write(&mut self, addr: u16, val: u8) -> WriteResult {
        (**self).write(addr, val)
    }
}

impl From<Vec<u8>> for ROM<Vec<u8>> {
    fn from(bytes: Vec<u8>) -> Self {
        ROM {
            content: bytes.clone(),
        }
    }
}

impl From<&[u8]> for ROM<Vec<u8>> {
    fn from(bytes: &[u8]) -> Self {
        ROM {
            content: bytes.to_owned(),
        }
    }
}

pub mod stock {
    use super::*;

    // FIXME include_bytes
    pub const BASIC_ROM: ROM<&'static [u8]> = ROM {
        content: include_bytes!("../rsrc/basic_rom.img"),
    };
    pub const KERNAL: ROM<&'static [u8]> = ROM {
        content: include_bytes!("../rsrc/kernal.img"),
    };
    pub const CHAR_ROM: ROM<&'static [u8]> = ROM {
        content: include_bytes!("../rsrc/char_rom.img"),
    };
}
