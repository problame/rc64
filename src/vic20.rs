mod mem;

use self::mem::MemoryView;
use crate::ram::RAM;
use crate::rom::ROM;
use std::cell::RefCell;
use std::rc::Rc;

pub struct VIC20<'r, T: AsRef<[u8]>> {
    mem: MemoryView<'r, T>,
}

impl<'r, T: AsRef<[u8]>> VIC20<'r, T> {
    pub fn new(char_rom: &'r ROM<T>, ram: Rc<RefCell<RAM>>) -> Self {
        VIC20 {
            mem: MemoryView::new(char_rom, ram),
        }
    }

    // FIXME should take &mut self
    pub fn cycle(&self) {
        unimplemented!()
    }
}

use super::mos6510::{MemoryArea, WriteResult};

/// These map the VIC20 Control Registers
impl<'r, T: AsRef<[u8]>> MemoryArea for VIC20<'r, T> {
    fn read(&mut self, addr: u16) -> u8 {
        unimplemented!()
    }
    fn write(&mut self, addr: u16, v: u8) -> WriteResult {
        unimplemented!()
    }
}
