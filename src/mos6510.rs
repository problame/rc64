mod mem;

use crate::ram::RAM;
pub use mem::*;
use std::cell::RefCell;
use std::rc::Rc;

pub struct MOS6510 {
    mem: MemoryView,
    chr: u8,
}

impl MOS6510 {
    pub fn new(areas: Areas, ram: Rc<RefCell<RAM>>) -> Self {
        let mem = MemoryView::new(areas, ram);
        MOS6510 { mem, chr: 0 }
    }

    pub fn cycle(&mut self) {
        self.mem.write(0x400, self.chr);
        std::thread::sleep(std::time::Duration::from_millis(200));
        self.chr = (self.chr + 1) % 24;

        for i in 0xd800..0xdc00 {
            self.mem.write(i, self.chr % 16)
        }
    }
}
