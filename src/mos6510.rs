mod mem;

use crate::ram::RAM;
pub use mem::*;
use std::cell::RefCell;
use std::rc::Rc;

pub struct MOS6510 {
    mem: MemoryView,
}

impl MOS6510 {
    pub fn new(areas: Areas, ram: Rc<RefCell<RAM>>) -> Self {
        let mem = MemoryView::new(areas, ram);
        MOS6510 { mem }
    }

    pub fn cycle(&mut self) {
        unimplemented!()
    }
}
