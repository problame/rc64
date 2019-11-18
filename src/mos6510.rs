mod mem;

pub use mem::*;

pub struct MOS6510 {
    mem: MemoryView,
}

impl MOS6510 {
    pub fn new(areas: Areas) -> Self {
        let mem = MemoryView::new(areas);
        MOS6510 { mem }
    }

    pub fn cycle(&mut self) {
        unimplemented!()
    }
}
