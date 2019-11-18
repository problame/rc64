pub struct VIC20 {}

impl VIC20 {
    pub fn new() -> Self { unimplemented!() }
}

use super::mos6510::{MemoryArea, WriteResult};
impl MemoryArea for VIC20 {
    fn read(&self, addr: u16) -> u8 {
        unimplemented!()
    } 
    fn write(&mut self, addr: u16, v: u8) -> WriteResult {
        unimplemented!()
    }
}

