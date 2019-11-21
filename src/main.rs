mod mos6510;
mod ram;
mod rom;
mod vic20;

use crate::ram::RAM;

use std::cell::RefCell;

struct UnimplMemoryArea;
impl mos6510::MemoryArea for UnimplMemoryArea {
    fn read(&self, _addr: u16) -> u8 {
        unimplemented!()
    }
    fn write(&mut self, _addr: u16, _d: u8) -> mos6510::WriteResult {
        unimplemented!()
    }
}

fn main() {
    use std::rc::Rc;
    let ram = Rc::new(RefCell::new(RAM::default()));

    let vic20 = Rc::new(vic20::VIC20::new(&rom::stock::CHAR_ROM, ram.clone()));

    use mos6510::*;

    // FIXME These probably all need to be Rc<RefCell<.>>s
    let areas = enum_map::enum_map! {
        MemoryAreaKind::BasicRom =>  Rc::new(&*rom::stock::BASIC_ROM) as Rc<dyn MemoryArea>,
        MemoryAreaKind::KernelRom => Rc::new(&*rom::stock::KERNAL) as Rc<dyn MemoryArea>,
        MemoryAreaKind::IO1 =>       Rc::new(UnimplMemoryArea) as Rc<dyn MemoryArea>,
        MemoryAreaKind::IO2 =>       Rc::new(UnimplMemoryArea) as Rc<dyn MemoryArea>,
        MemoryAreaKind::CIA2 =>      Rc::new(UnimplMemoryArea) as Rc<dyn MemoryArea>,
        MemoryAreaKind::CIA1 =>      Rc::new(UnimplMemoryArea) as Rc<dyn MemoryArea>,
        MemoryAreaKind::ColorRam =>  Rc::new(UnimplMemoryArea) as Rc<dyn MemoryArea>,
        MemoryAreaKind::SID =>       Rc::new(UnimplMemoryArea) as Rc<dyn MemoryArea>,
        MemoryAreaKind::VIC =>       vic20.clone(),
        MemoryAreaKind::CharRom =>   Rc::new(&*rom::stock::CHAR_ROM) as Rc<dyn MemoryArea>,
    };

    let mut mpu = mos6510::MOS6510::new(areas, ram.clone());

    loop {
        vic20.cycle();
        mpu.cycle();
    }
}
