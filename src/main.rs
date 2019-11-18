mod mos6510;
mod vic20;
mod rom;

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
    let vic20 = Rc::new(vic20::VIC20::new());

    use mos6510::*;

    let areas = enum_map::enum_map! {
            MemoryAreaKind::BasicRom => Rc::new(rom::stock_basic_rom()) as Rc<dyn MemoryArea>,
            MemoryAreaKind::KernelRom =>Rc::new(rom::stock_kernal()) as Rc<dyn MemoryArea>,
            MemoryAreaKind::IO1 =>      Rc::new(UnimplMemoryArea) as Rc<dyn MemoryArea>,
            MemoryAreaKind::IO2 =>      Rc::new(UnimplMemoryArea) as Rc<dyn MemoryArea>,
            MemoryAreaKind::CIA2 =>     Rc::new(UnimplMemoryArea) as Rc<dyn MemoryArea>,
            MemoryAreaKind::CIA1 =>     Rc::new(UnimplMemoryArea) as Rc<dyn MemoryArea>,
            MemoryAreaKind::ColorRam => Rc::new(UnimplMemoryArea) as Rc<dyn MemoryArea>,
            MemoryAreaKind::SID =>      Rc::new(UnimplMemoryArea) as Rc<dyn MemoryArea>,
            MemoryAreaKind::VIC =>      vic20.clone(),
            MemoryAreaKind::CharRom =>  Rc::new(rom::stock_char_rom()) as Rc<dyn MemoryArea>,
    };

    let mut mpu = mos6510::MOS6510::new(areas);

    loop {
        mpu.cycle();
    }
}
