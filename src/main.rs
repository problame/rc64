#[macro_use]
mod utils;
mod color_ram;
mod mos6510;
mod ram;
mod rom;
mod vic20;
mod backend {
    pub(super) mod fb_minifb;
}

use crate::color_ram::ColorRAM;
use crate::ram::RAM;
use crate::utils::R2C;

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
    let ram = r2c_new!(RAM::default());
    let color_ram = r2c_new!(ColorRAM::default());

    let screen = Box::new(backend::fb_minifb::Minifb::new());

    let vic20 = r2c_new!(vic20::VIC20::new(
        rom::stock::CHAR_ROM,
        ram.clone(),
        color_ram.clone(),
        screen
    ));

    use mos6510::*;

    // FIXME These probably all need to be Rc<RefCell<.>>s
    let areas = enum_map::enum_map! {
        MemoryAreaKind::BasicRom =>  r2c_new!(rom::stock::BASIC_ROM) as R2C<dyn MemoryArea>,
        MemoryAreaKind::KernelRom => r2c_new!(rom::stock::KERNAL) as R2C<dyn MemoryArea>,
        MemoryAreaKind::IO1 =>       r2c_new!(UnimplMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::IO2 =>       r2c_new!(UnimplMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::CIA2 =>      r2c_new!(UnimplMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::CIA1 =>      r2c_new!(UnimplMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::ColorRam =>  color_ram.clone() as R2C<dyn MemoryArea>,
        MemoryAreaKind::SID =>       r2c_new!(UnimplMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::VIC =>       vic20.clone(),
        MemoryAreaKind::CharRom =>   r2c_new!(rom::stock::CHAR_ROM) as R2C<dyn MemoryArea>,
    };

    let mut mpu = mos6510::MOS6510::new(areas, ram.clone());

    loop {
        vic20.borrow_mut().cycle();
        mpu.cycle();
    }
}
