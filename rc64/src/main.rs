#[macro_use]
extern crate strum_macros;

#[macro_use]
extern crate bitflags;

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
mod debugger_cli;

use crate::color_ram::ColorRAM;
use crate::ram::RAM;
use crate::utils::R2C;
use std::sync::Arc;

use std::path::PathBuf;
use structopt::StructOpt;

struct UnimplMemoryArea;
impl mos6510::MemoryArea for UnimplMemoryArea {
    fn read(&self, _addr: u16) -> u8 {
        unimplemented!()
    }
    fn write(&mut self, _addr: u16, _d: u8) -> mos6510::WriteResult {
        unimplemented!()
    }
}

struct HeadlessChickenMemoryArea;
impl mos6510::MemoryArea for HeadlessChickenMemoryArea {
    fn read(&self, _addr: u16) -> u8 {
        return 0;
    }
    fn write(&mut self, _addr: u16, _d: u8) -> mos6510::WriteResult {
        mos6510::WriteResult::Ignored
    }
}

#[derive(Debug, StructOpt)]
struct Args {
    #[structopt(long, help="use custom kernal image")]
    kernal: Option<PathBuf>,
}

fn main() {
    let args = Args::from_args();

    let kernal = args.kernal.map_or_else(
        || r2c_new!(rom::stock::KERNAL) as R2C<dyn MemoryArea>,
        |p| {
            let kernal_img = std::fs::read(p).expect("read custom kernal image failed");
            r2c_new!(rom::ROM::from(kernal_img)) as R2C<dyn MemoryArea>
        },
    );

    let ram = r2c_new!(RAM::default());
    let color_ram = r2c_new!(ColorRAM::default());

    let screen = r2c_new!(backend::fb_minifb::Minifb::new());

    let vic20 = r2c_new!(vic20::VIC20::new(rom::stock::CHAR_ROM, ram.clone(), color_ram.clone(), screen.clone()));

    use mos6510::*;

    // FIXME These probably all need to be Rc<RefCell<.>>s
    let areas = enum_map::enum_map! {
        MemoryAreaKind::BasicRom =>  r2c_new!(rom::stock::BASIC_ROM) as R2C<dyn MemoryArea>,
        MemoryAreaKind::KernelRom => kernal.clone(),
        MemoryAreaKind::IO1 =>       r2c_new!(HeadlessChickenMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::IO2 =>       r2c_new!(HeadlessChickenMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::CIA2 =>      r2c_new!(HeadlessChickenMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::CIA1 =>      r2c_new!(HeadlessChickenMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::ColorRam =>  color_ram.clone() as R2C<dyn MemoryArea>,
        MemoryAreaKind::SID =>       r2c_new!(HeadlessChickenMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::VIC =>       vic20.clone(),
        MemoryAreaKind::CharRom =>   r2c_new!(rom::stock::CHAR_ROM) as R2C<dyn MemoryArea>,
        MemoryAreaKind::Unmapped =>      r2c_new!(HeadlessChickenMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::CartRomLow =>      r2c_new!(HeadlessChickenMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::CartRomHi =>      r2c_new!(HeadlessChickenMemoryArea) as R2C<dyn MemoryArea>,
    };

    let debugger = r2c_new!(mos6510::Debugger::default());
    debugger.borrow_mut().add_pc_breakpoint(0);
    let debugger_cli = r2c_new!(debugger_cli::DebuggerCli::default());

    let sigint_pending = Arc::new(std::sync::atomic::AtomicBool::default());
    signal_hook::flag::register(signal_hook::SIGINT, Arc::clone(&sigint_pending)).expect("cannot register SIGINT handler");

    let mut mpu = mos6510::MOS6510::new(areas, ram.clone(), debugger.clone(), debugger_cli as R2C<dyn DebuggerUI>);

    use spin_sleep::LoopHelper;

    let mut loop_helper = LoopHelper::builder().report_interval_s(0.5).build_with_target_rate(1_000.0f64); // scale by 1000

    let mut cycles = 0;
    loop {
        if cycles % 1000 == 0 {
            // scale callback by 1000 because our loop is so fast
            loop_helper.loop_start();
        }
        cycles += 1;
        let is_vic_cycle = cycles % 100_000 == 0;

        if is_vic_cycle {
            vic20.borrow_mut().cycle();
        }

        if sigint_pending.load(std::sync::atomic::Ordering::SeqCst) {
            eprintln!("SIGINT CAUGHT");
            sigint_pending.store(false, std::sync::atomic::Ordering::SeqCst);
            debugger.borrow_mut().break_after_next_decode();
        }
        mpu.cycle();

        if let Some(r) = loop_helper.report_rate() {
            println!("Mcycles / sec = {:.?}", r / 1_000.0);
        }
        if cycles % 1000 == 0 {
            // scale callback by 1000 because our loop is so fast
            loop_helper.loop_sleep();
        }
    }
}
