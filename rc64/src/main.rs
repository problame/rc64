#[macro_use]
extern crate strum_macros;

#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate derive_more;

#[macro_use]
mod utils;
mod cia;
mod color_ram;
mod interrupt;
mod mos6510;
mod ram;
mod rom;
mod vic20;
mod backend {
    pub(super) mod fb_minifb;
}
mod autoload;
mod cycler;
mod debugger_cli;

use crate::cia::keyboard::EmulatedKeyboard;
use crate::cia::{CIAKind, CIA};
use crate::color_ram::ColorRAM;
use crate::ram::RAM;
use crate::utils::R2C;
use crate::vic20::RasterBreakpointBackend;

use std::convert::TryFrom;
use std::path::PathBuf;
use std::sync::Arc;
use structopt::StructOpt;

struct UnimplMemoryArea;
impl mos6510::MemoryArea for UnimplMemoryArea {
    fn read(&self, _addr: u16) -> u8 {
        unimpl!(0)
    }
    fn write(&mut self, _addr: u16, _d: u8) -> mos6510::WriteResult {
        unimpl!(mos6510::WriteResult::Ignored)
    }
}

#[derive(Debug, StructOpt)]
struct Args {
    #[structopt(long, help = "use custom kernal image")]
    kernal: Option<PathBuf>,

    #[structopt(long, help = "trap to debugger after first instr")]
    trap_init: bool,

    #[structopt(help = "autostart prg file")]
    prg: Option<PathBuf>,

    #[structopt(long, help = "number of allowed slow cycles per second", default_value = "150000")]
    slow_cycle_thresh: u64,

    #[structopt(long, help = "disable clock-frequency emulation (full-speed mode)")]
    disable_clock_freq_limit: bool,

    #[structopt(long, help = "run for specified number of clock cycles, exit when reached")]
    exit_after_cycles: Option<u64>,
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

    let vic20 =
        r2c_new!(vic20::VIC20::new(rom::stock::CHAR_ROM, ram.clone(), color_ram.clone(), screen.clone()));

    let keyboard_emulator = r2c_new!(EmulatedKeyboard::new());

    let cia1 = r2c_new!(CIA::<()>::new(CIAKind::Chip1 {
        peripherals: r2c_new!((screen, keyboard_emulator.clone()))
    }));
    let cia2 = r2c_new!(CIA::new(CIAKind::Chip2 { vic: vic20.clone() }));

    use mos6510::*;

    let areas = enum_map::enum_map! {
        MemoryAreaKind::BasicRom =>  r2c_new!(rom::stock::BASIC_ROM) as R2C<dyn MemoryArea>,
        MemoryAreaKind::KernelRom => kernal.clone(),
        MemoryAreaKind::IO1 =>       r2c_new!(UnimplMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::IO2 =>       r2c_new!(UnimplMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::CIA1 =>      cia1.clone(),
        MemoryAreaKind::CIA2 =>      cia2.clone(),
        MemoryAreaKind::ColorRam =>  color_ram.clone() as R2C<dyn MemoryArea>,
        MemoryAreaKind::SID =>       r2c_new!(UnimplMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::VIC =>       vic20.clone(),
        MemoryAreaKind::CharRom =>   r2c_new!(rom::stock::CHAR_ROM) as R2C<dyn MemoryArea>,
        MemoryAreaKind::Unmapped =>      r2c_new!(UnimplMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::CartRomLow =>      r2c_new!(UnimplMemoryArea) as R2C<dyn MemoryArea>,
        MemoryAreaKind::CartRomHi =>      r2c_new!(UnimplMemoryArea) as R2C<dyn MemoryArea>,
    };

    let debugger = r2c_new!(mos6510::Debugger::new(vic20.clone() as R2C<dyn RasterBreakpointBackend>));
    if args.trap_init {
        debugger.borrow_mut().add_pc_breakpoint(0);
    }
    let debugger_cli = r2c_new!(debugger_cli::DebuggerCli::default());

    let sigint_pending = Arc::new(std::sync::atomic::AtomicBool::default());
    signal_hook::flag::register(signal_hook::SIGINT, Arc::clone(&sigint_pending))
        .expect("cannot register SIGINT handler");

    let mut mpu = mos6510::MOS6510::new(
        areas,
        ram.clone(),
        debugger.clone(),
        debugger_cli as R2C<dyn DebuggerUI>,
        keyboard_emulator.clone(),
    );

    let mut autoload_state = args
        .prg
        .map(|path| std::fs::read(path).expect("read PRG"))
        .map(|bytes| autoload::PRG::try_from(bytes).expect("parse PRG"))
        .map(|prg| autoload::AutloadState::new(prg, ram.clone(), keyboard_emulator.clone()));

    let mut loop_helper = cycler::Cycler::new(cycler::Config {
        guest_core_cps_hz: 1_000_000.0,
        report_interval: Some(std::time::Duration::from_secs(1)),
    });

    let mut cycles = 0;

    let mut last_report: Option<cycler::Report> = None;
    loop {
        if !args.disable_clock_freq_limit {
            let report = loop_helper.cycle(cycles);
            if let Some(delta) =
                report.as_ref().and_then(|new| last_report.as_ref().and_then(|old| new.delta(&old)))
            {
                if delta.slow_cycles > args.slow_cycle_thresh {
                    println!("slow cycles exceeded threshold {}: {:?}", args.slow_cycle_thresh, delta);
                    loop_helper.reset_startup(cycles);
                }
            }
            if let Some(report) = report {
                last_report = Some(report);
            }
        }

        debugger.borrow_mut().update_cycles(cycles);

        let cia_irq = cia1.borrow().cycle();
        let cia_nmi = cia2.borrow().cycle();

        let vic_irq = vic20.borrow_mut().cycle(cycles, mpu.debugger_refmut());

        if let Some(autoload_state) = &mut autoload_state {
            autoload_state.cycle();
        }

        if sigint_pending.load(std::sync::atomic::Ordering::SeqCst) {
            eprintln!("SIGINT CAUGHT");
            sigint_pending.store(false, std::sync::atomic::Ordering::SeqCst);
            debugger.borrow_mut().break_after_next_decode();
        }
        mpu.cycle(cia_irq.or(vic_irq), cia_nmi);

        cycles += 1;

        if let Some(after_cycles) = args.exit_after_cycles {
            if cycles >= after_cycles {
                std::process::exit(0);
            }
        }
    }
}
