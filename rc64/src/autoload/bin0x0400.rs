use crate::mos6510::{
    instr::{Addr, Instr, Op},
    InterruptPending, State, MOS6510,
};
use crate::ram::RAM;
use crate::utils::R2C;

pub struct AutoloadState {
    cycles: usize,
    bin_bytes: Vec<u8>,
    ram: R2C<RAM>,
    cpu: R2C<MOS6510>,
}

impl AutoloadState {
    const ENTRY_POINT: u16 = 0x0400;

    pub fn new(bin_bytes: Vec<u8>, ram: R2C<RAM>, cpu: R2C<MOS6510>) -> Self {
        AutoloadState { bin_bytes, ram, cpu, cycles: 0 }
    }
}

impl super::Autloader for AutoloadState {
    fn cycle(&mut self) {
        match self.cycles {
            0 => {
                self.cycles += 1;
                // fallthrough
            }
            _ => return, // ignore
        }

        let mut ram = self.ram.borrow_mut();
        eprintln!("writing RAM length=0x{:x}", self.bin_bytes.len());
        for (i, b) in self.bin_bytes.iter().enumerate() {
            ram.write(i as u16, *b);
        }
        drop(ram);

        let mut cpu = self.cpu.borrow_mut();
        cpu.set_state(State::DecodedInstr {
            interrupts: InterruptPending::default(),
            next_instr: Instr(Op::JMP, Addr::Abs(Self::ENTRY_POINT)),
        });
        cpu.mem_mut().write(0x0000, 0);
        cpu.mem_mut().write(0x0001, 0);
    }
}
