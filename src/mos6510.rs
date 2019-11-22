mod instr;
mod mem;

use crate::ram::RAM;
pub use mem::*;
use std::cell::RefCell;
use std::rc::Rc;

pub struct MOS6510 {
    mem: MemoryView,
    reg: Regs,
    state: State,
}

// https://www.c64-wiki.com/wiki/Processor_Status_Register
bitflags! {
    #[derive(Default)]
    struct Flags: u8 {
        const CARRY = 0b00_00_00_01;
        const ZERO = 0b00_00_00_10;
        const IRQD = 0b00_00_01_00;
        const DEC = 0b00_00_10_00;
        const BRK = 0b00_01_00_00;
        // unused bit 5
        const OVFL = 0b01_00_00_00;
        const NEG = 0b10_00_00_00;
    }
}

struct Regs {
    pc: u16,
    sp: u8,
    a: u8,
    x: u8,
    y: u8,
    p: Flags,
}

const STACK_TOP: u16 = 0x01ff;
const STACK_BOTTOM: u16 = 0x0100;
const RESET_VEC: u16 = 0xfffc;

impl Default for Regs {
    fn default() -> Self {
        Regs {
            pc: 0,
            sp: 0x0,
            a: 0,
            x: 0,
            y: 0,
            p: Flags::default(),
        }
    }
}

impl Regs {
    #[inline]
    fn sp_abs(&self) -> u16 {
        STACK_BOTTOM + (self.sp as u16)
    }

    #[inline]
    fn pc_r_add(&mut self, o: i8) -> u16 {
        (((self.pc as i32).checked_add(o as i32).unwrap()) as u16) + 2 // FIXME try_from ?
    }
}

use instr::*;

enum State {
    Reset,
    Running,
}

impl MOS6510 {
    pub fn new(areas: Areas, ram: Rc<RefCell<RAM>>) -> Self {
        let mem = MemoryView::new(areas, ram);
        let reg = Regs::default();
        MOS6510 {
            mem,
            reg,
            state: State::Reset,
        }
    }

    pub fn cycle(&mut self) {
        match self.state {
            State::Reset => {
                self.state = State::Running;
                self.apply_instr(Instr(Op::JMP, Addr::Ind(RESET_VEC)), None);
                return;
            }
            State::Running => (), // fallthrough
        };

        let mut buf = [0; 3];
        self.mem.read_one_to_three(self.reg.pc, &mut buf);
        match instr::decode_instr(&buf) {
            Ok((instr, len)) => self.apply_instr(instr, self.reg.pc.checked_add(len as u16)),
            Err(e) => panic!("instruction decode error: {:?}", e),
        }
    }

    fn apply_instr(&mut self, instr: Instr, mut next_pc: Option<u16>) {
        // println!("pc={:x} {}", self.reg.pc, instr);
        use instr::Addr::*;
        use instr::Op::*;
        match instr {
            Instr(JMP, Ind(a)) => next_pc = Some(self.mem.read_u16(a)),
            Instr(JMP, Abs(a)) => next_pc = Some(a),
            Instr(LDA, Imm(v)) => self.reg.a = v,
            Instr(STA, Abs(a)) => self.mem.write(a, self.reg.a),
            Instr(TAY, Imp) => self.reg.y = self.reg.a,
            Instr(TXA, Imp) => self.reg.a = self.reg.x,
            Instr(TYA, Imp) => self.reg.a = self.reg.y,
            Instr(CLC, Imp) => self.reg.p.set(Flags::CARRY, false),
            Instr(ADC, Imm(v)) => {
                let (v, ovfl1) = v.overflowing_add(self.reg.p.contains(Flags::CARRY) as u8);
                let (res, ovfl2) = self.reg.a.overflowing_add(v);
                let ovfl = ovfl1 || ovfl2;
                self.reg.a = res;
                self.reg.p.set(Flags::NEG, (res as i8) < 0);
                self.reg.p.set(Flags::OVFL, ovfl);
                self.reg.p.set(Flags::ZERO, res == 0);
                self.reg.p.set(Flags::CARRY, ovfl)
            }
            Instr(BCS, PCr(o)) => {
                if self.reg.p.contains(Flags::CARRY) {
                    next_pc = Some(self.reg.pc_r_add(o));
                }
            }
            Instr(BEQ, PCr(o)) => {
                if self.reg.p.contains(Flags::ZERO) {
                    next_pc = Some(self.reg.pc_r_add(o));
                }
            }
            Instr(BMI, PCr(o)) => {
                if self.reg.p.contains(Flags::NEG) {
                    next_pc = Some(self.reg.pc_r_add(o));
                }
            }
            Instr(INX, Imp) => self.reg.x = self.reg.x.overflowing_add(1).0,
            Instr(LDX, Imm(v)) => self.reg.x = v,
            Instr(LDA, AbX(a)) => self.reg.a = self.mem.read(a + (self.reg.x as u16)),
            Instr(STX, Abs(a)) => self.mem.write(a, self.reg.x),
            Instr(CMP, addr) => {
                let subtrahend = match addr {
                    AbX(a) => self.mem.read(a + (self.reg.x as u16)),
                    Imm(i) => i,
                    x => panic!("unimplemented addressing mode {:?}", x),
                };
                let (res, ovfl) = self.reg.a.overflowing_sub(subtrahend);
                self.reg.p.set(Flags::ZERO, res == 0);
                self.reg.p.set(Flags::NEG, (res as i8) < 0);
                self.reg.p.set(Flags::CARRY, ovfl); // ??
            }
            Instr(SEI, Imp) => self.reg.p |= Flags::IRQD,
            Instr(CLI, Imp) => self.reg.p.remove(Flags::IRQD),
            Instr(TXS, Imp) => self.reg.sp = self.reg.x,
            Instr(CLD, Imp) => self.reg.p.remove(Flags::DEC),
            Instr(DEX, Imp) => self.reg.x = self.reg.x.overflowing_sub(1).0,
            Instr(JSR, Abs(a)) => {
                self.reg.pc += 2;
                self.reg.sp -= 2; // TODO stack overflow instrumentation
                self.mem.write_u16(self.reg.sp_abs(), self.reg.pc);
                next_pc = Some(a);
            }
            Instr(RTS, Imp) => {
                next_pc = Some(self.mem.read_u16(self.reg.sp_abs()));
                self.reg.sp += 2;
            }
            Instr(BNE, PCr(o)) => next_pc = Some(self.reg.pc_r_add(o)),
            _ => panic!("unimplemented instruction: {}", instr),
        }

        if let Some(pc) = next_pc {
            self.reg.pc = pc;
        }
    }
}
