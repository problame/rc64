pub mod instr;
mod mem;

use crate::cia::keyboard::EmulatedKeyboard;
use crate::interrupt::Interrupt;
use crate::ram::RAM;
use crate::utils::R2C;
use crate::vic20::RasterBreakpointBackend;
use crate::vic20::VIC20;
pub use mem::*;
use std::cell::RefCell;
use std::iter::FromIterator;
use std::rc::Rc;

use rc64_macros::gen_instr_match;

pub struct MOS6510 {
    ram: R2C<RAM>,
    mem: MemoryView,
    reg: Regs,
    state: State,
    debugger: R2C<Debugger>,
    debugger_ui: R2C<dyn DebuggerUI>,
    pub keyboard_emulator: R2C<EmulatedKeyboard>,
}

impl MOS6510 {
    pub fn reg(&self) -> &Regs {
        &self.reg
    }
    pub fn state(&self) -> &State {
        &self.state
    }
    pub fn mem(&self) -> &MemoryView {
        &self.mem
    }
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
        const UNUSED = 0b00_10_00_00;
        const OVFL = 0b01_00_00_00;
        const NEG = 0b10_00_00_00;
    }
}

use std::fmt::{self, Display, Formatter};

impl Display for Flags {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        macro_rules! flag {
            ($flag:expr, $ch:literal) => {
                let ch = if self.contains($flag) {
                    $ch.to_uppercase().to_string()
                } else {
                    $ch.to_lowercase().to_string()
                };
                write!(formatter, "{}", ch)?;
            };
        }
        flag!(Flags::NEG, 'N');
        flag!(Flags::OVFL, 'V');
        flag!(Flags::UNUSED, '-');
        flag!(Flags::BRK, 'B');
        flag!(Flags::DEC, 'D');
        flag!(Flags::IRQD, 'I');
        flag!(Flags::ZERO, 'Z');
        flag!(Flags::CARRY, 'C');
        fmt::Result::Ok(())
    }
}

#[derive(Debug)]
pub struct Regs {
    pc: u16,
    sp: u8,
    a: u8,
    x: u8,
    y: u8,
    p: Flags,
}

impl std::fmt::Display for Regs {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Regs { pc, sp, a, x, y, p } = &self;
        write!(formatter, "PC:{:04x} SP:{:02x} A:{:02x} X:{:02x} Y:{:02x} P:{}", pc, sp, a, x, y, p)
    }
}

const STACK_BOTTOM: u16 = 0x0100;
const STACK_TOP: u16 = STACK_BOTTOM + 0xff;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Display)]
#[repr(u16)]
pub enum ResetVec {
    NMI = 0xfffa,
    RESET = 0xfffc,
    IRQ = 0xfffe,
}

impl Default for Regs {
    fn default() -> Self {
        Regs { pc: 0, sp: 0x0, a: 0, x: 0, y: 0, p: Flags::default() }
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

    #[inline]
    fn set_nz_flags(&mut self, v: u8) {
        self.p.set(Flags::NEG, (v as i8) < 0);
        self.p.set(Flags::ZERO, v == 0);
    }

    #[inline]
    fn set_nzc_flags(&mut self, v: u8, carry: bool) {
        self.set_nz_flags(v);
        self.p.set(Flags::CARRY, carry);
    }

    #[inline]
    fn lda(&mut self, v: u8) {
        self.a = v;
        self.set_nz_flags(v);
    }

    #[inline]
    fn ldx(&mut self, v: u8) {
        self.x = v;
        self.set_nz_flags(v);
    }

    #[inline]
    fn ldy(&mut self, v: u8) {
        self.y = v;
        self.set_nz_flags(v);
    }

    // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    #[inline]
    fn add_to_a_with_carry_and_set_carry(&mut self, v: u8) {
        let pre_carry_u8: u8 = self.p.contains(Flags::CARRY) as u8;
        let (carry, sign, overflow) = {
            let sixth_lsbits_add = (self.a & 0b0111_1111) + (v & 0b0111_1111) + (pre_carry_u8);
            let c6 = (sixth_lsbits_add & 0b1000_0000) != 0;
            let a7 = self.a & 0b1000_0000 != 0;
            let v7 = v & 0b1000_0000 != 0;
            match (a7 as u8, v7 as u8, c6 as u8) {
                (0, 0, 0) => (0, 0, 0), // No unsigned carry or signed overflow	0x50+0x10=0x60	80+16=96	80+16=96
                (0, 0, 1) => (0, 1, 1), // No unsigned carry but signed overflow	0x50+0x50=0xa0	80+80=160	80+80=-96
                (0, 1, 0) => (0, 1, 0), // No unsigned carry or signed overflow	0x50+0x90=0xe0	80+144=224	80+-112=-32
                (0, 1, 1) => (1, 0, 0), // Unsigned carry, but no signed overflow	0x50+0xd0=0x120	80+208=288	80+-48=32
                (1, 0, 0) => (0, 1, 0), // No unsigned carry or signed overflow	0xd0+0x10=0xe0	208+16=224	-48+16=-32
                (1, 0, 1) => (1, 0, 0), // Unsigned carry but no signed overflow	0xd0+0x50=0x120	208+80=288	-48+80=32
                (1, 1, 0) => (1, 0, 1), // Unsigned carry and signed overflow	0xd0+0x90=0x160	208+144=352	-48+-112=96
                (1, 1, 1) => (1, 1, 0), // Unsigned carry, but no signed overflow	0xd0+0xd0=0x1a0	208+208=416	-48+-48=-96
                (_, _, _) => unreachable!(),
            }
        };
        let res_u16: u16 = (self.a as u16) + (pre_carry_u8 as u16) + (v as u16);
        let res = (res_u16 & 0xff) as u8;
        self.a = res;
        self.p.set(Flags::ZERO, res == 0);
        self.p.set(Flags::OVFL, overflow != 0);
        assert_eq!(res_u16 > 0xff, carry != 0);
        self.p.set(Flags::CARRY, carry != 0);
        self.p.set(Flags::NEG, sign != 0);
    }

    // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    #[inline]
    fn sub_from_a_with_carry_and_set_carry(&mut self, v: u8) {
        if cfg!(feature = "headless-chicken") {
            self.add_to_a_with_carry_and_set_carry(!v); // TODO doesn't pass test case
        } else {
            unimplemented!()
        }
    }

    #[inline]
    fn cmp_a(&mut self, v: u8) {
        self.cmp_impl(self.a, v)
    }

    #[inline]
    fn cmp_x(&mut self, v: u8) {
        self.cmp_impl(self.x, v)
    }

    #[inline]
    fn cmp_y(&mut self, v: u8) {
        self.cmp_impl(self.y, v)
    }

    #[inline]
    fn cmp_impl(&mut self, minuend: u8, subtrahend: u8) {
        let (res, ovfl) = minuend.overflowing_sub(subtrahend); // Yes, carry not included
        self.p.set(Flags::ZERO, res == 0);
        self.p.set(Flags::NEG, (res as i8) < 0);
        self.p.set(Flags::CARRY, !ovfl);
    }
}

use instr::*;

#[derive(Eq, PartialEq, Debug, Copy, Clone, Default)]
pub struct InterruptPending {
    // in order of priority
    pub inject_instr: Option<Instr>,
    pub irq: bool,
    pub nmi: bool,
}

impl Display for InterruptPending {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let InterruptPending { irq, nmi, inject_instr } = self;
        write!(f, "irq={:?} nmi={:?} inject_instr={:?}", irq, nmi, inject_instr)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum InterruptAckResult {
    None,
    ResetVec(ResetVec),
    InjectInstr(Instr),
}

impl InterruptPending {
    fn update_merge(&mut self, update: &InterruptPending) {
        if let Some(v) = update.inject_instr {
            self.inject_instr = Some(v);
        }
        self.irq |= update.irq;
        self.nmi |= update.nmi;
    }
    fn ack_highest_prio(&mut self) -> InterruptAckResult {
        if self.nmi {
            self.nmi = false;
            self.irq = false;
            return InterruptAckResult::ResetVec(ResetVec::NMI);
        } else if self.irq {
            self.irq = false;
            return InterruptAckResult::ResetVec(ResetVec::IRQ);
        } else {
            return self
                .inject_instr
                .map(|i| InterruptAckResult::InjectInstr(i))
                .take()
                .unwrap_or(InterruptAckResult::None);
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum State {
    CheckInterrupts { interrupts: InterruptPending },
    InjectResetVecJmp { interrupts: InterruptPending, reset_vec: ResetVec },
    Fetch { interrupts: InterruptPending },
    Decode { interrupts: InterruptPending },
    DecodedInstr { interrupts: InterruptPending, next_instr: Instr },
    ExecInstr { instr: Instr, remaining_cycles: usize, interrupts: InterruptPending },
    ExecInterruptStackInstrs { remaining_cycles: usize, interrupts: InterruptPending, reset_vec: ResetVec },
}

impl State {
    pub fn decoded_instr(&self) -> Option<&Instr> {
        match self {
            State::DecodedInstr { next_instr, .. } => Some(next_instr),
            _ => None,
        }
    }
    pub fn interrupts(&mut self) -> &mut InterruptPending {
        use State::*;
        match self {
            CheckInterrupts { interrupts } => interrupts,
            InjectResetVecJmp { interrupts, .. } => interrupts,
            Fetch { interrupts, .. } => interrupts,
            Decode { interrupts, .. } => interrupts,
            ExecInstr { interrupts, .. } => interrupts,
            DecodedInstr { interrupts, .. } => interrupts,
            ExecInterruptStackInstrs { interrupts, .. } => interrupts,
        }
    }
}

impl Display for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            State::CheckInterrupts { interrupts } => write!(f, "check_irqs irqs={{{}}}", interrupts),
            State::InjectResetVecJmp { interrupts, reset_vec } => {
                write!(f, "inject_reset_vec_jmp {}, irqs={{{}}}", reset_vec, interrupts)
            }
            State::Fetch { interrupts } => write!(f, "fetch irqs={{{}}}", interrupts),
            State::Decode { interrupts } => write!(f, "decode irqs={{{}}}", interrupts),
            State::DecodedInstr { interrupts, next_instr } => {
                write!(f, "decoded {}, irqs={{{}}}", next_instr, interrupts)
            }
            State::ExecInstr { instr, remaining_cycles, interrupts } => {
                write!(f, "exec[{} cycles left] {} irqs={{{}}}", remaining_cycles, instr, interrupts)
            }
            State::ExecInterruptStackInstrs { remaining_cycles, interrupts, reset_vec } => write!(
                f,
                "exec_interrupt_stack_instrs[{} cycles left] reset_vec={} irqs={{{}}}",
                remaining_cycles, reset_vec, interrupts
            ),
        }
    }
}

use std::collections::HashSet;

pub struct Debugger {
    cycles: u64,
    pc_bps: HashSet<u16>,
    ea_bps: HashSet<u16>,
    break_after_next_decode: bool,
    instr_logging_enabled: bool,
    break_on_brk: bool,
    vic: R2C<dyn RasterBreakpointBackend>,
}

impl Debugger {
    pub fn new(vic: R2C<dyn RasterBreakpointBackend>) -> Self {
        Debugger {
            cycles: 0,
            pc_bps: HashSet::default(),
            ea_bps: HashSet::default(),
            break_after_next_decode: false,
            instr_logging_enabled: false,
            break_on_brk: false,
            vic,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum DebuggerPostDecodePreApplyCbAction {
    DoCycle,
    BreakToDebugPrompt,
}

impl Debugger {
    pub fn cycles(&self) -> u64 {
        self.cycles
    }
    pub fn update_cycles(&mut self, cycles: u64) {
        self.cycles = cycles;
    }
    pub fn set_instr_logging_enabled(&mut self, enabled: bool) {
        self.instr_logging_enabled = enabled;
    }
    pub fn break_after_next_decode(&mut self) {
        self.break_after_next_decode = true;
    }
    pub fn add_pc_breakpoint(&mut self, pc: u16) {
        self.pc_bps.insert(pc);
    }
    pub fn del_pc_breakpoint(&mut self, pc: u16) {
        self.pc_bps.remove(&pc);
    }

    pub fn add_ea_breakpoint(&mut self, ea: u16) {
        self.ea_bps.insert(ea);
    }
    pub fn del_ea_breakpoint(&mut self, ea: u16) {
        self.ea_bps.remove(&ea);
    }

    pub fn pc_breakpoints(&self) -> Vec<u16> {
        self.pc_bps.iter().cloned().collect()
    }

    pub fn ea_breakpoints(&self) -> Vec<u16> {
        self.ea_bps.iter().cloned().collect()
    }

    pub fn set_break_on_brk(&mut self, enabled: bool) {
        self.break_on_brk = enabled;
    }

    fn post_decode_pre_apply_cb(&mut self, mos: &MOS6510) -> DebuggerPostDecodePreApplyCbAction {
        if self.instr_logging_enabled {
            println!("INSTRLOG: {} REG: {}", mos.state(), mos.reg()) // FIXME to DebuggerUI
        }
        if self.break_on_brk
            && (mos.state.decoded_instr().unwrap().op() == crate::mos6510::instr::Op::BRK
                || mos.reg.p.contains(Flags::BRK))
        {
            return DebuggerPostDecodePreApplyCbAction::BreakToDebugPrompt;
        }
        if self.break_after_next_decode || self.pc_bps.contains(&mos.reg.pc) {
            self.break_after_next_decode = false;
            DebuggerPostDecodePreApplyCbAction::BreakToDebugPrompt
        } else {
            DebuggerPostDecodePreApplyCbAction::DoCycle
        }
    }
    fn ea_cb(&mut self, ea: u16, mos: &MOS6510) {
        if self.ea_bps.contains(&ea) {
            self.break_after_next_decode = true; // FIXME hacky
        }
    }
}

pub enum DebuggerMOSMutation {
    InjectInstr(Instr),
    ExecInstr(Instr),
    SetPC(u16),
}

pub trait DebuggerUI {
    fn handle_post_decode_pre_apply_action(
        &mut self,
        action: DebuggerPostDecodePreApplyCbAction,
        mos: &MOS6510,
        vic: &mut dyn RasterBreakpointBackend,
    ) -> Option<DebuggerMOSMutation>;
}

/// Shared functions of MOS6510 and InstrMatchArgs
trait StackHelper {
    fn mem(&mut self) -> &mut MemoryView;
    fn reg(&mut self) -> &mut Regs;

    fn push(&mut self, val: u8) {
        let sp = self.reg().sp_abs();
        self.mem().write(sp, val);
        self.reg().sp -= 1;
    }

    fn push_u16(&mut self, val: u16) {
        self.reg().sp -= 1;
        let sp = self.reg().sp_abs();
        self.mem().write_u16(sp, val);
        self.reg().sp -= 1;
    }

    fn pull(&mut self) -> u8 {
        self.reg().sp += 1;
        let sp = self.reg().sp_abs();
        self.mem().read(sp)
    }

    fn pull_u16(&mut self) -> u16 {
        self.reg().sp += 1;
        let sp = self.reg().sp_abs();
        let val = self.mem().read_u16(sp);
        self.reg().sp += 1;
        val
    }
}

impl StackHelper for MOS6510 {
    fn mem(&mut self) -> &mut MemoryView {
        &mut self.mem
    }
    fn reg(&mut self) -> &mut Regs {
        &mut self.reg
    }
}

impl MOS6510 {
    pub fn new(
        areas: Areas,
        ram: Rc<RefCell<RAM>>,
        debugger: R2C<Debugger>,
        debugger_ui: R2C<dyn DebuggerUI>,
        keyboard_emulator: R2C<EmulatedKeyboard>,
    ) -> Self {
        let mem = MemoryView::new(areas, ram.clone());
        let reg = Regs::default();
        MOS6510 {
            mem,
            ram,
            reg,
            state: State::InjectResetVecJmp {
                interrupts: InterruptPending::default(),
                reset_vec: ResetVec::RESET,
            },
            debugger,
            debugger_ui,
            keyboard_emulator,
        }
    }

    pub fn ram(&self) -> R2C<RAM> {
        self.ram.clone()
    }

    pub fn reset_pc_to(&mut self, addr: u16) {
        self.reg.pc = addr;
    }

    pub fn inject_instr_on_next_fetch(&mut self, i: Instr) {
        self.state.interrupts().inject_instr = Some(i);
    }

    pub fn cycle(&mut self, irq: Option<Interrupt>, nmi: Option<Interrupt>) {
        // record interrupt now, deliver it next time we are in state CheckInterrupts (i.e. next instr)
        let cycle_interrupt_pending = {
            let mut ip = InterruptPending::default();
            ip.irq = !self.reg.p.contains(Flags::IRQD) && irq.is_some();
            ip.nmi = nmi.is_some();
            ip
        };
        if cycle_interrupt_pending.nmi {
            unimplemented!()
        }
        self.state.interrupts().update_merge(&cycle_interrupt_pending);

        let mut instrbuf = [0 as u8; 3];
        let next_instr = loop {
            match self.state {
                State::ExecInstr { ref mut remaining_cycles, ref mut interrupts, .. } => {
                    *remaining_cycles = remaining_cycles.saturating_sub(1);
                    if *remaining_cycles == 0 {
                        self.state = State::CheckInterrupts { interrupts: *interrupts };
                        continue;
                    }
                    return;
                }
                State::ExecInterruptStackInstrs { ref mut remaining_cycles, interrupts, reset_vec } => {
                    *remaining_cycles = remaining_cycles.saturating_sub(1);
                    if *remaining_cycles == 0 {
                        self.state = State::InjectResetVecJmp { interrupts, reset_vec };
                    }
                    return;
                }
                State::InjectResetVecJmp { interrupts, reset_vec } => {
                    let vector = reset_vec as u16;
                    instrbuf = [0x6C, (vector & 0xFF) as u8, (vector >> 8) as u8];
                    // Careful: self.mem[self.reg.pc] is now inequal to instrbuf! (Might look
                    // confusing in debugger)
                    self.state = State::Decode { interrupts };
                }
                State::CheckInterrupts { interrupts } => {
                    if self.reg.p.contains(Flags::IRQD) {
                        self.state = State::Fetch { interrupts: InterruptPending::default() };
                        return;
                    }
                    let mut interrupts = interrupts;
                    match interrupts.ack_highest_prio() {
                        InterruptAckResult::ResetVec(reset_vec) => {
                            // https://www.c64-wiki.de/wiki/IRQ
                            // println!("irq raised: {}", self.reg());
                            self.push_u16(self.reg.pc);
                            self.push(self.reg.p.bits());
                            self.reg.p.set(Flags::IRQD, true);
                            // "Next, the CPU takes 7 cycles to store its return address and processor status."
                            //  --https://codebase64.org/doku.php?id=base:double_irq_explained
                            let remaining_cycles = 7;
                            self.state =
                                State::ExecInterruptStackInstrs { remaining_cycles, interrupts, reset_vec };
                        }
                        InterruptAckResult::InjectInstr(i) => {
                            self.state =
                                State::DecodedInstr { next_instr: i, interrupts: *self.state.interrupts() };
                        }
                        InterruptAckResult::None => {
                            self.state = State::Fetch { interrupts };
                        }
                    }
                }
                State::Fetch { interrupts } => {
                    self.mem.read_one_to_three(self.reg.pc, &mut instrbuf[..]);
                    self.state = State::Decode { interrupts };
                }
                State::Decode { .. } => {
                    let (next_instr, _) = match instr::decode_instr(&instrbuf[..]) {
                        Err(e) => panic!(
                            "instruction decode error: {:?}\nregs: {}\nstack:\n\t{}",
                            e,
                            self.reg,
                            self.dump_stack_lines(true).join("\n\t")
                        ),
                        Ok(instr) => instr,
                    };
                    self.state = State::DecodedInstr { next_instr, interrupts: *self.state.interrupts() };
                }
                State::DecodedInstr { next_instr, .. } => {
                    break next_instr;
                }
            }
        };

        // debugger callback
        let action = self.debugger.borrow_mut().post_decode_pre_apply_cb(&self);
        let vic = self.debugger.borrow_mut().vic.clone();
        assert!(self.debugger.try_borrow().is_ok());
        let mos_mutation = match action {
            DebuggerPostDecodePreApplyCbAction::DoCycle => None,
            DebuggerPostDecodePreApplyCbAction::BreakToDebugPrompt => self
                .debugger_ui
                .borrow_mut()
                .handle_post_decode_pre_apply_action(action, &self, &mut *vic.borrow_mut()),
        };

        // apply debugger action if necessary, may exit early
        match mos_mutation {
            Some(DebuggerMOSMutation::ExecInstr(i)) => {
                self.apply_instr(i, Some(self.reg.pc)); // don't move instruction pointer
                match self.state {
                    State::ExecInstr { ref remaining_cycles, .. } => {
                        println!("skipping {} remaining cycles, back to debug prompt", remaining_cycles);
                    }
                    s => panic!("apply_instr set invalid state {:?}", s),
                }
                return;
            }
            Some(DebuggerMOSMutation::InjectInstr(i)) => self.inject_instr_on_next_fetch(i),
            Some(DebuggerMOSMutation::SetPC(pc)) => {
                self.reg.pc = pc;
                self.state = State::Fetch { interrupts: *self.state.interrupts() };
                return;
            }
            None => (),
        };

        // hot path
        self.apply_instr(next_instr, self.reg.pc.checked_add(next_instr.len() as u16));
        match self.state {
            State::ExecInstr { ref mut remaining_cycles, .. } => {
                assert!(
                    *remaining_cycles >= 2,
                    "remaining_cycles = {} (minimum cycle count is 2)",
                    remaining_cycles
                );
                // we already did one cycle in apply_instr
                *remaining_cycles -= 1;
            }
            s => panic!("apply_instr set invalid state {:?}", s),
        }
    }

    pub fn debugger_refmut(&self) -> std::cell::RefMut<'_, Debugger> {
        self.debugger.borrow_mut()
    }

    pub fn dump_stack_lines(&self, from_sp_upward: bool) -> Vec<String> {
        Vec::from_iter(
            self.copy_stack(from_sp_upward)
                .into_iter()
                .map(|(addr, val)| format!("0x{:04x} = 0x{:02x}", addr, val)),
        )
    }

    pub fn copy_stack(&self, from_sp_upward: bool) -> Vec<(u16, u8)> {
        let lower = if from_sp_upward { self.reg.sp_abs() } else { STACK_BOTTOM };
        Vec::from_iter((lower..=STACK_TOP).rev().map(|addr| (addr, self.mem.read(addr))))
    }

    #[inline]
    // returns (carry_set, u8)
    fn asl(v: u8) -> (bool, u8) {
        let res = (v as u16) << 1;
        (res & 0x100 != 0, (res & 0xff) as u8)
    }

    #[inline]
    // returns (carry_set, u8)
    fn lsr(v: u8) -> (bool, u8) {
        (v & 0x1 != 0, v >> 1)
    }

    #[inline]
    fn rol(v: u8, carry: bool) -> (bool, u8) {
        let res = (v as u16) << 1;
        (res & 0x100 != 0, ((res & 0xff) as u8) | (carry as u8))
    }

    #[inline]
    fn ror(v: u8, carry: bool) -> (bool, u8) {
        let carry_out = v & 0x1 != 0;
        let val = (((v as u16) >> 1) | ((carry as u16) << 7)) as u8;
        (carry_out, val)
    }

    fn apply_instr(&mut self, instr: Instr, mut next_pc: Option<u16>) {
        use instr::Addr::*;
        use instr::Op::*;
        macro_rules! ea {
            ($base:expr, $effective:expr) => {{
                Some(AddrCalcVars { base: $base, effective: $effective })
            }};
        }

        let effective_addr: Option<AddrCalcVars> = {
            match instr.1 {
                Imp | Acc => None,
                Imm(_) => None,
                Zpi(v) => ea!(v as u16, v as u16),
                ZpX(v) => ea!(v as u16, v.overflowing_add(self.reg.x).0 as u16),
                ZpY(v) => ea!(v as u16, v.overflowing_add(self.reg.y).0 as u16),
                PCr(_) => None, // TODO 2 byte offset?
                Abs(v) => ea!(v, v),
                AbX(v) => ea!(v, v.overflowing_add(self.reg.x as u16).0 as u16),
                AbY(v) => ea!(v, v.overflowing_add(self.reg.y as u16).0 as u16),
                Ind(v) => ea!(v, self.mem.read_u16(v)),
                IzX(i) => {
                    // http://archive.6502.org/datasheets/mos_6501-6505_mpu_preliminary_aug_1975.pdf
                    // The second byte of the instruction is added to the contents of the X index register,
                    // discarding the carry.
                    let (effective_zp_addr, _) = i.overflowing_add(self.reg.x);
                    // The result of this addition points to a memory location on a page zero whose contents is the low order eight bits of the effective address.
                    // The next memory location in page zero contains the high orer eight bits of the effective address.
                    // Both memory locations specifying the high and low order bytes of the effective address must be in page zero.
                    let effective_addr = if std::u8::MAX - effective_zp_addr < 1 {
                        unimplemented!() // higher byte on addr 0?
                    } else {
                        self.mem.read_u16(effective_zp_addr as u16)
                    };
                    ea!(effective_addr, effective_addr) // indirectX not affected by page boundaries
                }
                IzY(i) => {
                    // INDIRECT INDEXED ADDRESSING
                    // In indirect indexed addressing the second byte of the instruction points
                    // to a memory location in page zero.
                    // The contents of this memory location is added to the contents of the Y index register,
                    // the result being the low order eight bits of the effective address.
                    let lower = self.mem.read(i as u16);
                    let (lower_ea, carry) = lower.overflowing_add(self.reg.y);
                    let carry = carry as u8;

                    // The carry from this addition is added to the contents of the next
                    // page zero memory location, the result being the high order eight bits
                    // of the effective address.
                    let next_page_zero_location_value = if std::u8::MAX - i < 1 {
                        unimplemented!() // higher byte on addr 0?
                    } else {
                        self.mem.read((i + 1) as u16)
                    };
                    let (higher_ea, _) = next_page_zero_location_value.overflowing_add(carry);
                    let ea = ((higher_ea as u16) << 8) | (lower_ea as u16);
                    ea!((next_page_zero_location_value as u16) << 8, ea) // indirectY _IS_ affected by page boundaries
                                                                         // => if carry == 0, the CPU can use next_page_zero_location_value as MSB without adding
                                                                         //    carry to it => saves one cycle
                }
            }
        };

        if let Some(ea) = effective_addr {
            self.debugger.borrow_mut().ea_cb(ea.effective, &self);
        }

        let effective_addr_load = effective_addr.map(|a| self.mem.read(a.effective));

        struct InstrMatchArgs<'a> {
            instr: Instr,
            reg: &'a mut Regs,
            mem: &'a mut MemoryView,
            effective_addr: Option<u16>,
            effective_addr_load: Option<u8>,
            next_pc: &'a mut Option<u16>,
        }

        impl<'a> StackHelper for InstrMatchArgs<'a> {
            fn mem(&mut self) -> &mut MemoryView {
                &mut self.mem
            }
            fn reg(&mut self) -> &mut Regs {
                &mut self.reg
            }
        }

        // call to the macro-transformed fn instr_match below
        let (interrupt_pending, branch_outcome) = instr_match(InstrMatchArgs {
            instr,
            reg: &mut self.reg,
            effective_addr: effective_addr.map(|v| v.effective),
            effective_addr_load,
            next_pc: &mut next_pc,
            mem: &mut self.mem,
        });

        self.state.interrupts().update_merge(&interrupt_pending);

        //debug_assert_eq!(self.state, State::BeginInstr);
        self.state = State::ExecInstr {
            remaining_cycles: instr.cycles(effective_addr, branch_outcome),
            instr,
            interrupts: *self.state.interrupts(),
        };

        if let Some(pc) = next_pc {
            self.reg.pc = pc;
        }

        macro_rules! branch {
            ($flag:tt == $flag_set:expr, $pcr_offset:expr, $args:expr, $branch_outcome:ident) => {{
                let args = $args;
                let pcr_offset: i8 = $pcr_offset;
                let flag_set: bool = $flag_set != 0;
                if args.reg.p.contains(Flags::$flag) == flag_set {
                    *args.next_pc = Some(args.reg.pc_r_add(pcr_offset));
                    $branch_outcome = Some(instr::BranchOutcome::Taken);
                } else {
                    $branch_outcome = Some(instr::BranchOutcome::NotTaken);
                }
            }};
        }

        #[gen_instr_match]
        fn instr_match(mut args: InstrMatchArgs) -> (InterruptPending, Option<instr::BranchOutcome>) {
            let mut interrupt_pending = InterruptPending::default();
            let mut branch_outcome = None;

            match args.instr {
                // Sources http://www.obelisk.me.uk/6502/instructions.html
                //         https://www.masswerk.at/6502/6502_instruction_set.html

                /***************** Load/Store Operations ******************/
                // LDA 	Load Accumulator 	N,Z
                Instr(LDA, Imm(i)) => args.reg.lda(i),
                mi!(LDA, Zpi, ZpX, Abs, AbX, AbY, IzX, IzY) => args.reg.lda(args.effective_addr_load.unwrap()),
                // LDX 	Load X Register 	N,Z
                Instr(LDX, Imm(i)) => args.reg.ldx(i),
                mi!(LDX, Zpi, ZpY, Abs, AbY) => args.reg.ldx(args.effective_addr_load.unwrap()),
                // LDY 	Load Y Register 	N,Z
                Instr(LDY, Imm(i)) => args.reg.ldy(i),
                mi!(LDY, Zpi, ZpX, Abs, AbX) => args.reg.ldy(args.effective_addr_load.unwrap()),
                // STA 	Store Accumulator
                mi!(STA, Zpi, ZpX, Abs, AbX, AbY, IzX, IzY) => args.mem.write(args.effective_addr.unwrap(), args.reg.a),
                // STX 	Store X Register
                mi!(STX, Zpi, ZpY, Abs) => args.mem.write(args.effective_addr.unwrap(), args.reg.x),
                // STY 	Store Y Register
                mi!(STY, Zpi, ZpX, Abs) => args.mem.write(args.effective_addr.unwrap(), args.reg.y),

                /***************** Register Transfers ******************/
                // The contents of the X and Y registers can be moved to or from the accumulator, setting the negative (N) and zero (Z) flags as appropriate.

                // TAX 	Transfer accumulator to X 	N,Z
                Instr(TAX, Imp) => args.reg.ldx(args.reg.a),
                // TAY 	Transfer accumulator to Y 	N,Z
                Instr(TAY, Imp) => args.reg.ldy(args.reg.a),
                // TXA 	Transfer X to accumulator 	N,Z
                Instr(TXA, Imp) => args.reg.lda(args.reg.x),
                // TYA 	Transfer Y to accumulator 	N,Z
                Instr(TYA, Imp) => args.reg.lda(args.reg.y),

                /***************** Stack Operations ******************/
                // The 6502 microprocessor supports a 256 byte stack fixed between memory locations $0100 and $01FF.
                // A special 8-bit register, S, is used to keep track of the NEXT FREE BYTE of stack space.
                // Pushing a byte on to the stack causes the value to be stored at the current free location (e.g. $0100,S)
                // AND THEN the stack pointer is post decremented. Pull operations reverse this procedure.
                //
                // The stack register can only be accessed by transferring its value to or from the X register.
                //
                // Its value is automatically modified by push/pull instructions, subroutine calls and returns,
                // interrupts and returns from interrupts.

                // TSX 	Transfer stack pointer to X 	N,Z
                Instr(TSX, Imp) => args.reg.ldx(args.reg.sp),
                // TXS 	Transfer X to stack pointer
                Instr(TXS, Imp) => args.reg.sp = args.reg.x,
                // PHA 	Push accumulator on stack
                Instr(PHA, Imp) => {
                    args.push(args.reg.a)
                },
                // PHP 	Push processor status on stack
                Instr(PHP, Imp) => {
                    args.push(args.reg.p.bits())
                },
                // PLA 	Pull accumulator from stack 	N,Z
                Instr(PLA, Imp) => {
                    let acc = args.pull();
                    args.reg.lda(acc)
                }
                // PLP 	Pull processor status from stack 	All
                Instr(PLP, Imp) => {
                    match Flags::from_bits(args.pull()) {
                        Some(p) => args.reg.p = p,
                        None => unimplemented!(), // TODO processor behavior if unused bit is set?
                    }
                }

                /***************** Logical ******************/

                // The following instructions perform logical operations on the contents of the
                // accumulator and another value held in memory. The BIT instruction performs a
                // logical AND to test the presence of bits in the memory value to set the flags but
                // does not keep the result.

                // AND 	Logical AND 	N,Z
                Instr(AND, Imm(i)) => args.reg.lda(args.reg.a & i),
                mi!(AND, Zpi, ZpX, Abs, AbX, AbY, IzX, IzY) => args.reg.lda(args.reg.a & args.effective_addr_load.unwrap()),
                // EOR 	Exclusive OR 	N,Z
                Instr(EOR, Imm(i)) => args.reg.lda(args.reg.a ^ i),
                mi!(EOR, Zpi, ZpX, Abs, AbX, AbY, IzX, IzY) => args.reg.lda(args.reg.a ^ args.effective_addr_load.unwrap()),
                // ORA 	Logical Inclusive OR 	N,Z
                Instr(ORA, Imm(i)) => args.reg.lda(args.reg.a | i),
                mi!(ORA, Zpi, ZpX, Abs, AbX, AbY, IzX, IzY) => args.reg.lda(args.reg.a | args.effective_addr_load.unwrap()),
                // BIT 	Bit Test 	N,V,Z
                mi!(BIT, Zpi, Abs) => {
                    let v = args.effective_addr_load.unwrap();
                    args.reg.p.set(Flags::NEG, v & (1<<7) != 0);
                    args.reg.p.set(Flags::OVFL, v & (1<<6) != 0);
                    args.reg.p.set(Flags::ZERO, (args.reg.a & v) == 0);
                }

                /***************** Arithmetic ******************/
                // The arithmetic operations perform addition and subtraction on the contents of the accumulator.
                // The compare operations allow the comparison of the accumulator and X or Y with memory values.

                // ADC 	Add with Carry 	N,V,Z,C
                Instr(ADC, Imm(i)) => args.reg.add_to_a_with_carry_and_set_carry(i),
                mi!(ADC, Zpi, ZpX, Abs, AbX, AbY, IzX, IzY) => args.reg.add_to_a_with_carry_and_set_carry(args.effective_addr_load.unwrap()),
                // SBC 	Subtract with Carry 	N,V,Z,C
                Instr(SBC, Imm(i)) => args.reg.sub_from_a_with_carry_and_set_carry(i),
                mi!(SBC, Zpi, ZpX, Abs, AbX, AbY, IzX, IzY) => args.reg.sub_from_a_with_carry_and_set_carry(args.effective_addr_load.unwrap()),
                // CMP 	Compare accumulator 	N,Z,C
                Instr(CMP, Imm(i)) => args.reg.cmp_a(i),
                mi!(CMP, Zpi, ZpX, Abs, AbX, AbY, IzX, IzY) => args.reg.cmp_a(args.effective_addr_load.unwrap()),
                // CPX 	Compare X register 	N,Z,C
                Instr(CPX, Imm(i)) => args.reg.cmp_x(i),
                mi!(CPX, Zpi, Abs) => args.reg.cmp_x(args.effective_addr_load.unwrap()),
                // CPY 	Compare Y register 	N,Z,C
                Instr(CPY, Imm(i)) => args.reg.cmp_y(i),
                mi!(CPY, Zpi, Abs) => args.reg.cmp_y(args.effective_addr_load.unwrap()),

                /***************** Increments & Decrements ******************/
                // Increment or decrement a memory location or one of the X or Y registers by one setting
                // the negative (N) and zero (Z) flags as appropriate,

                // INC 	Increment a memory location 	N,Z
                mi!(INC, Zpi, ZpX, Abs, AbX) => {
                    let res = args.effective_addr_load.unwrap().overflowing_add(1).0;
                    args.mem.write(args.effective_addr.unwrap(), res);
                    args.reg.set_nz_flags(res);
                }
                // DEC 	Decrement a memory location 	N,Z
                mi!(DEC, Zpi, ZpX, Abs, AbX) => {
                    let res = args.effective_addr_load.unwrap().overflowing_sub(1).0;
                    args.mem.write(args.effective_addr.unwrap(), res);
                    args.reg.set_nz_flags(res);
                }
                // INX 	Increment the X register 	N,Z
                Instr(INX, Imp) => args.reg.ldx(args.reg.x.overflowing_add(1).0),
                // INY 	Increment the Y register 	N,Z
                Instr(INY, Imp) => args.reg.ldy(args.reg.y.overflowing_add(1).0),
                // DEX 	Decrement the X register 	N,Z
                Instr(DEX, Imp) => args.reg.ldx(args.reg.x.overflowing_sub(1).0),
                // DEY 	Decrement the Y register 	N,Z
                Instr(DEY, Imp) => args.reg.ldy(args.reg.y.overflowing_sub(1).0),

                /***************** Shifts ******************/
                // Shift instructions cause the bits within either a memory location or the accumulator to be
                // shifted by one bit position. The rotate instructions use the contents if the carry flag (C)
                // to fill the vacant position generated by the shift and to catch the overflowing bit.
                // The arithmetic and logical shifts shift in an appropriate 0 or 1 bit as appropriate but catch
                // the overflow bit in the carry flag (C).

                // ASL 	Arithmetic Shift Left 	N,Z,C
                Instr(ASL, Imp) => {
                    let (carry, v) = MOS6510::asl(args.reg.a);
                    args.reg.lda(v);
                    args.reg.set_nzc_flags(v, carry);
                },
                mi!(ASL, Zpi, ZpX, Abs, AbX) => {
                    let (carry, v) = MOS6510::asl(args.effective_addr_load.unwrap());
                    args.mem.write(args.effective_addr.unwrap(), v);
                    args.reg.set_nzc_flags(v, carry);
                },
                // LSR 	Logical Shift Right 	N,Z,C
                Instr(LSR, Imp) => {
                    let (carry, v) = MOS6510::lsr(args.reg.a);
                    args.reg.lda(v);
                    args.reg.set_nzc_flags(v, carry);
                    args.reg.p.set(Flags::NEG, false);
                },
                mi!(LSR, Zpi, ZpX, Abs, AbX) => {
                    let (carry, v) = MOS6510::lsr(args.effective_addr_load.unwrap());
                    args.mem.write(args.effective_addr.unwrap(), v);
                    args.reg.set_nzc_flags(v, carry);
                    args.reg.p.set(Flags::NEG, false);
                },
                // ROL 	Rotate Left 	N,Z,C
                Instr(ROL, Imp) => {
                    let (carry, res) = MOS6510::rol(args.reg.a, args.reg.p.contains(Flags::CARRY));
                    args.reg.lda(res);
                    args.reg.set_nzc_flags(res, carry);
                }
                mi!(ROL, Zpi, ZpX, Abs, AbX) => {
                    let (carry, res) = MOS6510::rol(args.effective_addr_load.unwrap(), args.reg.p.contains(Flags::CARRY));
                    args.mem.write(args.effective_addr.unwrap(), res);
                    args.reg.set_nzc_flags(res, carry);
                }
                // ROR 	Rotate Right 	N,Z,C
                Instr(ROR, Imp) => {
                    let (carry, res) = MOS6510::ror(args.reg.a, args.reg.p.contains(Flags::CARRY));
                    args.reg.lda(res);
                    args.reg.set_nzc_flags(res, carry);
                }
                mi!(ROR, Zpi, ZpX, Abs, AbX) => {
                    let (carry, res) = MOS6510::ror(args.effective_addr_load.unwrap(), args.reg.p.contains(Flags::CARRY));
                    args.mem.write(args.effective_addr.unwrap(), res);
                    args.reg.set_nzc_flags(res, carry);
                }

                /***************** Jumps & Calls ******************/
                // The following instructions modify the program counter causing a break to normal sequential execution.
                // The JSR instruction pushes the old PC onto the stack before changing it to the new location allowing
                // a subsequent RTS to return execution to the instruction after the call.

                // JMP 	Jump to another location
                Instr(JMP, Ind(a)) => *args.next_pc = Some(args.mem.read_u16(a)),
                Instr(JMP, Abs(a)) => *args.next_pc = Some(a),
                // JSR 	Jump to a subroutine
                Instr(JSR, Abs(a)) => {
                    args.push_u16(args.next_pc.unwrap() - 1);
                    *args.next_pc = Some(a);
                }
                // RTS 	Return from subroutine
                Instr(RTS, Imp) => {
                    *args.next_pc = Some(args.pull_u16().overflowing_add(1).0);
                }

                /***************** Branches ******************/
                // Branch instructions break the normal sequential flow of execution by changing the program counter
                // if a specified condition is met. All the conditions are based on examining a single bit within the
                // processor status.

                // BCC 	Branch if carry flag clear
                Instr(BCC, PCr(o)) => branch!(CARRY == 0, o, args, branch_outcome),
                // BCS 	Branch if carry flag set
                Instr(BCS, PCr(o)) => branch!(CARRY == 1, o, args, branch_outcome),
                // BEQ 	Branch if zero flag set
                Instr(BEQ, PCr(o)) => branch!(ZERO == 1, o, args, branch_outcome),
                // BMI 	Branch if negative flag set
                Instr(BMI, PCr(o)) => branch!(NEG == 1, o, args, branch_outcome),
                // BNE 	Branch if zero flag clear
                Instr(BNE, PCr(o)) => branch!(ZERO == 0, o, args, branch_outcome),
                // BPL 	Branch if negative flag clear
                Instr(BPL, PCr(o)) => branch!(NEG == 0, o, args, branch_outcome),
                // BVC 	Branch if overflow flag clear
                Instr(BVC, PCr(o)) => branch!(OVFL == 0, o, args, branch_outcome),
                // BVS 	Branch if overflow flag set
                Instr(BVS, PCr(o)) => branch!(OVFL == 1, o, args, branch_outcome),

                /***************** Status Flag Changes ******************/
                // The following instructions change the values of specific status flags.

                // CLC 	Clear carry flag 	C
                Instr(CLC, Imp) => args.reg.p.set(Flags::CARRY, false),
                // CLD 	Clear decimal mode flag 	D
                Instr(CLD, Imp) => args.reg.p.set(Flags::DEC, false),
                // CLI 	Clear interrupt disable flag 	I
                Instr(CLI, Imp) => args.reg.p.set(Flags::IRQD, false),
                // CLV 	Clear overflow flag 	V
                Instr(CLV, Imp) => args.reg.p.set(Flags::OVFL, false),
                // SEC 	Set carry flag 	C
                Instr(SEC, Imp) => args.reg.p.set(Flags::CARRY, true),
                // SED 	Set decimal mode flag 	D
                Instr(SED, Imp) => args.reg.p.set(Flags::DEC, true),
                // SEI 	Set interrupt disable flag 	I
                Instr(SEI, Imp) => args.reg.p.set(Flags::IRQD, true),

                /***************** System Functions ******************/
                // The remaining instructions perform useful but rarely used functions.

                // BRK 	Force an interrupt 	B
                Instr(BRK, Imp) => {
                    // https://www.c64-wiki.com/wiki/BRK
                    interrupt_pending.irq = true;
                    // BRK Increases PC by 2, but length of BRK is only 1
                    *args.next_pc = args.next_pc.and_then(|pc| pc.checked_add(1));
                    args.reg.p.insert(Flags::BRK);
                }
                // NOP 	No Operation
                Instr(NOP, Imp) => (),
                // RTI 	Return from Interrupt 	All
                Instr(RTI, Imp) => {
                    args.reg.p = match Flags::from_bits(args.pull()) {
                        Some(f) => f,
                        None => unimplemented!(), // TODO (behaviorwith unset bits?)
                    };
                    *args.next_pc = Some(args.pull_u16());
                    // restore of p implicitly resets BRK
                },


                _ => panic!("unimplemented instruction: {}", args.instr),
            }

            (interrupt_pending, branch_outcome)
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn u8_to_i8() {
        let x: u8 = 255;
        let xi = x as i8;
        assert_eq!(xi, -1);
    }

    #[test]
    fn bool_to_u8() {
        assert_eq!(1 as u8, true as u8);
        assert_eq!(0 as u8, false as u8);
    }

    mod alu {
        use super::super::*;

        #[derive(Debug)]
        struct Case {
            pre_a: u8,
            pre_flags: Flags,
            v: u8,
            post_a: u8,
            post_flags: Flags,
        }

        #[test]
        fn adc() {
            // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
            let tests = [
                Case {
                    pre_a: 0x50,
                    pre_flags: Flags::empty(),
                    v: 0x50,
                    post_a: 0xa0,
                    post_flags: Flags::OVFL | Flags::NEG,
                },
                Case {
                    pre_a: 0x50,
                    pre_flags: Flags::empty(),
                    v: 0xd0,
                    post_a: 0x20,
                    post_flags: Flags::CARRY,
                },
                Case {
                    pre_a: 0xd0,
                    pre_flags: Flags::empty(),
                    v: 0x90,
                    post_a: 0x60,
                    post_flags: Flags::CARRY | Flags::OVFL,
                },
            ];
            for case in &tests {
                println!("running:\n{:#?}", case);
                let mut reg = Regs::default();
                reg.p = case.pre_flags;
                reg.a = case.pre_a;
                reg.add_to_a_with_carry_and_set_carry(case.v);
                assert_eq!(reg.a, case.post_a);
                assert_eq!(reg.p, case.post_flags, "\nis:     {}\nshould: {}", reg.p, case.post_flags)
            }
        }

        #[test]
        fn sdc() {
            // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
            let tests = [
                Case {
                    pre_a: 0x50,
                    pre_flags: Flags::empty(),
                    v: 0xf0,
                    post_a: 0x60,
                    post_flags: Flags::empty(),
                },
                Case {
                    pre_a: 0x50,
                    pre_flags: Flags::empty(),
                    v: 0xb0,
                    post_a: 0xa0,
                    post_flags: Flags::OVFL,
                },
            ];
            for case in &tests {
                println!("running:\n{:#?}", case);
                let mut reg = Regs::default();
                reg.p = case.pre_flags;
                reg.a = case.pre_a;
                reg.sub_from_a_with_carry_and_set_carry(case.v);
                assert_eq!(reg.a, case.post_a);
                assert_eq!(reg.p, case.post_flags, "\nis:     {}\nshould: {}", reg.p, case.post_flags)
            }
        }
    }
}
