use crate::mos6510;
use std::cell::Cell;

pub fn register(self_jmp_pc: u16, debugger: &mut mos6510::Debugger) {
    let mut counter = Cell::new(0);
    debugger.add_pc_breakpoint_conditional(self_jmp_pc, move |mos| {
            use mos6510::instr::{Addr, Instr, Op};
            mos.state()
                .decoded_instr()
                .map(|i| {
                    match i {
                    Instr(Op::JMP, Addr::Abs(a)) if *a == self_jmp_pc => {
                        let ctr = counter.get_mut();
                        *ctr += 1;
                        if *ctr >= 100 {
                            return Some(mos6510::DebuggerPostDecodePreApplyCbAction::StopEmulatorAfterHavingReachedSelfJump)
                        }
                    }
                    _ => (),
                }
                None
                })
                .unwrap_or(None)
        })
}
