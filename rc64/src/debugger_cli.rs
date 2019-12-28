use crate::mos6510;
use lazy_static::lazy_static;
use regex::Regex;

pub struct DebuggerCli {
    rl: rustyline::Editor<()>,
    last_input: String,
}

impl Default for DebuggerCli {
    fn default() -> Self {
        DebuggerCli { rl: rustyline::Editor::<()>::new(), last_input: "".to_owned() }
    }
}

impl DebuggerCli {
    fn do_loop(&mut self, mos: &mos6510::MOS6510) {
        let mut is_first_iteration = true;

        loop {
            let status = format!("{}\n{}", mos.reg(), mos.state());
            if is_first_iteration {
                println!("{}", status);
            }
            is_first_iteration = false;

            // get input
            let line = self.rl.readline(">> ").expect("readline");
            let line = match line.as_str() {
                "" => self.last_input.clone(),
                _ => line,
            };
            self.last_input = line.clone();

            // parse and dispatch actions
            lazy_static! {
                static ref BREAKPOINT_OP_RE: Regex =
                    Regex::new(r"^([bd])\s+(pc|ea)\s+([\dxXa-fA-F]+)").unwrap();
            }
            let breakpoint_op = BREAKPOINT_OP_RE.captures(&line);
            match line.as_str() {
                "h" | "help" => println!(
                    r#"commands:
h | help            show this help menu
c                   continue
b                   show pc & ea breakpoints (hex-encoded)
b pc|ea HEXADDR     add pc/ea breakpoint add hex-encoded addr
d pc|ea HEXADDR     del pc/ea breakpoint add hex-encoded addr
info                MOS dump
stack sp            dump stack from sp upward
stack all           dump entire stack page
instrlog on|off     enable instruciton logging to console
brk on|off          trap to debugger on BRK instr (doesn't affect handling of BRK)
readmem HEXADDR     read memory at address
                    "#
                ),
                "c" => {
                    return;
                }
                "exit" => {
                    std::process::exit(1); // FIXME: should report this back as a result
                }
                "s" => {
                    mos.debugger_refmut().break_after_next_decode();
                    return;
                }
                x if x.starts_with("stack ") => {
                    let from_sp_upward = match x {
                        "stack sp" => true,
                        "stack all" => false,
                        x => {
                            println!("unknown command {:?}", x);
                            continue;
                        }
                    };
                    println!("{}", mos.dump_stack_lines(from_sp_upward).join("\n"));
                }
                "info" => {
                    println!("{}", status);
                    continue;
                }
                x if x.starts_with("instrlog ") => {
                    let enabled = match x {
                        "instrlog on" => true,
                        "instrlog off" => false,
                        x => {
                            println!("unknown command {:?}", x);
                            continue;
                        }
                    };
                    mos.debugger_refmut().set_instr_logging_enabled(enabled);
                }
                x if x.starts_with("brk ") => {
                    let enabled = match x {
                        "brk on" => true,
                        "brk off" => false,
                        x => {
                            println!("unknown command {:?}", x);
                            continue;
                        }
                    };
                    mos.debugger_refmut().set_break_on_brk(enabled);
                }
                "b" => {
                    let dbg = mos.debugger_refmut();
                    let mut tbl = [("pc", dbg.pc_breakpoints()), ("ea", dbg.ea_breakpoints())];
                    for (name, list) in tbl.iter_mut() {
                        list.sort();
                        let list = list.into_iter().map(|b| format!("{:x}", b)).collect::<Vec<_>>();
                        println!("{} breakpoints: {}", name, list.join(", "));
                    }
                    continue;
                }
                _ if breakpoint_op.is_some() => {
                    let (opc, ea_or_pc, pc) = breakpoint_op
                        .map(|c| {
                            (
                                c.get(1).unwrap().as_str(),
                                c.get(2).unwrap().as_str(),
                                c.get(3).unwrap().as_str(),
                            )
                        })
                        .unwrap();
                    let addr: u16 = {
                        let radix =
                            if pc.to_lowercase().chars().all(|ch| ch.is_numeric()) { 10 } else { 16 };
                        match u16::from_str_radix(&pc, radix) {
                            Ok(pc) => pc,
                            Err(e) => {
                                println!("cannot parse PC address {:?}: {:?}", pc, e);
                                continue;
                            }
                        }
                    };
                    let mut dbg = mos.debugger_refmut();
                    match (opc, ea_or_pc) {
                        ("b", "pc") => dbg.add_pc_breakpoint(addr),
                        ("d", "pc") => dbg.del_pc_breakpoint(addr),
                        ("b", "ea") => dbg.add_ea_breakpoint(addr),
                        ("d", "ea") => dbg.del_ea_breakpoint(addr),
                        _ => unreachable!(),
                    }
                }
                x if x.starts_with("readmem ") => {
                    let comps = x.split(" ").collect::<Vec<_>>();
                    let addr = {
                        if comps.len() != 2 {
                            println!("invalid argument");
                            continue;
                        } else {
                            match u16::from_str_radix(&comps[1], 16) {
                                Err(e) => {
                                    println!("invalid address {:?}: {:?}", &comps[1], e);
                                    continue;
                                }
                                Ok(addr) => addr,
                            }
                        }
                    };
                    let val = mos.mem().read(addr);
                    println!("0x{:04x} = {:02x}", addr, val);
                }
                x => {
                    println!("unknown command: {:?}", x);
                    continue;
                }
            }
        }
    }
}

impl mos6510::DebuggerUI for DebuggerCli {
    fn handle_post_decode_pre_apply_action(
        &mut self,
        action: mos6510::DebuggerPostDecodePreApplyCbAction,
        mos: &mos6510::MOS6510,
    ) {
        assert_eq!(action, mos6510::DebuggerPostDecodePreApplyCbAction::BreakToDebugPrompt);
        self.do_loop(mos);
    }
}
