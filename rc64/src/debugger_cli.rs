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
                static ref BREAKPOINT_OP_RE: Regex = Regex::new(r"^([bd])\s+([\dxXa-fA-F]+)").unwrap();
            }
            let breakpoint_op = BREAKPOINT_OP_RE.captures(&line);
            match line.as_str() {
                "h" | "help" => println!(
                    r#"commands:
h | help        show this help menu
c               continue
b               show breakpoints (hex-encoded)
b <PCADDR_HEX>  add breakpoint add hex-encoded PC addr
d <PCADDR_HEX>  del breakpoint add hex-encoded PC addr
info            MOS dump
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
                "info" => {
                    println!("{}", status);
                    continue;
                }
                "b" => {
                    let mut bs = mos.debugger_refmut().breakpoints();
                    bs.sort();
                    let bs = bs.into_iter().map(|b| format!("{:x}", b)).collect::<Vec<_>>();
                    println!("breakpoints: {}", bs.join(", "));
                    continue;
                }
                _ if breakpoint_op.is_some() => {
                    let (opc, pc) = breakpoint_op.map(|c| (c.get(1).unwrap().as_str(), c.get(2).unwrap().as_str())).unwrap();
                    let pc: u16 = {
                        let radix = if pc.to_lowercase().chars().all(|ch| ch.is_numeric()) { 10 } else { 16 };
                        match u16::from_str_radix(&pc, radix) {
                            Ok(pc) => pc,
                            Err(e) => {
                                println!("cannot parse PC address {:?}: {:?}", pc, e);
                                continue;
                            }
                        }
                    };
                    let mut dbg = mos.debugger_refmut();
                    match opc {
                        "b" => dbg.add_breakpoint(pc),
                        "d" => dbg.del_breakpoint(pc),
                        _ => unreachable!(),
                    }
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
    fn handle_post_decode_pre_apply_action(&mut self, action: mos6510::DebuggerPostDecodePreApplyCbAction, mos: &mos6510::MOS6510) {
        assert_eq!(action, mos6510::DebuggerPostDecodePreApplyCbAction::BreakToDebugPrompt);
        self.do_loop(mos);
    }
}
