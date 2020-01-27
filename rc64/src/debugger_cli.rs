use crate::cia::keyboard::C64Key;
use crate::mos6510;
use crate::vic20::RasterBreakpointBackend;
use lazy_static::lazy_static;
use regex::Regex;
use std::str::FromStr;
use std::time;

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
    fn do_loop(
        &mut self,
        mos: &mos6510::MOS6510,
        vic: &mut dyn RasterBreakpointBackend,
    ) -> Option<mos6510::DebuggerMOSMutation> {
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
                static ref RASTER_BREAKPOINT_OP_RE: Regex =
                    Regex::new(r"^([bd])\s+rst\s+([\d]+|\*)").unwrap();
            }
            let breakpoint_op = BREAKPOINT_OP_RE.captures(&line);
            let raster_breakpoint_op = RASTER_BREAKPOINT_OP_RE.captures(&line);
            match line.as_str() {
                "h" | "help" => println!(
                    r#"commands:
h | help                show this help menu
c                       continue
b                       show pc & ea breakpoints (hex-encoded)
b pc|ea HEXADDR         add pc/ea breakpoint add hex-encoded addr
d pc|ea HEXADDR         del pc/ea breakpoint add hex-encoded addr
b rst N|*               break when raster beam reaches beginning of line N, N may be '*'
d rst N|*               delete raster beam breakpoint
info                    MOS dump
stack sp                dump stack from sp upward
stack all               dump entire stack page
instrlog on|off         enable instruciton logging to console
brk on|off              trap to debugger on BRK instr (doesn't affect handling of BRK)
readmem HEXADDR         read memory at address
inject HEX [HEX [HEX]]  inject raw instruction on next fetch cycle
exec   HEX [HEX [HEX]]  exe raw instruction now
press KEY ...           emulate simultaneous press of keys
beam on|off             Highlight raster beam position
"#
                ),
                "c" => {
                    return None;
                }
                "exit" => {
                    std::process::exit(1); // FIXME: should report this back as a result
                }
                "s" => {
                    mos.debugger_refmut().break_after_next_decode();
                    return None;
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
                _ if raster_breakpoint_op.is_some() => {
                    let (opc, line) = raster_breakpoint_op
                        .map(|c| (c.get(1).unwrap().as_str(), c.get(2).unwrap().as_str()))
                        .unwrap();

                    match (opc, usize::from_str(line)) {
                        ("b", Ok(line)) => vic.add_raster_breakpoint(line),
                        ("b", Err(_)) if line == "*" => vic.break_on_every_raster_line(true),
                        ("d", Ok(line)) => vic.remove_raster_breakpoint(line),
                        ("d", Err(_)) if line == "*" => vic.break_on_every_raster_line(false),
                        (_, Err(err)) => println!("{}", err),
                        (_, Ok(_)) => unreachable!(),
                    }
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
                x if x.starts_with("inject ") || x.starts_with("exec ") => {
                    let rem = x.split(" ").collect::<Vec<_>>();
                    if rem.len() < 2 {
                        println!("at least the opcode, please!");
                        continue;
                    }
                    if rem.len() > 4 {
                        println!("at most 3 bytes");
                        continue;
                    }
                    let bytes: Result<Vec<u8>, _> =
                        rem.iter().skip(1).map(|byte| u8::from_str_radix(byte, 16)).collect();
                    let mut bytes = match bytes {
                        Ok(b) => b,
                        Err(e) => {
                            println!("at least one byte not hex: {:?}", e);
                            continue;
                        }
                    };
                    let prepadlen = bytes.len();
                    let padding = vec![0; 3 - bytes.len()];
                    bytes.extend(padding);
                    let (instr, len) = match mos6510::instr::decode_instr(&bytes[..]) {
                        Ok((i, l)) => (i, l),
                        Err(e) => {
                            println!("instruction decode error {:?}", e);
                            continue;
                        }
                    };
                    assert_eq!(len, prepadlen as u8);
                    println!("injecting instruction {:?}", instr);
                    let mutation = match rem[0] {
                        "inject" => mos6510::DebuggerMOSMutation::InjectInstr(instr),
                        "exec" => mos6510::DebuggerMOSMutation::ExecInstr(instr),
                        x => unreachable!("{:?}", x),
                    };
                    return Some(mutation);
                }
                x if x.starts_with("press ") => {
                    let mut keys = x.split(" ").collect::<Vec<_>>();
                    keys.remove(0);
                    let keys = {
                        let keys: Result<Vec<_>, _> = keys.into_iter().map(C64Key::from_str).collect();
                        match keys {
                            Ok(keys) => keys,
                            Err(err) => {
                                println!("Invalid key: {}", err);
                                continue;
                            }
                        }
                    };

                    mos.keyboard_emulator
                        .borrow_mut()
                        .enqueue_key_event(time::Duration::from_millis(250), keys);
                }
                x if x.starts_with("loadtomem ") => {
                    let comps = x.split(" ").collect::<Vec<_>>();
                    let (addr, path) = {
                        if comps.len() != 3 {
                            println!("invalid argument");
                            continue;
                        } else {
                            (comps[1], comps[2])
                        }
                    };
                    let addr = match u16::from_str_radix(addr, 16) {
                        Err(e) => {
                            println!("invalid address {:?}: {:?}", &addr, e);
                            continue;
                        }
                        Ok(addr) => addr,
                    };
                    let data = match std::fs::read(path) {
                        Ok(d) => d,
                        Err(e) => {
                            println!("{:?}", e);
                            continue;
                        }
                    };
                    println!("loading {} bytes to addr 0x{:04x}", data.len(), addr);
                    let ram = mos.ram();
                    let mut ram = ram.borrow_mut();
                    for (i, b) in data.iter().enumerate() {
                        ram.write(addr + (i as u16), *b);
                    }
                }
                x if x.starts_with("setpc ") => {
                    let comps = x.split(" ").collect::<Vec<_>>();
                    let addr = {
                        if comps.len() != 2 {
                            println!("invalid argument");
                            continue;
                        } else {
                            comps[1]
                        }
                    };
                    let addr = match u16::from_str_radix(addr, 16) {
                        Err(e) => {
                            println!("invalid address {:?}: {:?}", &addr, e);
                            continue;
                        }
                        Ok(addr) => addr,
                    };
                    return Some(mos6510::DebuggerMOSMutation::SetPC(addr));
                }
                x if x.starts_with("beam ") => match x.split(" ").collect::<Vec<_>>().get(1) {
                    Some(&"on") => vic.highlight_raster_beam(true),
                    Some(&"off") => vic.highlight_raster_beam(false),
                    Some(arg) => {
                        println!("Invalid arg {:?}", arg);
                        continue;
                    }
                    _ => {
                        println!("Missing arg");
                        continue;
                    }
                },
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
        vic: &mut dyn RasterBreakpointBackend,
    ) -> Option<mos6510::DebuggerMOSMutation> {
        assert_eq!(action, mos6510::DebuggerPostDecodePreApplyCbAction::BreakToDebugPrompt);
        self.do_loop(mos, vic)
    }
}
