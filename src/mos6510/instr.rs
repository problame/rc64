#[derive(Debug, PartialEq, Eq, ToString, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum Op {
    // Logical and Arithmetic
    ORA,
    AND,
    EOR,
    ADC,
    SBC,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    INC,
    INX,
    INY,
    ASL,
    ROL,
    LSR,
    ROR,

    // Move commands
    LDA,
    STA,
    LDX,
    STX,
    LDY,
    STY,
    TAX,
    TXA,
    TAY,
    TYA,
    TSX,
    TXS,
    PLA,
    PHA,
    PLP,
    PHP,

    // Jump / Flag commands
    BPL,
    BMI,
    BVC,
    BVS,
    BCC,
    BCS,
    BNE,
    BEQ,
    BRK,
    RTI,
    JSR,
    RTS,
    JMP,
    BIT,
    CLC,
    SEC,
    CLD,
    SED,
    CLI,
    SEI,
    CLV,
    NOP,
}

/// http://www.obelisk.me.uk/6502/addressing.html
#[derive(Debug, PartialEq, Eq)]
pub enum Addr {
    Imp, // Implicit
    Acc,
    Imm(u8),
    Zpi(u8),
    ZpX(u8),
    ZpY(u8), // LDX, STX
    PCr(i8), // for branching
    Abs(u16),
    AbX(u16),
    AbY(u16),
    Ind(u16), // 16bit address to another 16bit addr => JMP to *{u16}
    IzX(u8),  // u8 points to LSB of a 16bit addr A on zero page. *{A + X}
    IzY(u8),  // u8 points to LSB of a 16bit addr A on the zero page. *{*{u8, u8+1} + Y}
}

#[derive(Debug, PartialEq, Eq)]
pub struct Instr(pub Op, pub Addr);
type Opcode = u8;

pub fn decode_instr(peek: &[u8]) -> Result<(Instr, usize), Opcode> {
    debug_assert!(peek.len() > 0);

    use Addr::*;
    use Op::*;

    macro_rules! val {
        (u8) => {{
            peek[1]
        }};
        (i8) => {{
            peek[1] as i8
        }};
        (u16) => {{
            (peek[1] as u16) | ((peek[2] as u16) << 8)
        }};
    }

    macro_rules! i {
        ($($op:expr => { $($mode:tt=$opc:expr),* $(,)? });* $(;)?) => {
            match peek[0] {
                $( $($opc => Ok( ( Instr($op, i!(CONV $mode) ), 1) )),* ),*,
                x => Err(x),
            }
        };
        (CONV $mode:tt) => { i!(CONVP $mode) };
        (CONVP Imp) => { Imp };
        (CONVP Acc) => { Acc };
        (CONVP Imm) => { Imm(val!(u8) ) };
        (CONVP Zpi) => { Zpi(val!(u8) ) };
        (CONVP ZpX) => { ZpX(val!(u8) ) };
        (CONVP ZpY) => { ZpY(val!(u8) ) };
        (CONVP PCr) => { PCr(val!(i8) ) };
        (CONVP Abs) => { Abs(val!(u16)) };
        (CONVP AbX) => { AbX(val!(u16)) };
        (CONVP AbY) => { AbY(val!(u16)) };
        (CONVP Ind) => { Ind(val!(u16)) };
        (CONVP IzX) => { IzX(val!(u8) ) };
        (CONVP IzY) => { IzY(val!(u8) ) };
    };

    // http://www.oxyron.de/html/opcodes02.html
    i! {
    ORA => {  Imm=0x09, Zpi=0x05, ZpX=0x15,  IzX=0x01, IzY=0x11, Abs=0x0D, AbX=0x1D, AbY=0x19,   };
    AND => {  Imm=0x29, Zpi=0x25, ZpX=0x35,  IzX=0x21, IzY=0x31, Abs=0x2D, AbX=0x3D, AbY=0x39,   };
    EOR => {  Imm=0x49, Zpi=0x45, ZpX=0x55,  IzX=0x41, IzY=0x51, Abs=0x4D, AbX=0x5D, AbY=0x59,   };
    ADC => {  Imm=0x69, Zpi=0x65, ZpX=0x75,  IzX=0x61, IzY=0x71, Abs=0x6D, AbX=0x7D, AbY=0x79,   };
    SBC => {  Imm=0xE9, Zpi=0xE5, ZpX=0xF5,  IzX=0xE1, IzY=0xF1, Abs=0xED, AbX=0xFD, AbY=0xF9,   };
    CMP => {  Imm=0xC9, Zpi=0xC5, ZpX=0xD5,  IzX=0xC1, IzY=0xD1, Abs=0xCD, AbX=0xDD, AbY=0xD9,   };
    CPX => {  Imm=0xE0, Zpi=0xE4,     Abs=0xEC,     };
    CPY => {  Imm=0xC0, Zpi=0xC4,     Abs=0xCC,     };
    DEC => {   Zpi=0xC6, ZpX=0xD6,    Abs=0xCE, AbX=0xDE,    };
    DEX => { Imp=0xCA,            };
    DEY => { Imp=0x88,            };
    INC => {   Zpi=0xE6, ZpX=0xF6,    Abs=0xEE, AbX=0xFE,    };
    INX => { Imp=0xE8,            };
    INY => { Imp=0xC8,            };
    ASL => { Imp=0x0A,  Zpi=0x06, ZpX=0x16,    Abs=0x0E, AbX=0x1E,    };
    ROL => { Imp=0x2A,  Zpi=0x26, ZpX=0x36,    Abs=0x2E, AbX=0x3E,    };
    LSR => { Imp=0x4A,  Zpi=0x46, ZpX=0x56,    Abs=0x4E, AbX=0x5E,    };
    ROR => { Imp=0x6A,  Zpi=0x66, ZpX=0x76,    Abs=0x6E, AbX=0x7E,    };
    LDA => {  Imm=0xA9, Zpi=0xA5, ZpX=0xB5,  IzX=0xA1, IzY=0xB1, Abs=0xAD, AbX=0xBD, AbY=0xB9,   };
    STA => {   Zpi=0x85, ZpX=0x95,  IzX=0x81, IzY=0x91, Abs=0x8D, AbX=0x9D, AbY=0x99,   };
    LDX => {  Imm=0xA2, Zpi=0xA6,  ZpY=0xB6,   Abs=0xAE,  AbY=0xBE,   };
    STX => {   Zpi=0x86,  ZpY=0x96,   Abs=0x8E,     };
    LDY => {  Imm=0xA0, Zpi=0xA4, ZpX=0xB4,    Abs=0xAC, AbX=0xBC,    };
    STY => {   Zpi=0x84, ZpX=0x94,    Abs=0x8C,     };
    TAX => { Imp=0xAA,            };
    TXA => { Imp=0x8A,            };
    TAY => { Imp=0xA8,            };
    TYA => { Imp=0x98,            };
    TSX => { Imp=0xBA,            };
    TXS => { Imp=0x9A,            };
    PLA => { Imp=0x68,            };
    PHA => { Imp=0x48,            };
    PLP => { Imp=0x28,            };
    PHP => { Imp=0x08,            };
    BPL => {            PCr=0x10, };
    BMI => {            PCr=0x30, };
    BVC => {            PCr=0x50, };
    BVS => {            PCr=0x70, };
    BCC => {            PCr=0x90, };
    BCS => {            PCr=0xB0, };
    BNE => {            PCr=0xD0, };
    BEQ => {            PCr=0xF0, };
    BRK => { Imp=0x00,            };
    RTI => { Imp=0x40,            };
    JSR => {        Abs=0x20,     };
    RTS => { Imp=0x60,            };
    JMP => {        Abs=0x4C,   Ind=0x6C,  };
    BIT => {   Zpi=0x24,     Abs=0x2C,     };
    CLC => { Imp=0x18,            };
    SEC => { Imp=0x38,            };
    CLD => { Imp=0xD8,            };
    SED => { Imp=0xF8,            };
    CLI => { Imp=0x58,            };
    SEI => { Imp=0x78,            };
    CLV => { Imp=0xB8,            };
    NOP => { Imp=0xEA,            };
        }
}

#[cfg(test)]
mod tests {

    pub const BASIC_ROM: &'static [u8] = include_bytes!("../../rsrc/basic_rom.img");

    #[test]
    fn test_dec_basic_rom() {
        // https://www.pagetable.com/c64disasm/
        //
        // => starts at 0xa000, subtract it within the loo
        //
        // NOTE: the website shows some absolute addresses which are in fact encoded as PCrelative

        use super::*;
        use Addr::*;
        use Op::*;
        let tt = hashmap! {
            0xA4CD => Instr(LDA, Zpi(0x5F)),
            0xA4E3 => Instr(INY, Imp),
            0xA4FB => Instr(STA, Zpi(0x5a)),
            0xA437 => Instr(JMP, Ind(0x0300)),
            // TODO
        };
        for (addr, exp) in tt {
            let addr = addr - 0xa000;
            let dec = super::decode_instr(&BASIC_ROM[addr..addr + 4]);
            assert!(dec.is_ok(), "{:?}", dec);
            let (dec, _) = dec.unwrap();
            assert_eq!(dec, exp);
        }
    }
}
