#[derive(Debug, PartialEq, Eq, ToString, EnumString, Clone, Copy)]
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
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

impl std::fmt::Display for Addr {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Addr::Imp => write!(fmt, "<??implicit??>"),
            Addr::Acc => write!(fmt, "A"),
            Addr::Imm(x) => write!(fmt, "#${:x}", x),
            Addr::Zpi(x) => write!(fmt, "${:x}", x),
            Addr::ZpX(x) => write!(fmt, "${:x},X", x),
            Addr::ZpY(x) => write!(fmt, "${:?},Y", x),
            Addr::PCr(x) => write!(fmt, "r{:+x}", x),
            Addr::Abs(x) => write!(fmt, "${:x}", x),
            Addr::AbX(x) => write!(fmt, "${:x},X", x),
            Addr::AbY(x) => write!(fmt, "${:x},Y", x),
            Addr::Ind(x) => write!(fmt, "(${:x})", x),
            Addr::IzX(x) => write!(fmt, "(${:x}),X", x),
            Addr::IzY(x) => write!(fmt, "(${:x}),Y", x),
        }
    }
}

impl std::fmt::Display for Instr {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "{:?}", self.0)?;
        if self.1 != Addr::Imp {
            write!(fmt, " {}", self.1)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Instr(pub Op, pub Addr);

#[derive(Debug, Eq, PartialEq)]
pub enum DecodeErr {
    InvalidOpcode(u8),
    PeekLength {
        opcode: u8,
        expected_length: u8,
        available_peek: usize,
    },
}

#[allow(clippy::cognitive_complexity)]
pub fn decode_instr(peek: &[u8]) -> Result<(Instr, u8), DecodeErr> {
    debug_assert!(!peek.is_empty());

    use Addr::*;
    use Op::*;

    let mut len: u8 = 1; // opcode

    macro_rules! peek_or_err {
        ($n:expr) => {{
            len += $n;
            if peek.len() < len as usize {
                return Err(DecodeErr::PeekLength {
                    opcode: peek[0],
                    expected_length: len,
                    available_peek: peek.len(),
                });
            }
        }};
    }

    macro_rules! val {
        (u8) => {{
            peek_or_err!(1);
            peek[1]
        }};
        (i8) => {{
            peek_or_err!(1);
            peek[1] as i8
        }};
        (u16) => {{
            peek_or_err!(2);
            (peek[1] as u16) | ((peek[2] as u16) << 8)
        }};
    }

    macro_rules! i {
        ($($op:expr => { $($mode:tt=$opc:expr),* $(,)? });* $(;)?) => {
            match peek[0] {
                $( $($opc => Instr($op, i!(CONV $mode))  ),* ),*,
                x => return Err(DecodeErr::InvalidOpcode(x)),
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
    let i = i! {
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
        };

    Ok((i, len))
}

#[derive(Clone, Copy)]
pub struct AddrCalcVars {
    pub base: u16,
    pub effective: u16,
}

impl AddrCalcVars {
    #[inline]
    const fn crosses_page(self) -> bool {
        (self.base ^ self.effective) & (!0xFF) != 0
    }
}

impl Instr {
    pub fn cycles(&self, acv: Option<AddrCalcVars>) -> usize {
        let (num_cycles, boundary_crossing) = instr_cycles(self);
        let additional_cycles = match (
            boundary_crossing,
            acv.map(|a| a.crosses_page()).unwrap_or(false),
        ) {
            (BoundaryCrossingBehavior::NoAdditionalCycle, _) => 0,
            (BoundaryCrossingBehavior::AddOneCycle, true) => 1,
            (BoundaryCrossingBehavior::AddOneCycle, false) => 0,
        };
        num_cycles + additional_cycles
    }
}

enum BoundaryCrossingBehavior {
    NoAdditionalCycle,
    AddOneCycle,
}

fn instr_cycles(instr: &Instr) -> (usize, BoundaryCrossingBehavior) {
    use BoundaryCrossingBehavior::*;
    match instr {
        // generated using xfrm_timing_table.rs
        Instr(Op::BRK, Addr::Imp) => (7, NoAdditionalCycle),
        Instr(Op::BRK, Addr::Acc) => (7, NoAdditionalCycle),
        Instr(Op::ORA, Addr::IzX(_)) => (6, NoAdditionalCycle),
        Instr(Op::ORA, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::ASL, Addr::Zpi(_)) => (5, NoAdditionalCycle),
        Instr(Op::PHP, Addr::Imp) => (3, NoAdditionalCycle),
        Instr(Op::PHP, Addr::Acc) => (3, NoAdditionalCycle),
        Instr(Op::ORA, Addr::Imm(_)) => (2, NoAdditionalCycle),
        Instr(Op::ASL, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::ASL, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::ORA, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::ASL, Addr::Abs(_)) => (6, NoAdditionalCycle),
        Instr(Op::BPL, Addr::PCr(_)) => (2, AddOneCycle),
        Instr(Op::ORA, Addr::IzY(_)) => (5, AddOneCycle),
        Instr(Op::ORA, Addr::ZpX(_)) => (4, NoAdditionalCycle),
        Instr(Op::ASL, Addr::ZpX(_)) => (6, NoAdditionalCycle),
        Instr(Op::CLC, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::CLC, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::ORA, Addr::AbY(_)) => (4, AddOneCycle),
        Instr(Op::ORA, Addr::AbX(_)) => (4, AddOneCycle),
        Instr(Op::ASL, Addr::AbX(_)) => (7, NoAdditionalCycle),
        Instr(Op::JSR, Addr::Abs(_)) => (6, NoAdditionalCycle),
        Instr(Op::AND, Addr::IzX(_)) => (6, NoAdditionalCycle),
        Instr(Op::BIT, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::AND, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::ROL, Addr::Zpi(_)) => (5, NoAdditionalCycle),
        Instr(Op::PLP, Addr::Imp) => (4, NoAdditionalCycle),
        Instr(Op::PLP, Addr::Acc) => (4, NoAdditionalCycle),
        Instr(Op::AND, Addr::Imm(_)) => (2, NoAdditionalCycle),
        Instr(Op::ROL, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::ROL, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::BIT, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::AND, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::ROL, Addr::Abs(_)) => (6, NoAdditionalCycle),
        Instr(Op::BMI, Addr::PCr(_)) => (2, AddOneCycle),
        Instr(Op::AND, Addr::IzY(_)) => (5, AddOneCycle),
        Instr(Op::AND, Addr::ZpX(_)) => (4, NoAdditionalCycle),
        Instr(Op::ROL, Addr::ZpX(_)) => (6, NoAdditionalCycle),
        Instr(Op::SEC, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::SEC, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::AND, Addr::AbY(_)) => (4, AddOneCycle),
        Instr(Op::AND, Addr::AbX(_)) => (4, AddOneCycle),
        Instr(Op::ROL, Addr::AbX(_)) => (7, NoAdditionalCycle),
        Instr(Op::RTI, Addr::Imp) => (6, NoAdditionalCycle),
        Instr(Op::RTI, Addr::Acc) => (6, NoAdditionalCycle),
        Instr(Op::EOR, Addr::IzX(_)) => (6, NoAdditionalCycle),
        Instr(Op::EOR, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::LSR, Addr::Zpi(_)) => (5, NoAdditionalCycle),
        Instr(Op::PHA, Addr::Imp) => (3, NoAdditionalCycle),
        Instr(Op::PHA, Addr::Acc) => (3, NoAdditionalCycle),
        Instr(Op::EOR, Addr::Imm(_)) => (2, NoAdditionalCycle),
        Instr(Op::LSR, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::LSR, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::JMP, Addr::Abs(_)) => (3, NoAdditionalCycle),
        Instr(Op::EOR, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::LSR, Addr::Abs(_)) => (6, NoAdditionalCycle),
        Instr(Op::BVC, Addr::PCr(_)) => (2, AddOneCycle),
        Instr(Op::EOR, Addr::IzY(_)) => (5, AddOneCycle),
        Instr(Op::EOR, Addr::ZpX(_)) => (4, NoAdditionalCycle),
        Instr(Op::LSR, Addr::ZpX(_)) => (6, NoAdditionalCycle),
        Instr(Op::CLI, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::CLI, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::EOR, Addr::AbY(_)) => (4, AddOneCycle),
        Instr(Op::EOR, Addr::AbX(_)) => (4, AddOneCycle),
        Instr(Op::LSR, Addr::AbX(_)) => (7, NoAdditionalCycle),
        Instr(Op::RTS, Addr::Imp) => (6, NoAdditionalCycle),
        Instr(Op::RTS, Addr::Acc) => (6, NoAdditionalCycle),
        Instr(Op::ADC, Addr::IzX(_)) => (6, NoAdditionalCycle),
        Instr(Op::ADC, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::ROR, Addr::Zpi(_)) => (5, NoAdditionalCycle),
        Instr(Op::PLA, Addr::Imp) => (4, NoAdditionalCycle),
        Instr(Op::PLA, Addr::Acc) => (4, NoAdditionalCycle),
        Instr(Op::ADC, Addr::Imm(_)) => (2, NoAdditionalCycle),
        Instr(Op::ROR, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::ROR, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::JMP, Addr::Ind(_)) => (5, NoAdditionalCycle),
        Instr(Op::ADC, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::ROR, Addr::Abs(_)) => (6, NoAdditionalCycle),
        Instr(Op::BVS, Addr::PCr(_)) => (2, AddOneCycle),
        Instr(Op::ADC, Addr::IzY(_)) => (5, AddOneCycle),
        Instr(Op::ADC, Addr::ZpX(_)) => (4, NoAdditionalCycle),
        Instr(Op::ROR, Addr::ZpX(_)) => (6, NoAdditionalCycle),
        Instr(Op::SEI, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::SEI, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::ADC, Addr::AbY(_)) => (4, AddOneCycle),
        Instr(Op::ADC, Addr::AbX(_)) => (4, AddOneCycle),
        Instr(Op::ROR, Addr::AbX(_)) => (7, NoAdditionalCycle),
        Instr(Op::STA, Addr::IzX(_)) => (6, NoAdditionalCycle),
        Instr(Op::STY, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::STA, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::STX, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::DEY, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::DEY, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::TXA, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::TXA, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::STY, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::STA, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::STX, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::BCC, Addr::PCr(_)) => (2, AddOneCycle),
        Instr(Op::STA, Addr::IzY(_)) => (6, NoAdditionalCycle),
        Instr(Op::STY, Addr::ZpX(_)) => (4, NoAdditionalCycle),
        Instr(Op::STA, Addr::ZpX(_)) => (4, NoAdditionalCycle),
        Instr(Op::STX, Addr::ZpY(_)) => (4, NoAdditionalCycle),
        Instr(Op::TYA, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::TYA, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::STA, Addr::AbY(_)) => (5, NoAdditionalCycle),
        Instr(Op::TXS, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::TXS, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::STA, Addr::AbX(_)) => (5, NoAdditionalCycle),
        Instr(Op::LDY, Addr::Imm(_)) => (2, NoAdditionalCycle),
        Instr(Op::LDA, Addr::IzX(_)) => (6, NoAdditionalCycle),
        Instr(Op::LDX, Addr::Imm(_)) => (2, NoAdditionalCycle),
        Instr(Op::LDY, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::LDA, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::LDX, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::TAY, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::TAY, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::LDA, Addr::Imm(_)) => (2, NoAdditionalCycle),
        Instr(Op::TAX, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::TAX, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::LDY, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::LDA, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::LDX, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::BCS, Addr::PCr(_)) => (2, AddOneCycle),
        Instr(Op::LDA, Addr::IzY(_)) => (5, AddOneCycle),
        Instr(Op::LDY, Addr::ZpX(_)) => (4, NoAdditionalCycle),
        Instr(Op::LDA, Addr::ZpX(_)) => (4, NoAdditionalCycle),
        Instr(Op::LDX, Addr::ZpY(_)) => (4, NoAdditionalCycle),
        Instr(Op::CLV, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::CLV, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::LDA, Addr::AbY(_)) => (4, AddOneCycle),
        Instr(Op::TSX, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::TSX, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::LDY, Addr::AbX(_)) => (4, AddOneCycle),
        Instr(Op::LDA, Addr::AbX(_)) => (4, AddOneCycle),
        Instr(Op::LDX, Addr::AbY(_)) => (4, AddOneCycle),
        Instr(Op::CPY, Addr::Imm(_)) => (2, NoAdditionalCycle),
        Instr(Op::CMP, Addr::IzX(_)) => (6, NoAdditionalCycle),
        Instr(Op::CPY, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::CMP, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::DEC, Addr::Zpi(_)) => (5, NoAdditionalCycle),
        Instr(Op::INY, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::INY, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::CMP, Addr::Imm(_)) => (2, NoAdditionalCycle),
        Instr(Op::DEX, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::DEX, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::CPY, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::CMP, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::DEC, Addr::Abs(_)) => (6, NoAdditionalCycle),
        Instr(Op::BNE, Addr::PCr(_)) => (2, AddOneCycle),
        Instr(Op::CMP, Addr::IzY(_)) => (5, AddOneCycle),
        Instr(Op::CMP, Addr::ZpX(_)) => (4, NoAdditionalCycle),
        Instr(Op::DEC, Addr::ZpX(_)) => (6, NoAdditionalCycle),
        Instr(Op::CLD, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::CLD, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::CMP, Addr::AbY(_)) => (4, AddOneCycle),
        Instr(Op::CMP, Addr::AbX(_)) => (4, AddOneCycle),
        Instr(Op::DEC, Addr::AbX(_)) => (7, NoAdditionalCycle),
        Instr(Op::CPX, Addr::Imm(_)) => (2, NoAdditionalCycle),
        Instr(Op::SBC, Addr::IzX(_)) => (6, NoAdditionalCycle),
        Instr(Op::CPX, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::SBC, Addr::Zpi(_)) => (3, NoAdditionalCycle),
        Instr(Op::INC, Addr::Zpi(_)) => (5, NoAdditionalCycle),
        Instr(Op::INX, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::INX, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::SBC, Addr::Imm(_)) => (2, NoAdditionalCycle),
        Instr(Op::NOP, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::NOP, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::CPX, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::SBC, Addr::Abs(_)) => (4, NoAdditionalCycle),
        Instr(Op::INC, Addr::Abs(_)) => (6, NoAdditionalCycle),
        Instr(Op::BEQ, Addr::PCr(_)) => (2, AddOneCycle),
        Instr(Op::SBC, Addr::IzY(_)) => (5, AddOneCycle),
        Instr(Op::SBC, Addr::ZpX(_)) => (4, NoAdditionalCycle),
        Instr(Op::INC, Addr::ZpX(_)) => (6, NoAdditionalCycle),
        Instr(Op::SED, Addr::Imp) => (2, NoAdditionalCycle),
        Instr(Op::SED, Addr::Acc) => (2, NoAdditionalCycle),
        Instr(Op::SBC, Addr::AbY(_)) => (4, AddOneCycle),
        Instr(Op::SBC, Addr::AbX(_)) => (4, AddOneCycle),
        Instr(Op::INC, Addr::AbX(_)) => (7, NoAdditionalCycle),
        _ => panic!("invalid instruction {:?}", instr),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub const BASIC_ROM: &'static [u8] = include_bytes!("../../rsrc/basic_rom.img");

    #[test]
    fn test_dec_basic_rom() {
        // https://www.pagetable.com/c64disasm/
        //
        // => starts at 0xa000, subtract it within the loo
        //
        // NOTE: the website shows some absolute addresses which are in fact encoded as PCrelative

        use maplit::hashmap;
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

    #[test]
    fn test_page_boundary_crossing() {
        use Addr::*;
        use Op::*;
        // https://www.c64-wiki.com/wiki/LDA
        use AddrCalcVars as ACV;
        assert_eq!(
            4,
            Instr(LDA, AbX(0x1234)).cycles(Some(ACV {
                base: 0x1234,
                effective: 0x1234
            })),
            "no crossing"
        );
        assert_eq!(
            5,
            Instr(LDA, AbX(0x1234)).cycles(Some(ACV {
                base: 0x1234,
                effective: 0x1300
            })),
            "crossing"
        );
    }
}
