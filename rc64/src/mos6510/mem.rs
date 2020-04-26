use crate::ram::RAM;
use crate::utils::R2C;

pub type Areas = enum_map::EnumMap<MemoryAreaKind, R2C<dyn MemoryArea>>;
pub struct MemoryView {
    banking_state: BankingState,
    memory_areas: Areas,
    ram: R2C<RAM>,
}

impl MemoryView {
    pub fn new(memory_areas: Areas, ram: R2C<RAM>) -> Self {
        let banking_state = BankingState::default();
        MemoryView { banking_state, memory_areas, ram }
    }

    pub fn read_u16(&self, addr: u16) -> u16 {
        debug_assert!(std::u16::MAX - addr > 0);
        (self.read(addr) as u16) | ((self.read(addr + 1) as u16) << 8)
    }

    pub fn write_u16(&mut self, addr: u16, val: u16) {
        debug_assert!(std::u16::MAX - addr > 0);
        self.write(addr, (val & 0xff) as u8); // LSB
        self.write(addr + 1, (val >> 8) as u8); // MSB
    }

    pub fn read_one_to_three(&self, addr: u16, out: &mut [u8]) {
        debug_assert!(out.len() == 3);
        out[0] = self.read(addr);
        out[1] = addr.checked_add(1).map(|x| self.read(x)).unwrap_or(0);
        out[2] = addr.checked_add(2).map(|x| self.read(x)).unwrap_or(0);
    }

    pub fn read(&self, addr: u16) -> u8 {
        if addr == 0 {
            return 0b111;
        } else if addr == 1 {
            return self.banking_state.cpu_control_lines.bits();
        }

        for segment in self.banking_state.iter() {
            if let Some(addr) = segment.relative_address(addr) {
                return self.memory_areas[segment.kind].borrow().read(addr);
            }
        }

        self.ram.borrow().read(addr)
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        if addr == 0 {
            assert_eq!(val & 0b111, 0b111);
        } else if addr == 1 {
            self.banking_state.update(CpuControlLines::from_bits_truncate(val));
        }

        for segment in self.banking_state.iter() {
            if let Some(addr) = segment.relative_address(addr) {
                let area = &mut self.memory_areas[segment.kind].borrow_mut();
                match area.write(addr, val) {
                    WriteResult::Wrote => {
                        return;
                    }
                    WriteResult::Ignored => {
                        continue;
                    }
                }
            }
        }

        self.ram.borrow_mut().write(addr, val);
    }
}

#[derive(Clone, Copy, Debug, enum_map::Enum)]
pub enum MemoryAreaKind {
    Unmapped,

    // Roms
    BasicRom,
    KernelRom,

    // I/O Registers
    IO2,
    IO1,
    CIA2,
    CIA1,
    ColorRam,
    SID,
    VIC,

    CharRom,

    CartRomLow,
    CartRomHi,
}

#[derive(Debug)]
pub struct BankingState {
    cpu_control_lines: CpuControlLines,
    banking: Vec<Segment>,
}

bitflags! {
    pub struct CpuControlLines: u8 {
        #[allow(clippy::inconsistent_digit_grouping)]
        const LORAM = 0b00000_001;
        #[allow(clippy::inconsistent_digit_grouping)]
        const HIRAM = 0b00000_010;
        #[allow(clippy::inconsistent_digit_grouping)]
        const CHAREN = 0b00000_100;
    }
}

impl Default for BankingState {
    fn default() -> BankingState {
        let mut b =
            BankingState { cpu_control_lines: CpuControlLines::default(), banking: Vec::with_capacity(20) };
        b.update_banking();
        b
    }
}

impl Default for CpuControlLines {
    fn default() -> Self {
        CpuControlLines::all()
    }
}

impl BankingState {
    pub fn update(&mut self, value: CpuControlLines) {
        if self.cpu_control_lines != value {
            println!("CPU control lines update: {:?} -> {:?}", self.cpu_control_lines, value)
        }
        self.cpu_control_lines = value;
        self.update_banking()
    }

    #[allow(clippy::collapsible_if)]
    #[allow(clippy::cognitive_complexity)]
    fn update_banking(&mut self) {
        self.banking.clear();

        let loram = self.cpu_control_lines.contains(CpuControlLines::LORAM);
        let hiram = self.cpu_control_lines.contains(CpuControlLines::HIRAM);
        let charen = self.cpu_control_lines.contains(CpuControlLines::CHAREN);

        // we don't support cartridges => emulate that a catridge is _not_ plugged in
        // by emulating the pull-up resisitor which puts the pins to default logical 1
        let game = true;
        let exrom = true;

        let combined = {
            let mut v: u16 = 0;
            v |= (exrom as u16) << 4;
            v |= (game as u16) << 3;
            v |= (charen as u16) << 2;
            v |= (hiram as u16) << 1;
            v |= loram as u16;
            v
        };
        debug_assert!(combined < 32);

        macro_rules! push_seg {
            (RAM, $base:expr, $end_incl:expr) => {};
            (IO, $base:expr, $end_incl:expr) => {{
                push_seg!(VIC, 0xd000, 0xd000 + 0x400 - 1);
                push_seg!(SID, 0xd400, 0xd400 + 0x400 - 1);
                push_seg!(ColorRam, 0xd800, 0xd800 + 0x400 - 1); // TODO wasn't this 4096?
                push_seg!(CIA1, 0xdc00, 0xdc00 + 0x100 - 1);
                push_seg!(CIA2, 0xdd00, 0xdd00 + 0x100 - 1);
                push_seg!(IO1, 0xde00, 0xde00 + 0x100 - 1);
                push_seg!(IO2, 0xdf00, 0xdf00 + 0x100 - 1);
            }};
            ($kind:expr, $base:expr, $end_incl:expr ) => {
                debug_assert!($end_incl > $base);
                self.banking.push(Segment { base: $base, len: $end_incl - $base + 1, kind: $kind })
            };
        }
        macro_rules! config {
            ($a_0000_0FFF:tt,
             $a_1000_7FFF:tt,
             $a_8000_9FFF:tt,
             $a_A000_BFFF:tt,
             $a_C000_CFFF:tt,
             $a_D000_DFFF:tt,
             $a_E000_FFFF:tt) => {{
                push_seg!($a_0000_0FFF, 0x0000, 0x0FFF);
                push_seg!($a_1000_7FFF, 0x1000, 0x7FFF);
                push_seg!($a_8000_9FFF, 0x8000, 0x9FFF);
                push_seg!($a_A000_BFFF, 0xA000, 0xBFFF);
                push_seg!($a_C000_CFFF, 0xC000, 0xCFFF);
                push_seg!($a_D000_DFFF, 0xD000, 0xDFFF);
                push_seg!($a_E000_FFFF, 0xE000, 0xFFFF);
            }};
        }

        // ModeTable from https://www.c64-wiki.com/wiki/Bank_Switching#Mode_Table
        use MemoryAreaKind::*;
        match combined {
            0b011_111 => config!(RAM, RAM, RAM, BasicRom, RAM, IO, KernelRom),
            0b011_110 => config!(RAM, RAM, RAM, RAM, RAM, IO, KernelRom),
            0b011_101 => config!(RAM, RAM, RAM, RAM, RAM, IO, RAM),
            0b011_100 => config!(RAM, RAM, RAM, RAM, RAM, RAM, RAM),
            0b011_011 => config!(RAM, RAM, RAM, BasicRom, RAM, CharRom, KernelRom),
            0b011_010 => config!(RAM, RAM, RAM, RAM, RAM, CharRom, KernelRom),
            0b011_001 => config!(RAM, RAM, RAM, RAM, RAM, CharRom, RAM),
            0b011_000 => config!(RAM, RAM, RAM, RAM, RAM, RAM, RAM),
            23 => unreachable!(),
            22 => unreachable!(),
            21 => unreachable!(),
            20 => unreachable!(),
            19 => unreachable!(),
            18 => unreachable!(),
            17 => unreachable!(),
            16 => unreachable!(),
            15 => unreachable!(),
            14 => unreachable!(),
            13 => unreachable!(),
            12 => unreachable!(),
            11 => unreachable!(),
            10 => unreachable!(),
            9 => unreachable!(),
            8 => unreachable!(),
            7 => unreachable!(),
            6 => unreachable!(),
            5 => unreachable!(),
            4 => unreachable!(),
            3 => unreachable!(),
            2 => unreachable!(),
            1 => unreachable!(),
            0 => unreachable!(),
            _ => assert!(combined < 32, "{}", combined),
        }

        assert!(
            {
                let mut banking = self.banking.clone();
                banking.sort_by_key(|seg| seg.base);
                let mut min_next_begin: u32 = 0;
                for seg in banking {
                    debug_assert!(seg.base as u32 >= min_next_begin);
                    min_next_begin = seg.base as u32 + seg.len as u32;
                }
                true
            },
            "segments must not overlap"
        );
    }

    fn iter(&self) -> impl Iterator<Item = &Segment> {
        self.banking.iter()
    }
}

#[derive(Clone, Copy, Debug)]
struct Segment {
    base: u16,
    len: u16,

    kind: MemoryAreaKind,
}

impl Segment {
    fn relative_address(self, absolute_addr: u16) -> Option<u16> {
        absolute_addr.checked_sub(self.base).and_then(|relative_addr| {
            if relative_addr < self.len {
                Some(relative_addr)
            } else {
                None
            }
        })
    }
}

pub trait MemoryArea {
    fn read(&self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, val: u8) -> WriteResult;
}

#[derive(Clone, Copy)]
pub enum WriteResult {
    Wrote,
    Ignored,
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_banking_default() {
        let banking = BankingState::default();
        let segs = banking.iter().collect::<Vec<_>>();
        println!("{:?}", segs);
    }

    struct MockArea(u8, WriteResult);

    impl MemoryArea for MockArea {
        fn read(&self, _addr: u16) -> u8 {
            return self.0;
        }
        fn write(&mut self, _addr: u16, _d: u8) -> WriteResult {
            return self.1;
        }
    }
}
