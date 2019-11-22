use crate::ram::RAM;
use crate::utils::R2C;
use bit_vec::BitVec;

pub type Areas = enum_map::EnumMap<MemoryAreaKind, R2C<dyn MemoryArea>>;
pub struct MemoryView {
    banking_state: BankingState,
    memory_areas: Areas,
    ram: R2C<RAM>,
}

impl MemoryView {
    pub fn new(memory_areas: Areas, ram: R2C<RAM>) -> Self {
        let banking_state = BankingState::default();
        MemoryView {
            banking_state,
            memory_areas,
            ram,
        }
    }

    pub fn read_u16(&self, addr: u16) -> u16 {
        debug_assert!(std::u16::MAX - addr > 0);
        (self.read(addr) as u16) | ((self.read(addr + 1) as u16) << 8)
    }

    pub fn write_u16(&mut self, addr: u16, val: u16) {
        debug_assert!(std::u16::MAX - addr > 0);
        self.write(addr, (val & !(8 - 1)) as u8);
        self.write(addr + 1, (val >> 8) as u8);
    }

    pub fn read_one_to_three(&self, addr: u16, out: &mut [u8]) {
        debug_assert!(out.len() == 3);
        out[0] = self.read(addr);
        out[1] = addr.checked_add(1).map(|x| self.read(x)).unwrap_or(0);
        out[2] = addr.checked_add(2).map(|x| self.read(x)).unwrap_or(0);
    }

    pub fn read(&self, addr: u16) -> u8 {
        if addr == 0 {
            return self.banking_state.cpu_control_lines;
        } else if addr == 1 {
            return self.banking_state.expansion_port;
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
            self.banking_state
                .update(BankingStateUpdate::CpuControlLines(val))
        } else if addr == 1 {
            self.banking_state
                .update(BankingStateUpdate::ExpansionPort(val))
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
}

struct BankingState {
    cpu_control_lines: u8,
    expansion_port: u8,
    banking: Vec<Segment>,
}

enum BankingStateUpdate {
    CpuControlLines(u8),
    ExpansionPort(u8),
}

impl Default for BankingState {
    fn default() -> BankingState {
        let mut b = BankingState {
            cpu_control_lines: 0b00_00_01_11,
            expansion_port: 0b00_00_00_11,
            banking: Vec::with_capacity(20),
        };
        b.update_banking();
        b
    }
}

impl BankingState {
    pub fn update(&mut self, update: BankingStateUpdate) {
        match update {
            BankingStateUpdate::CpuControlLines(value) => {
                self.cpu_control_lines = value;
            }
            BankingStateUpdate::ExpansionPort(value) => {
                self.expansion_port = value;
            }
        }
        self.update_banking()
    }

    #[allow(clippy::collapsible_if)]
    fn update_banking(&mut self) {
        self.banking.clear();

        let bitmap = BitVec::from_bytes(&[self.expansion_port, self.cpu_control_lines]);
        #[allow(clippy::identity_op)]
        let loram = bitmap.get(15 - 0).unwrap();
        let hiram = bitmap.get(15 - 1).unwrap();
        let charen = bitmap.get(15 - 2).unwrap();
        let _game = bitmap.get(15 - 8).unwrap();
        let _exrom = bitmap.get(15 - 9).unwrap();

        macro_rules! push_seg {
            ($kind:expr, $base:expr, $len:expr ) => {
                self.banking.push(Segment {
                    base: $base,
                    len: $len,
                    kind: $kind,
                })
            };
        }
        use MemoryAreaKind::*;
        // https://www.c64-wiki.com/wiki/Bank_Switching#Control_bits
        // BASIC ROM
        if loram {
            push_seg!(BasicRom, 0xa000, 0x2000);
        }
        // KERNAL ROM
        if hiram {
            push_seg!(KernelRom, 0xe000, 0x2000);
        }
        // Deal with I/O | CHAR ROM | RAM
        if !hiram && !loram {
            // charen insignificant, map ram, i.e. no overlays
        } else {
            if charen {
                // I/O devices appear in the CPU address space
                push_seg!(VIC, 0xd000, 0x400);
                push_seg!(SID, 0xd400, 0x400);
                push_seg!(ColorRam, 0xd800, 0x400); // TODO wasn't this 4096?
                push_seg!(CIA1, 0xdc00, 0x100);
                push_seg!(CIA2, 0xdd00, 0x100);
                push_seg!(IO1, 0xde00, 0x100);
                push_seg!(IO2, 0xdf00, 0x100);
            } else {
                push_seg!(CharRom, 0xd000, 0x1000);
            }
        }

        debug_assert!(
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
    fn relative_address(&self, absolute_addr: u16) -> Option<u16> {
        absolute_addr
            .checked_sub(self.base)
            .and_then(|relative_addr| {
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

    #[test]
    fn test_default_memory_view() {
        let areas = enum_map::enum_map! {
            MemoryAreaKind::BasicRom => r2c_new!(MockArea(0, WriteResult::Wrote)) as R2C<dyn MemoryArea>,
            MemoryAreaKind::KernelRom => r2c_new!(MockArea(1, WriteResult::Ignored)) as R2C<dyn MemoryArea>,
            MemoryAreaKind::IO1 => r2c_new!(MockArea(2, WriteResult::Wrote)) as R2C<dyn MemoryArea>,
            MemoryAreaKind::IO2 => r2c_new!(MockArea(3, WriteResult::Wrote)) as R2C<dyn MemoryArea>,
            MemoryAreaKind::CIA2 => r2c_new!(MockArea(4, WriteResult::Wrote)) as R2C<dyn MemoryArea>,
            MemoryAreaKind::CIA1 => r2c_new!(MockArea(5, WriteResult::Wrote)) as R2C<dyn MemoryArea>,
            MemoryAreaKind::ColorRam => r2c_new!(MockArea(6, WriteResult::Wrote)) as R2C<dyn MemoryArea>,
            MemoryAreaKind::SID => r2c_new!(MockArea(7, WriteResult::Wrote)) as R2C<dyn MemoryArea>,
            MemoryAreaKind::VIC => r2c_new!(MockArea(8, WriteResult::Wrote)) as R2C<dyn MemoryArea>,
            MemoryAreaKind::CharRom => r2c_new!(MockArea(9, WriteResult::Wrote)) as R2C<dyn MemoryArea>,
        };
        let ram = r2c_new!(RAM::default());

        let mut mv = MemoryView::new(areas, ram);

        mv.write(0x23, 42);
        assert_eq!(mv.read(0x23), 42);

        // write to vic control registers, these capture
        mv.write(0xd023, 42);
        assert_eq!(mv.read(0xd023), 8); // mock for vic returns 8

        // test writethrough areas (i.e. KERNAL ROM)
        mv.write(0xe023, 42);
        assert_eq!(mv.read(0xe023), 1);
        assert_eq!(mv.ram.borrow().read(0xe023), 42);
        // now unmap kernal rom by unsetting
        mv.write(0x0000, mv.banking_state.cpu_control_lines & !(0x2));
        assert_eq!(mv.read(0xe023), 42);
        assert_eq!(mv.ram.borrow().read(0xe023), 42);
        // now remap kernal rom by setting KERNAL
        mv.write(0x0000, mv.banking_state.cpu_control_lines | (0x2));
        assert_eq!(mv.read(0xe023), 1);
        assert_eq!(mv.ram.borrow().read(0xe023), 42);
    }
}
