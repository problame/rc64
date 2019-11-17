fn main() {
    println!("Hello, world!");
}

mod mos6510 {
    mod mem {
        use bit_vec::BitVec;

        struct MemoryView {
            banking_state: BankingState,

            memory_areas: [Box<dyn MemoryArea>; MemoryAreaKind::Num as usize],

            ram: RAM,
        }

        impl MemoryView {
            fn read(&self, addr: u16) -> u8 {
                unimplemented!()
            }

            fn write(&mut self, addr: u16, val: u8) {
                if addr == 0 {
                    self.banking_state
                        .update(BankingStateUpdate::CpuControlLines(val))
                } else if addr == 1 {
                    self.banking_state
                        .update(BankingStateUpdate::ExpansionPort(val))
                }

                for segment in self.banking_state.iter() {
                    if segment.contains(addr) {
                        match self.memory_areas[segment.kind as usize].write(addr, val) {
                            WriteResult::Wrote => {
                                return;
                            }
                            WriteResult::Ignored => {
                                continue;
                            }
                        }
                    }
                }

                self.ram.write(addr, val);
            }
        }

        #[derive(Clone, Copy)]
        enum MemoryAreaKind {
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

            Num,
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

        impl BankingState {
            fn update(&mut self, update: BankingStateUpdate) {
                match update {
                    BankingStateUpdate::CpuControlLines(value) => {
                        self.cpu_control_lines = value;
                    }
                    BankingStateUpdate::ExpansionPort(value) => {
                        self.expansion_port = value;
                    }
                }

                self.banking.clear();

                let bitmap = BitVec::from_bytes(&[self.expansion_port, self.cpu_control_lines]);
                let loram = bitmap.get(16 - 0).unwrap();
                let hiram = bitmap.get(16 - 1).unwrap();
                let charen = bitmap.get(16 - 2).unwrap();
                let game = bitmap.get(16 - 8).unwrap();
                let exrom = bitmap.get(16 - 9).unwrap();

                unimplemented!()
            }

            fn iter(&self) -> impl Iterator<Item = &Segment> {
                self.banking.iter()
            }
        }

        struct Segment {
            base: u16,
            len: u16,

            kind: MemoryAreaKind,
        }

        impl Segment {
            fn contains(&self, addr: u16) -> bool {
                self.base <= addr && addr < self.base + self.len
            }
        }

        trait MemoryArea {
            fn read(&self, addr: u16) -> u8;
            fn write(&mut self, addr: u16, val: u8) -> WriteResult;
        }

        enum WriteResult {
            Wrote,
            Ignored,
        }

        struct RAM {
            content: [u8; 0xffff],
        }

        impl RAM {
            fn read(&self, addr: u16) -> u8 {
                self.content[addr as usize]
            }

            fn write(&mut self, addr: u16, val: u8) {
                self.content[addr as usize] = val
            }
        }

        struct ROM {
            content: Vec<u8>,
        }

        impl MemoryArea for ROM {
            fn read(&self, addr: u16) -> u8 {
                self.content[addr as usize]
            }

            fn write(&mut self, addr: u16, val: u8) -> WriteResult {
                WriteResult::Ignored
            }
        }
    }
}
