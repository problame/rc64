pub struct RAM {
    content: [u8; 0x10000],
}
impl Default for RAM {
    fn default() -> Self {
        RAM { content: [0; 0x10000] }
    }
}

impl RAM {
    pub fn read(&self, addr: u16) -> u8 {
        self.content[addr as usize]
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        self.content[addr as usize] = val
    }
}
