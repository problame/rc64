//! VIC II Registers
//!
//!  #| Adr.  |Bit7|Bit6|Bit5|Bit4|Bit3|Bit2|Bit1|Bit0| Function
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//!  0| $d000 |                  M0X                  | X coordinate sprite 0
//! --+-------+---------------------------------------+------------------------
//!  1| $d001 |                  M0Y                  | Y coordinate sprite 0
//! --+-------+---------------------------------------+------------------------
//!  2| $d002 |                  M1X                  | X coordinate sprite 1
//! --+-------+---------------------------------------+------------------------
//!  3| $d003 |                  M1Y                  | Y coordinate sprite 1
//! --+-------+---------------------------------------+------------------------
//!  4| $d004 |                  M2X                  | X coordinate sprite 2
//! --+-------+---------------------------------------+------------------------
//!  5| $d005 |                  M2Y                  | Y coordinate sprite 2
//! --+-------+---------------------------------------+------------------------
//!  6| $d006 |                  M3X                  | X coordinate sprite 3
//! --+-------+---------------------------------------+------------------------
//!  7| $d007 |                  M3Y                  | Y coordinate sprite 3
//! --+-------+---------------------------------------+------------------------
//!  8| $d008 |                  M4X                  | X coordinate sprite 4
//! --+-------+---------------------------------------+------------------------
//!  9| $d009 |                  M4Y                  | Y coordinate sprite 4
//! --+-------+---------------------------------------+------------------------
//! 10| $d00a |                  M5X                  | X coordinate sprite 5
//! --+-------+---------------------------------------+------------------------
//! 11| $d00b |                  M5Y                  | Y coordinate sprite 5
//! --+-------+---------------------------------------+------------------------
//! 12| $d00c |                  M6X                  | X coordinate sprite 6
//! --+-------+---------------------------------------+------------------------
//! 13| $d00d |                  M6Y                  | Y coordinate sprite 6
//! --+-------+---------------------------------------+------------------------
//! 14| $d00e |                  M7X                  | X coordinate sprite 7
//! --+-------+---------------------------------------+------------------------
//! 15| $d00f |                  M7Y                  | Y coordinate sprite 7
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 16| $d010 |M7X8|M6X8|M5X8|M4X8|M3X8|M2X8|M1X8|M0X8| MSBs of X coordinates
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 17| $d011 |RST8| ECM| BMM| DEN|RSEL|    YSCROLL   | Control register 1
//! --+-------+----+----+----+----+----+--------------+------------------------
//! 18| $d012 |                 RASTER                | Raster counter
//! --+-------+---------------------------------------+------------------------
//! 19| $d013 |                  LPX                  | Light pen X
//! --+-------+---------------------------------------+------------------------
//! 20| $d014 |                  LPY                  | Light pen Y
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 21| $d015 | M7E| M6E| M5E| M4E| M3E| M2E| M1E| M0E| Sprite enabled
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 22| $d016 |  - |  - | RES| MCM|CSEL|    XSCROLL   | Control register 2
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 23| $d017 |M7YE|M6YE|M5YE|M4YE|M3YE|M2YE|M1YE|M0YE| Sprite Y expansion
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 24| $d018 |VM13|VM12|VM11|VM10|CB13|CB12|CB11|  - | Memory pointers
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 25| $d019 | IRQ|  - |  - |  - | ILP|IMMC|IMBC|IRST| Interrupt register
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 26| $d01a |  - |  - |  - |  - | ELP|EMMC|EMBC|ERST| Interrupt enabled
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 27| $d01b |M7DP|M6DP|M5DP|M4DP|M3DP|M2DP|M1DP|M0DP| Sprite data priority
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 28| $d01c |M7MC|M6MC|M5MC|M4MC|M3MC|M2MC|M1MC|M0MC| Sprite multicolor
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 29| $d01d |M7XE|M6XE|M5XE|M4XE|M3XE|M2XE|M1XE|M0XE| Sprite X expansion
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 30| $d01e | M7M| M6M| M5M| M4M| M3M| M2M| M1M| M0M| Sprite-sprite collision
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 31| $d01f | M7D| M6D| M5D| M4D| M3D| M2D| M1D| M0D| Sprite-data collision
//! --+-------+----+----+----+----+----+----+----+----+------------------------
//! 32| $d020 |  - |  - |  - |  - |         EC        | Border color
//! --+-------+----+----+----+----+-------------------+------------------------
//! 33| $d021 |  - |  - |  - |  - |        B0C        | Background color 0
//! --+-------+----+----+----+----+-------------------+------------------------
//! 34| $d022 |  - |  - |  - |  - |        B1C        | Background color 1
//! --+-------+----+----+----+----+-------------------+------------------------
//! 35| $d023 |  - |  - |  - |  - |        B2C        | Background color 2
//! --+-------+----+----+----+----+-------------------+------------------------
//! 36| $d024 |  - |  - |  - |  - |        B3C        | Background color 3
//! --+-------+----+----+----+----+-------------------+------------------------
//! 37| $d025 |  - |  - |  - |  - |        MM0        | Sprite multicolor 0
//! --+-------+----+----+----+----+-------------------+------------------------
//! 38| $d026 |  - |  - |  - |  - |        MM1        | Sprite multicolor 1
//! --+-------+----+----+----+----+-------------------+------------------------
//! 39| $d027 |  - |  - |  - |  - |        M0C        | Color sprite 0
//! --+-------+----+----+----+----+-------------------+------------------------
//! 40| $d028 |  - |  - |  - |  - |        M1C        | Color sprite 1
//! --+-------+----+----+----+----+-------------------+------------------------
//! 41| $d029 |  - |  - |  - |  - |        M2C        | Color sprite 2
//! --+-------+----+----+----+----+-------------------+------------------------
//! 42| $d02a |  - |  - |  - |  - |        M3C        | Color sprite 3
//! --+-------+----+----+----+----+-------------------+------------------------
//! 43| $d02b |  - |  - |  - |  - |        M4C        | Color sprite 4
//! --+-------+----+----+----+----+-------------------+------------------------
//! 44| $d02c |  - |  - |  - |  - |        M5C        | Color sprite 5
//! --+-------+----+----+----+----+-------------------+------------------------
//! 45| $d02d |  - |  - |  - |  - |        M6C        | Color sprite 6
//! --+-------+----+----+----+----+-------------------+------------------------
//! 46| $d02e |  - |  - |  - |  - |        M7C        | Color sprite 7
//! --+-------+----+----+----+----+-------------------+------------------------
//!
//! Notes:
//!
//!  · The bits marked with '-' are not connected and give "1" on reading
//!  · The VIC registers are repeated each 64 bytes in the area $d000-$d3ff,
//!    i.e. register 0 appears on addresses $d000, $d040, $d080 etc.
//!  · The unused addresses $d02f-$d03f give $ff on reading, a write access is
//!    ignored
//!  · The registers $d01e and $d01f cannot be written and are automatically
//!    cleared on reading
//!  · The RES bit (bit 5) of register $d016 has no function on the VIC
//!    6567/6569 examined as yet. On the 6566, this bit is used to stop the
//!    VIC.
//!  · Bit 7 in register $d011 (RST8) is bit 8 of register $d012. Together they
//!    are called "RASTER" in the following. A write access to these bits sets
//!    the comparison line for the raster interrupt (see section 3.12.).

use super::VIC20;
use crate::mos6510::{MemoryArea, WriteResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Coordinate {
    pub x: u8,
    pub y: u8,
}

#[derive(Debug, Default)]
pub(super) struct Registers {
    pub control_register_1: ControlRegister1,
    pub control_register_2: ControlRegister2,

    pub raster_interrupt_line: usize,
    pub raster_counter: u8,

    pub light_pen: Coordinate,
    pub memory_pointers: u8,

    pub interrupt_register: InterruptRegister,
    pub interrupt_enabled: InterruptEnabled,

    pub coordinate_sprite: [Coordinate; 8],
    pub msbs_of_x_coordinates: u8,
    pub sprite_enabled: u8,
    pub sprite_expansion: Coordinate,
    pub sprite_data_priority: u8,
    pub sprite_multicolor: u8,
    pub sprite_sprite_collision: u8,
    pub sprite_data_collision: u8,
    pub sprite_multicolors: [u8; 2],

    pub border_color: BorderColor,
    pub background_color: [u8; 4],
    pub color_sprite: [u8; 8],
}

bitflags! {
    #[derive(Default)]
    pub(super) struct ControlRegister1: u8 {
        const YSCROLL = 0b0_00_00_111;
        /// RSEL|  Display window height   | First line  | Last line
        /// ----+--------------------------+-------------+----------
        ///   0 | 24 text lines/192 pixels |   55 ($37)  | 246 ($f6)
        ///   1 | 25 text lines/200 pixels |   51 ($33)  | 250 ($fa)
        const RSEL = 0b0_00_01_000;
        const DEN = 0b0_00_10_000;
        /// Bit Map Mode
        const BMM = 0b0_01_00_000;
        /// Extendet Color Mode
        const ECM = 0b0_10_00_000;
        const RST8 = 0b1_00_00_000;
    }
}

bitflags! {
    #[derive(Default)]
    pub(super) struct ControlRegister2: u8 {
        const XSCROLL = 0b00_000_111;
        /// CSEL|   Display window width   | First X coo. | Last X coo.
        /// ----+--------------------------+--------------+------------
        ///   0 | 38 characters/304 pixels |   31 ($1f)   |  334 ($14e)
        ///   1 | 40 characters/320 pixels |   24 ($18)   |  343 ($157)
        const CSEL = 0b00_001_000;
        /// Multi Color Mode
        const MCM = 0b00_010_000;
        const RES = 0b00_100_000;
    }
}

bitflags! {
    #[derive(Default)]
    pub(super) struct BorderColor: u8 {
        const EC = 0b0000_1111;
    }
}

bitflags! {
    #[derive(Default)]
    pub(super) struct InterruptRegister: u8 {
        const IRST = 0b0_000_0001;
        const IMBC = 0b0_000_0010;
        const IMMC = 0b0_000_0100;
        const ILP  = 0b0_000_1000;
        const IRQ  = 0b1_000_0000;
    }
}

bitflags! {
    #[derive(Default)]
    pub(super) struct InterruptEnabled: u8 {
        const ERST = 0b0000_0001;
        const EMBC = 0b0000_0010;
        const EMMC = 0b0000_0100;
        const ELP  = 0b0000_1000;
    }
}

impl<T> MemoryArea for VIC20<T> {
    fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize & 0xff;
        let val = match addr {
            0x00..=0x0f => {
                // Access to x and y coordinates in alternating order
                let c = self.regs.coordinate_sprite[addr >> 1];
                if addr % 2 == 0 {
                    c.x
                } else {
                    c.y
                }
            }
            0x10 => self.regs.msbs_of_x_coordinates,
            0x11 => self.regs.control_register_1.bits(),
            0x12 => self.regs.raster_counter,
            0x13 => self.regs.light_pen.x,
            0x14 => self.regs.light_pen.y,
            0x15 => self.regs.sprite_enabled,
            0x16 => self.regs.control_register_2.bits() | 0b1100_0000,
            0x17 => self.regs.sprite_expansion.y,
            0x18 => self.regs.memory_pointers | 0b0000_0001,
            0x19 => self.regs.interrupt_register.bits() | 0b0111_0000,
            0x1a => self.regs.interrupt_enabled.bits() | 0b1111_0000,
            0x1b => self.regs.sprite_data_priority,
            0x1c => self.regs.sprite_multicolor,
            0x1d => self.regs.sprite_expansion.x,
            0x1e => self.regs.sprite_sprite_collision,
            0x1f => self.regs.sprite_data_collision,
            0x20 => self.regs.border_color.bits() | 0b1111_0000,
            0x21..=0x24 => self.regs.background_color[addr - 0x21] | 0b1111_0000,
            0x25..=0x26 => self.regs.sprite_multicolors[addr - 0x25] | 0b1111_0000,
            0x27..=0x2e => self.regs.color_sprite[addr - 0x27] | 0b1111_0000,
            _ => unreachable!("47usize..=std::usize::MAX should be masked out"),
        };

        // print!("VIC READ 0xd0{:0>2x}-> 0x{:0>2x}", addr, val);
        // if val != 0 {
        //     println!(" (!)");
        // } else {
        //     println!("");
        // }

        val
    }

    fn write(&mut self, addr: u16, val: u8) -> WriteResult {
        let addr = addr as usize & 0xff;
        // println!("VIC WRIT 0xd0{:0>2x}:= 0x{:0>2x}", addr, val);
        match addr {
            0x00..=0x0f => {
                // Access to x and y coordinates in alternating order
                let mut c = self.regs.coordinate_sprite[addr >> 1];
                if addr % 2 == 0 {
                    c.x = val
                } else {
                    c.y = val
                }
            }
            0x10 => self.regs.msbs_of_x_coordinates = val,
            0x11 => {
                let mut cr = ControlRegister1::from_bits(val).unwrap();

                let raster_interrupt_line_bit8 =
                    ((self.regs.control_register_1.contains(ControlRegister1::RST8)) as usize) << 8;
                self.regs.raster_interrupt_line &= !(raster_interrupt_line_bit8);
                self.regs.raster_interrupt_line |= raster_interrupt_line_bit8;

                // never commit bit 8 on a write, we track that in raster_interrupt_line
                // and when reading this cr, we want the read to reflect the current raster line
                // which is managed by VIC20::inc_y
                cr.remove(ControlRegister1::RST8);
                self.regs.control_register_1 = cr;
            }
            0x12 => {
                self.regs.raster_interrupt_line &= !(0xff);
                self.regs.raster_interrupt_line |= val as usize;
            }
            0x13 => self.regs.light_pen.x = val,
            0x14 => self.regs.light_pen.y = val,
            0x15 => self.regs.sprite_enabled = val,
            0x16 => self.regs.control_register_2 = ControlRegister2::from_bits_truncate(val),
            0x17 => self.regs.sprite_expansion.y = val,
            0x18 => self.regs.memory_pointers = val,
            0x19 => self.regs.interrupt_register = InterruptRegister::from_bits_truncate(val),
            0x1a => self.regs.interrupt_enabled = InterruptEnabled::from_bits_truncate(val),
            0x1b => self.regs.sprite_data_priority = val,
            0x1c => self.regs.sprite_multicolor = val,
            0x1d => self.regs.sprite_expansion.x = val,
            0x1e => self.regs.sprite_sprite_collision = val,
            0x1f => self.regs.sprite_data_collision = val,
            0x20 => self.regs.border_color = BorderColor::from_bits_truncate(val),
            0x21..=0x24 => self.regs.background_color[addr - 0x21] = val,
            0x25..=0x26 => self.regs.sprite_multicolors[addr - 0x25] = val,
            0x27..=0x2e => self.regs.color_sprite[addr - 0x27] = val,
            _ => unreachable!("47usize..=std::usize::MAX should be masked out"),
        }

        WriteResult::Wrote
    }
}
