pub mod framebuffer;
mod mem;
mod registers;

use self::mem::MemoryView;
use self::registers::{ControlRegister1, ControlRegister2, InterruptEnabled, InterruptRegister};
use crate::color_ram::ColorRAM;
use crate::ram::RAM;
use crate::rom::ROM;
use crate::utils::R2C;
use crate::vic20::registers::Registers;
use std::collections::HashSet;
use std::convert::TryFrom;
use std::iter;

use self::framebuffer::ARGB;
pub use self::mem::BankingState;
use bitvec::prelude::*;

use mem::U14;
use num_enum::TryFromPrimitive;

/// https://www.c64-wiki.com/wiki/Color
#[derive(Debug, Eq, PartialEq, TryFromPrimitive, Copy, Clone, EnumString)]
#[repr(u8)]
#[strum(serialize_all = "snake_case")]
pub enum Color {
    Black = 0,
    White = 1,
    Red = 2,
    Cyan = 3,
    Violet = 4,
    Green = 5,
    Blue = 6,
    Yellow = 7,
    Orange = 8,
    Brown = 9,
    LightRed = 10,
    DarkGrey1 = 11,
    Grey2 = 12,
    LightGreen = 13,
    LightBlue = 14,
    LightGrey3 = 15,
}

impl From<self::mem::U4> for Color {
    fn from(four: self::mem::U4) -> Self {
        Color::try_from(u8::from(four)).expect("U4 is always a valid color")
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Point(pub usize, pub usize);

pub struct VIC20<T> {
    mem: MemoryView<T>,
    screen: framebuffer::Writer,
    regs: Registers,
    x: isize,
    // The y encoded in regs is authoritative, this y is only for assertions.
    y: usize,
    raster_breakpoints: HashSet<usize>,
    raster_break_all: bool,
    highlight_raster_beam: bool,
    highlight_x_grid: Option<Color>,
}

// 9.5cycles * 8px
pub const HBLANK_LEFT_PX: isize = 76;
// BORDER+CONTENT+BORDER
pub const VISIBLE_HORIZONTAL_PX: isize = 48 + 320 + 36;
// 3cycles * 8px
pub const HBLANK_RIGHT_PX: isize = 3 * 8;
// WIDTH=HBLANK_LEFT+VISIBLE+HBLANK_RIGHT
pub const SCREEN_WIDTH: usize = (HBLANK_LEFT_PX + VISIBLE_HORIZONTAL_PX + HBLANK_RIGHT_PX) as usize;

const MAX_X: isize = SCREEN_WIDTH as isize + X_START;
const X_START: isize = -(HBLANK_LEFT_PX + 48 / 2);

pub const PIXELS_PER_CYCLE: usize = 8;

pub const VISIBLE_VERTICAL_PX: usize = 284;
pub const VBLANK_TOP_PX: usize = 14;
pub const VBLANK_BTM_PX: usize = 14;
pub const SCREEN_HEIGHT: usize = VBLANK_TOP_PX + VISIBLE_VERTICAL_PX + VBLANK_BTM_PX;

impl<T: AsRef<[u8]>> VIC20<T> {
    pub fn new(
        char_rom: ROM<T>,
        ram: R2C<RAM>,
        color_ram: R2C<ColorRAM>,
        screen: framebuffer::Writer,
    ) -> Self {
        VIC20 {
            mem: MemoryView::new(char_rom, ram, color_ram),
            screen,
            regs: Registers::default(),
            x: X_START,
            y: 0,
            raster_breakpoints: HashSet::new(),
            raster_break_all: false,
            highlight_raster_beam: false,
            highlight_x_grid: None,
        }
    }

    pub fn cycle(
        &mut self,
        cycles: u64,
        mut dbg: std::cell::RefMut<'_, super::mos6510::Debugger>,
    ) -> Option<crate::interrupt::Interrupt> {
        assert_eq!(VISIBLE_HORIZONTAL_PX, 404);
        assert_eq!(SCREEN_WIDTH, 504);
        assert_eq!(SCREEN_WIDTH / PIXELS_PER_CYCLE, 63);
        assert_eq!(SCREEN_HEIGHT, 312);

        let border_start_x = X_START + HBLANK_LEFT_PX + 1;
        let border_end_x = MAX_X - HBLANK_RIGHT_PX;

        let border_start_y = VBLANK_TOP_PX;
        let border_end_y = SCREEN_HEIGHT - VBLANK_BTM_PX;

        let (content_start_x, content_end_x) =
            if self.regs.control_register_2.contains(ControlRegister2::CSEL) {
                (24, 343)
            } else {
                (31, 334)
            };

        let (content_start_y, content_end_y) =
            if self.regs.control_register_1.contains(ControlRegister1::RSEL) {
                (51, 250)
            } else {
                (55, 246)
            };

        let inside_border_zone = self.x >= border_start_x
            && self.x < border_end_x
            && self.y() >= border_start_y
            && self.y() <= border_end_y;

        let inside_content_zone = self.x >= content_start_x
            && self.x <= content_end_x
            && self.y() >= content_start_y
            && self.y() <= content_end_y;

        assert!(!inside_content_zone || inside_border_zone);

        if inside_content_zone {
            match (
                self.regs.control_register_1.contains(ControlRegister1::BMM),
                self.regs.control_register_1.contains(ControlRegister1::ECM),
            ) {
                (false, false) => {
                    // Coordinate transformation
                    let x = (self.x - content_start_x) as usize;
                    let y = self.y() - content_start_y;

                    let char_row = y / 8;
                    let char_col = x / 8;

                    // c-access
                    let vm = self.regs.memory_pointers.video_matrix_base();
                    let vc = char_row * 40 + char_col;
                    let (c_access_msbs, ch) = self.mem.read(vm + vc).into();
                    let c_access_msbs = u8::from(c_access_msbs);

                    // g-access
                    let cb = self.regs.memory_pointers.character_generator_base();
                    let d = (ch as usize) << 3;
                    let rc = y & 0b111;

                    let bm: u8 = self.mem.read_data(cb + d + rc);
                    let bm = bm.bits::<Msb0>();

                    let bg = Color::try_from(self.regs.background_color[0] % 16).unwrap(); // text mode bg color

                    let (mc_flag, mcm_11_pixel_color) =
                        ((c_access_msbs & 0b1000) != 0, c_access_msbs & 0b0111);

                    if mc_flag && self.regs.control_register_2.contains(ControlRegister2::MCM) {
                        let pairs = {
                            let mut bm = bm.iter();
                            let pairs = [
                                (bm.next().unwrap(), bm.next().unwrap()),
                                (bm.next().unwrap(), bm.next().unwrap()),
                                (bm.next().unwrap(), bm.next().unwrap()),
                                (bm.next().unwrap(), bm.next().unwrap()),
                            ];
                            assert!(bm.next().is_none());
                            pairs
                        };

                        let fg = Color::try_from(mcm_11_pixel_color).unwrap();
                        let bg1 = Color::try_from(self.regs.background_color[1] % 16).unwrap();
                        let bg2 = Color::try_from(self.regs.background_color[2] % 16).unwrap();

                        let mut colors = [bg, bg, bg, bg, bg, bg, bg, bg];
                        for (i, pair) in pairs.iter().enumerate() {
                            let color = match pair {
                                (false, false) => bg,
                                (false, true) => bg1,
                                (true, false) => bg2,
                                (true, true) => fg,
                            };
                            colors[2 * i] = color;
                            colors[2 * i + 1] = color;
                        }
                        self.draw_horizontal_slice(&colors);
                    } else {
                        let fg = Color::try_from(c_access_msbs).unwrap();
                        self.draw_horizontal(bm.iter().map(|is_fg| match is_fg {
                            true => fg,
                            false => bg,
                        }))
                    }
                }
                _ => unimplemented!("Only support high-res/multicolor text mode for now"),
            }

            if let Some(col) = self.highlight_x_grid {
                self.draw_horizontal_slice(&[col]);
            }

            let vm = self.regs.memory_pointers.video_matrix_base();
            for (sprite_number, sprite) in
                self.regs.sprite_iter_ordered().enumerate().filter(|(_, sprite)| sprite.enabled)
            {
                // coordinate transformation
                let x = self.x as usize;
                let y = self.y;

                assert!(!sprite.multicolor, "sprite multicolor mode not supported");
                let x_expansion_factor = if sprite.x_expansion { 2 } else { 1 };
                let width = 24 * x_expansion_factor;
                let y_expansion_factor = if sprite.y_expansion { 2 } else { 1 };
                let height = 21 * y_expansion_factor;

                let in_rect =
                    (x >= sprite.x && x < sprite.x + width) && (y >= sprite.y && y < sprite.y + height);
                if in_rect {
                    // p-access
                    let addr = (vm & U14::try_from(0b1111_0000000_000u16).unwrap())
                        | U14::try_from(0b0000_1111111_000u16).unwrap()
                        | U14::try_from(sprite_number & 0b0000_0000000_111).unwrap();

                    let sprite_block_ptr = self.mem.read_data(addr); // https://www.c64-wiki.com/wiki/Screen_RAM
                                                                     // TODO shouldn't the above read only be allowed in banking state 0

                    // s-access
                    let sprite_bm_base = (sprite_block_ptr as usize) << 6;
                    // the lower 6 bytes of the sprite_bm_ptr consist of
                    // 3bytes per row if not expanded
                    // 21 rows
                    // => 63bytes (+ 1 byte )
                    let sprite_x_bm_idx = ((x - sprite.x) / x_expansion_factor) / 8;
                    let sprite_bm_ptr =
                        sprite_bm_base + ((y - sprite.y) / y_expansion_factor) * 3 + sprite_x_bm_idx;
                    let sprite_bm = self.mem.read_data(mem::U14::try_from(sprite_bm_ptr).unwrap());

                    let mut sprite_bm_spreaded: u16 = 0;
                    {
                        if !sprite.x_expansion {
                            sprite_bm_spreaded = ((sprite_bm as u16) << 8) | (sprite_bm as u16);
                        } else {
                            sprite_bm.bits::<Lsb0>().into_iter().enumerate().for_each(|(idx, value)| {
                                sprite_bm_spreaded |= (*value as u16) << (2 * idx + 0);
                                sprite_bm_spreaded |= (*value as u16) << (2 * idx + 1);
                            });
                        };
                    };

                    let off = {
                        let tile_x_offset = (8 * x_expansion_factor as isize) * (sprite_x_bm_idx as isize);
                        tile_x_offset - ((x as isize) - (sprite.x as isize))
                    };

                    self.draw_horizontal_opts(
                        off,
                        sprite_bm_spreaded.bits::<Msb0>().into_iter().take(8 * x_expansion_factor).map(
                            |bit| {
                                if sprite.multicolor {
                                    unimplemented!();
                                } else {
                                    if *bit {
                                        Some(sprite.color)
                                    } else {
                                        None
                                    }
                                }
                            },
                        ),
                    );
                }
            }
        } else if inside_border_zone {
            let c = Color::try_from(self.regs.border_color.bits()).unwrap();
            let colors = [c, c, c, c, c, c, c, c];
            self.draw_horizontal_slice(&colors)
        } else {
            use Color::LightGrey3 as C;
            const COLORS: [Color; 8] = [C, C, C, C, C, C, C, C];
            self.draw_horizontal_slice(&COLORS)
        }

        assert!(self.x != X_START || cycles % 63 == 0);
        assert!(
            !(self.y() == 0 && self.x == X_START) || (cycles % (63 * 312)) == 0,
            "cycles={} y={}",
            cycles,
            self.y()
        );

        self.x += PIXELS_PER_CYCLE as isize;
        if self.x >= SCREEN_WIDTH as isize + X_START {
            assert_eq!(self.x, SCREEN_WIDTH as isize + X_START);
            self.x = X_START;
            self.inc_y();
            if self.y() >= SCREEN_HEIGHT {
                assert_eq!(self.y(), SCREEN_HEIGHT);
                self.reset_y();
            }
            if self.y() == self.regs.raster_interrupt_line {
                self.regs.interrupt_register.insert(InterruptRegister::IRST); // CPU must clear it manually
            }
        }

        if self.highlight_raster_beam {
            self.highlight_next_beam_position();
        }

        if self.x == X_START && (self.raster_break_all || self.raster_breakpoints.contains(&self.y())) {
            dbg.break_after_next_decode();
        }

        // deliver irq if appropriate
        if self.regs.interrupt_enabled.contains(InterruptEnabled::ERST)
            && self.regs.interrupt_register.contains(InterruptRegister::IRST)
        {
            Some(crate::interrupt::Interrupt)
        } else {
            None
        }
    }
}

impl<T> VIC20<T> {
    pub fn update_banking(&mut self, state: mem::BankingState) {
        if self.mem.banking_state != state {
            println!("VIC banking update {:?} -> {:?}", self.mem.banking_state, state);
        }
        self.mem.banking_state = state;
    }

    pub fn get_banking(&self) -> mem::BankingState {
        self.mem.banking_state
    }

    fn y(&self) -> usize {
        let y = (self.regs.raster_counter as usize)
            | if self.regs.control_register_1.contains(ControlRegister1::RST8) { 0b1_0000_0000 } else { 0 };

        assert_eq!(self.y, y, "self.y={:#b}, y={:#b}", self.y, y);

        y
    }

    fn inc_y(&mut self) {
        let mut cur = self.y();
        assert!(cur < 1 << 9);
        cur += 1;
        self.regs.raster_counter = (cur & 0xff) as u8;
        self.regs.control_register_1.set(ControlRegister1::RST8, (cur & 0x100) != 0);

        self.y = self.y.overflowing_add(1).0;
        assert_eq!(self.y(), self.y);
    }

    fn reset_y(&mut self) {
        self.regs.raster_counter = 0;
        self.regs.control_register_1.remove(ControlRegister1::RST8);

        self.y = 0;
        assert_eq!(self.y(), self.y);
    }

    fn highlight_next_beam_position(&mut self) {
        use Color::Yellow as Y;
        const COLORS: [Color; 8] = [Y, Y, Y, Y, Y, Y, Y, Y];
        self.draw_horizontal_slice(&COLORS)
    }

    fn draw_horizontal_opts<I: ExactSizeIterator<Item = Option<Color>>>(&self, offset: isize, cols: I) {
        let starting_point = Point(usize::try_from((self.x - X_START) + offset).unwrap(), self.y());
        iter::successors(Some(starting_point), |p| Some(Point(p.0 + 1, p.1)))
            .zip(cols)
            .filter_map(|(point, opt)| opt.map(|col| (point, col)))
            .for_each(|(point, col)| self.screen.set_px(point, ARGB::from(col)))
    }

    fn draw_horizontal<I: ExactSizeIterator<Item = Color>>(&mut self, cols: I) {
        self.draw_horizontal_opts(0, cols.map(Some))
    }

    #[inline(always)]
    fn draw_horizontal_slice(&mut self, colors: &[Color]) {
        let start = Point((self.x - X_START) as usize, self.y());
        for (i, c) in colors.iter().cloned().enumerate() {
            self.screen.set_px(Point(start.0 + i, start.1), ARGB::from(c))
        }
    }
}

pub trait RasterBreakpointBackend {
    fn status_dump(&self) -> String;
    fn add_raster_breakpoint(&mut self, line: usize);
    fn remove_raster_breakpoint(&mut self, line: usize);
    fn break_on_every_raster_line(&mut self, brk: bool);
    fn highlight_raster_beam(&mut self, beam: bool);
    fn highlight_x_grid(&mut self, highlight: Option<Color>);
}

impl<T> RasterBreakpointBackend for VIC20<T> {
    fn status_dump(&self) -> String {
        format!("x={} regs={{{:?}}}", self.x, self.regs)
    }

    fn add_raster_breakpoint(&mut self, line: usize) {
        self.raster_breakpoints.insert(line);
    }

    fn remove_raster_breakpoint(&mut self, line: usize) {
        self.raster_breakpoints.remove(&line);
    }

    fn break_on_every_raster_line(&mut self, brk: bool) {
        self.raster_break_all = brk;
    }

    fn highlight_raster_beam(&mut self, beam: bool) {
        self.highlight_raster_beam = beam;
        if beam {
            self.highlight_next_beam_position();
        }
    }

    fn highlight_x_grid(&mut self, highlight: Option<Color>) {
        self.highlight_x_grid = highlight;
    }
}
