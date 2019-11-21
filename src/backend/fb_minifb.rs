use crate::vic20::{Color, Point, ScreenBackend, SCREEN_HEIGHT, SCREEN_WIDTH};
use minifb::{Window, WindowOptions};
use std::sync::{Arc, Mutex};

pub struct Minifb {
    fb_buf: Arc<Mutex<Vec<u32>>>,
    _jh: std::thread::JoinHandle<()>,
}

impl Minifb {
    pub fn new() -> Self {
        let fb_buf = Arc::new(Mutex::new(vec![0; SCREEN_WIDTH * SCREEN_HEIGHT]));

        let _jh = {
            let fb_buf = fb_buf.clone();
            std::thread::spawn(move || {
                let mut fb_winopts = WindowOptions::default();
                fb_winopts.scale = minifb::Scale::X2;
                let mut fb = Window::new(
                    "Test - ESC to exit",
                    SCREEN_WIDTH,
                    SCREEN_HEIGHT,
                    fb_winopts,
                )
                .unwrap();

                loop {
                    std::thread::sleep(std::time::Duration::from_micros(16666));
                    let buf = fb_buf.lock().unwrap();
                    fb.update_with_buffer(&buf).unwrap();
                }
            })
        };

        Minifb { fb_buf, _jh }
    }
}

impl ScreenBackend for Minifb {
    fn set_point(&mut self, p: Point, c: Color) {
        let Point(x, y) = p;
        let mut buf = self.fb_buf.lock().unwrap();
        buf[y * SCREEN_WIDTH + x] = ARGB::from(c).0;
    }
}

struct ARGB(u32);

impl From<Color> for ARGB {
    fn from(col: Color) -> Self {
        macro_rules! rgb {
            ($r:expr, $g: expr, $b: expr) => {{
                let (r, g, b): (u8, u8, u8) = ($r, $g, $b);
                let mut col: u32 = 0;
                col |= 0 << 24;
                col |= (r as u32) << 16;
                col |= (g as u32) << 8;
                col |= (b as u32) << 0;
                ARGB(col)
            }};
        }
        match col {
            Color::Black => rgb!(0, 0, 0),
            Color::White => rgb!(255, 255, 255),
            Color::Red => rgb!(136, 0, 0),
            Color::Cyan => rgb!(170, 255, 238),
            Color::Violet => rgb!(204, 68, 204),
            Color::Green => rgb!(0, 204, 85),
            Color::Blue => rgb!(0, 0, 170),
            Color::Yellow => rgb!(238, 238, 119),
            Color::Orange => rgb!(221, 136, 85),
            Color::Brown => rgb!(102, 68, 0),
            Color::LightRed => rgb!(255, 119, 119),
            Color::DarkGrey1 => rgb!(51, 51, 51),
            Color::Grey2 => rgb!(119, 119, 119),
            Color::LightGreen => rgb!(170, 255, 102),
            Color::LightBlue => rgb!(0, 136, 255),
            Color::LightGrey3 => rgb!(187, 187, 187),
        }
    }
}
