use crate::cia::keyboard::{C64Key, KeyboardMatrix};
use crate::cia::PeripheralDevicesBackend;
use crate::vic20::{Color, Point, ScreenBackend, SCREEN_HEIGHT, SCREEN_WIDTH};
use minifb::{Key, Window, WindowOptions};
use spin::Mutex;
use std::convert::TryFrom;
use std::sync::Arc;

pub struct Minifb {
    fb_buf: Arc<Mutex<Vec<u32>>>,
    pressed_keys: Arc<Mutex<Vec<Key>>>,
    _jh: std::thread::JoinHandle<()>,
}

impl Minifb {
    pub fn new() -> Self {
        let fb_buf = Arc::new(Mutex::new(vec![0; SCREEN_WIDTH * SCREEN_HEIGHT]));

        let pressed_keys = Arc::new(Mutex::new(Vec::new()));

        let _jh = {
            let fb_buf = fb_buf.clone();
            let pressed_keys = pressed_keys.clone();
            std::thread::spawn(move || {
                let mut fb_winopts = WindowOptions::default();
                fb_winopts.scale = minifb::Scale::X2;
                let mut fb =
                    Window::new("Test - ESC to exit", SCREEN_WIDTH, SCREEN_HEIGHT, fb_winopts).unwrap();

                loop {
                    std::thread::sleep(std::time::Duration::from_micros(16666));
                    let buf = fb_buf.lock();
                    fb.update_with_buffer(&buf).unwrap();

                    let mut pressed_keys = pressed_keys.lock();
                    *pressed_keys = fb.get_keys().unwrap_or_default();
                }
            })
        };

        Minifb { fb_buf, pressed_keys, _jh }
    }
}

impl ScreenBackend for Minifb {
    fn set_px(&mut self, p: Point, col: Color) {
        let mut buf = self.fb_buf.lock();
        assert!(p.0 < SCREEN_WIDTH);
        assert!(p.1 < SCREEN_HEIGHT);
        buf[p.1 * SCREEN_WIDTH + p.0] = ARGB::from(col).0;
    }
}

impl PeripheralDevicesBackend for Minifb {
    fn get_current_keyboard_matrix(&self) -> KeyboardMatrix {
        self.pressed_keys.lock().iter().cloned().map(C64Key::try_from).filter_map(Result::ok).into()
    }
}

pub struct UnmappedKey;
impl TryFrom<Key> for C64Key {
    type Error = UnmappedKey;

    /// ## Hidden keys
    /// Some keys are hidden ...
    ///   (US | DE => C64)
    ///     ' | ä => :
    ///     \ | # => +
    ///     [ | ü => @
    ///     ] | ~ => *
    ///     ` | ^ => €
    ///     ; | ö => ;
    #[rustfmt::skip]
    fn try_from(key: Key) -> Result<Self, Self::Error> {
        use Key::*;
        match key {
            Key0           => Ok(C64Key::Zero),
            Key1           => Ok(C64Key::One),
            Key2           => Ok(C64Key::Two),
            Key3           => Ok(C64Key::Three),
            Key4           => Ok(C64Key::Four),
            Key5           => Ok(C64Key::Five),
            Key6           => Ok(C64Key::Six),
            Key7           => Ok(C64Key::Seven),
            Key8           => Ok(C64Key::Eight),
            Key9           => Ok(C64Key::Nine),
            A              => Ok(C64Key::A),
            B              => Ok(C64Key::B),
            C              => Ok(C64Key::C),
            D              => Ok(C64Key::D),
            E              => Ok(C64Key::E),
            F              => Ok(C64Key::F),
            G              => Ok(C64Key::G),
            H              => Ok(C64Key::H),
            I              => Ok(C64Key::I),
            J              => Ok(C64Key::J),
            K              => Ok(C64Key::K),
            L              => Ok(C64Key::L),
            M              => Ok(C64Key::M),
            N              => Ok(C64Key::N),
            O              => Ok(C64Key::O),
            P              => Ok(C64Key::P),
            Q              => Ok(C64Key::Q),
            R              => Ok(C64Key::R),
            S              => Ok(C64Key::S),
            T              => Ok(C64Key::T),
            U              => Ok(C64Key::U),
            V              => Ok(C64Key::V),
            W              => Ok(C64Key::W),
            X              => Ok(C64Key::X),
            Y              => Ok(C64Key::Y),
            Z              => Ok(C64Key::Z),
            F1             => Ok(C64Key::F1),
            F2             => Err(UnmappedKey),
            F3             => Ok(C64Key::F3),
            F4             => Err(UnmappedKey),
            F5             => Ok(C64Key::F5),
            F6             => Err(UnmappedKey),
            F7             => Ok(C64Key::F7),
            F8             => Err(UnmappedKey),
            F9             => Err(UnmappedKey),
            F10            => Err(UnmappedKey),
            F11            => Err(UnmappedKey),
            F12            => Err(UnmappedKey),
            F13            => Err(UnmappedKey),
            F14            => Err(UnmappedKey),
            F15            => Err(UnmappedKey),
            Down           => Ok(C64Key::CursorDown),
            Left           => Ok(C64Key::Left),
            Right          => Ok(C64Key::CursorRight),
            Up             => Ok(C64Key::Up),
            Apostrophe     => Ok(C64Key::Colon),
            Backquote      => Ok(C64Key::Currency),
            Backslash      => Ok(C64Key::Plus),
            Comma          => Ok(C64Key::Comma),
            Equal          => Ok(C64Key::Equal),
            LeftBracket    => Ok(C64Key::AtSign),
            Minus          => Ok(C64Key::Dash),
            Period         => Ok(C64Key::Period),
            RightBracket   => Ok(C64Key::Star),
            Semicolon      => Ok(C64Key::Semicolon),
            Slash          => Ok(C64Key::Slash),
            Backspace      => Ok(C64Key::Delete),
            Delete         => Ok(C64Key::Delete),
            End            => Ok(C64Key::Stop),
            Enter          => Ok(C64Key::Return),
            Escape         => Ok(C64Key::Stop),
            Home           => Ok(C64Key::Home),
            Insert         => Err(UnmappedKey),
            Menu           => Err(UnmappedKey),
            PageDown       => Err(UnmappedKey),
            PageUp         => Err(UnmappedKey),
            Pause          => Err(UnmappedKey),
            Space          => Ok(C64Key::Space),
            Tab            => Err(UnmappedKey),
            NumLock        => Err(UnmappedKey),
            CapsLock       => Err(UnmappedKey),
            ScrollLock     => Err(UnmappedKey),
            LeftShift      => Ok(C64Key::LShift),
            RightShift     => Ok(C64Key::RShift),
            LeftCtrl       => Ok(C64Key::Ctrl),
            RightCtrl      => Ok(C64Key::Ctrl),
            NumPad0        => Ok(C64Key::Zero),
            NumPad1        => Ok(C64Key::One),
            NumPad2        => Ok(C64Key::Two),
            NumPad3        => Ok(C64Key::Three),
            NumPad4        => Ok(C64Key::Four),
            NumPad5        => Ok(C64Key::Five),
            NumPad6        => Ok(C64Key::Six),
            NumPad7        => Ok(C64Key::Seven),
            NumPad8        => Ok(C64Key::Eight),
            NumPad9        => Ok(C64Key::Nine),
            NumPadDot      => Ok(C64Key::Period),
            NumPadSlash    => Ok(C64Key::Slash),
            NumPadAsterisk => Ok(C64Key::Star),
            NumPadMinus    => Ok(C64Key::Dash),
            NumPadPlus     => Ok(C64Key::Plus),
            NumPadEnter    => Ok(C64Key::Return),
            LeftAlt        => Err(UnmappedKey),
            RightAlt       => Err(UnmappedKey),
            LeftSuper      => Ok(C64Key::Commodore),
            RightSuper     => Ok(C64Key::Commodore),
            Unknown        => Err(UnmappedKey),
            Count          => unreachable!(),
        }
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
