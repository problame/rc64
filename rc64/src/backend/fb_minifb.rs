use crate::cia::joystick::JoystickSwitch;
use crate::cia::keyboard::{C64Key, KeyboardMatrix};
use crate::cia::PeripheralDevicesBackend;
use crate::{vic20::framebuffer, JoystickMode};
use minifb::{Key, Window, WindowOptions};
use spin::Mutex;
use std::convert::TryFrom;
use std::sync::Arc;

pub struct Minifb {
    pressed_keys: Arc<Mutex<Vec<Key>>>,
    _jh: std::thread::JoinHandle<()>,
    joystick_mode: (JoystickMode, JoystickMode),
}

impl Minifb {
    pub fn new(fb_buf: framebuffer::Reader, joystick_mode: (JoystickMode, JoystickMode)) -> Self {
        let pressed_keys = Arc::new(Mutex::new(Vec::new()));

        let _jh = {
            let pressed_keys = pressed_keys.clone();
            std::thread::spawn(move || {
                let mut fb_winopts = WindowOptions::default();
                fb_winopts.scale = minifb::Scale::X2;
                let mut fb =
                    Window::new("rc64 - C64 Emulator", fb_buf.width(), fb_buf.height(), fb_winopts)
                        .unwrap();

                loop {
                    std::thread::sleep(std::time::Duration::from_micros(16666));
                    fb.update_with_buffer(fb_buf.as_u32_slice()).unwrap();

                    let mut pressed_keys = pressed_keys.lock();
                    *pressed_keys = fb.get_keys().unwrap_or_default();
                }
            })
        };

        Minifb { pressed_keys, _jh, joystick_mode }
    }

    fn get_current_joystick_state(&self, joystick_mode: JoystickMode) -> JoystickSwitch {
        self.pressed_keys
            .lock()
            .iter()
            .cloned()
            .map(|key| joystick_mode.event_from(key))
            .filter_map(Result::ok)
            .fold(JoystickSwitch::default(), std::ops::BitOr::bitor)
    }
}

impl PeripheralDevicesBackend for Minifb {
    fn get_current_keyboard_matrix(&self) -> KeyboardMatrix {
        self.pressed_keys
            .lock()
            .iter()
            .cloned()
            .filter(|k| !(self.joystick_mode.0.grabs_key(*k) || self.joystick_mode.1.grabs_key(*k)))
            .map(C64Key::try_from)
            .filter_map(Result::ok)
            .into()
    }

    fn get_current_joystick1_state(&self) -> JoystickSwitch {
        self.get_current_joystick_state(self.joystick_mode.0)
    }

    fn get_current_joystick2_state(&self) -> JoystickSwitch {
        self.get_current_joystick_state(self.joystick_mode.1)
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
            NumPad0        => Err(UnmappedKey), // Ok(C64Key::Zero),
            NumPad1        => Err(UnmappedKey), // Ok(C64Key::One),
            NumPad2        => Err(UnmappedKey), // Ok(C64Key::Two),
            NumPad3        => Err(UnmappedKey), // Ok(C64Key::Three),
            NumPad4        => Err(UnmappedKey), // Ok(C64Key::Four),
            NumPad5        => Err(UnmappedKey), // Ok(C64Key::Five),
            NumPad6        => Err(UnmappedKey), // Ok(C64Key::Six),
            NumPad7        => Err(UnmappedKey), // Ok(C64Key::Seven),
            NumPad8        => Err(UnmappedKey), // Ok(C64Key::Eight),
            NumPad9        => Err(UnmappedKey), // Ok(C64Key::Nine),
            NumPadDot      => Err(UnmappedKey), // Ok(C64Key::Period),
            NumPadSlash    => Err(UnmappedKey), // Ok(C64Key::Slash),
            NumPadAsterisk => Err(UnmappedKey), // Ok(C64Key::Star),
            NumPadMinus    => Err(UnmappedKey), // Ok(C64Key::Dash),
            NumPadPlus     => Err(UnmappedKey), // Ok(C64Key::Plus),
            NumPadEnter    => Err(UnmappedKey), // Ok(C64Key::Return),
            LeftAlt        => Err(UnmappedKey),
            RightAlt       => Err(UnmappedKey),
            LeftSuper      => Ok(C64Key::Commodore),
            RightSuper     => Ok(C64Key::Commodore),
            Unknown        => Err(UnmappedKey),
            Count          => unreachable!(),
        }
    }
}

impl JoystickMode {
    fn grabs_key(&self, key: Key) -> bool {
        self.event_from(key).is_ok()
    }

    fn event_from(&self, key: Key) -> Result<JoystickSwitch, UnmappedKey> {
        use Key::*;
        match self {
            JoystickMode::None => Err(UnmappedKey),
            JoystickMode::Wasd => match key {
                W => Ok(JoystickSwitch::UP),
                A => Ok(JoystickSwitch::LEFT),
                S => Ok(JoystickSwitch::DOWN),
                D => Ok(JoystickSwitch::RIGHT),
                Space => Ok(JoystickSwitch::FIRE),
                Count => unreachable!(),
                _ => Err(UnmappedKey),
            },

            JoystickMode::NumPad => match key {
                NumPad8 => Ok(JoystickSwitch::UP),
                NumPad4 => Ok(JoystickSwitch::LEFT),
                NumPad5 => Ok(JoystickSwitch::DOWN),
                NumPad6 => Ok(JoystickSwitch::RIGHT),
                NumPadEnter | NumPad0 => Ok(JoystickSwitch::FIRE),
                Count => unreachable!(),
                _ => Err(UnmappedKey),
            },

            JoystickMode::Arrows => match key {
                Down => Ok(JoystickSwitch::DOWN),
                Left => Ok(JoystickSwitch::LEFT),
                Right => Ok(JoystickSwitch::RIGHT),
                Up => Ok(JoystickSwitch::UP),
                Space => Ok(JoystickSwitch::FIRE),
                Count => unreachable!(),
                _ => Err(UnmappedKey),
            },
        }
    }
}
