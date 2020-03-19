use crate::cia::{joystick::JoystickSwitch, keyboard::KeyboardMatrix, PeripheralDevicesBackend};

pub struct NoninteractiveBackend;

pub fn new() -> NoninteractiveBackend {
    NoninteractiveBackend
}

impl PeripheralDevicesBackend for NoninteractiveBackend {
    fn get_current_keyboard_matrix(&self) -> KeyboardMatrix {
        KeyboardMatrix::default()
    }

    fn get_current_joystick1_state(&self) -> JoystickSwitch {
        JoystickSwitch::default()
    }

    fn get_current_joystick2_state(&self) -> JoystickSwitch {
        JoystickSwitch::default()
    }
}
