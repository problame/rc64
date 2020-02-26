use crate::cia::{keyboard::KeyboardMatrix, PeripheralDevicesBackend};

pub struct NoninteractiveBackend;

pub fn new() -> NoninteractiveBackend {
    NoninteractiveBackend
}

impl PeripheralDevicesBackend for NoninteractiveBackend {
    fn get_current_keyboard_matrix(&self) -> KeyboardMatrix {
        KeyboardMatrix::default()
    }
}
