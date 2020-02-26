use bitflags;

bitflags! {
    #[derive(Default)]
    pub struct JoystickSwitch: u8 {
        const UP           = 0b0_0001;
        const DOWN         = 0b0_0010;
        const LEFT         = 0b0_0100;
        const RIGHT        = 0b0_1000;
        const FIRE         = 0b1_0000;
    }
}
