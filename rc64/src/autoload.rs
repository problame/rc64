pub mod prg;
pub mod bin0x0400;
pub trait Autloader {
    fn cycle(&mut self);
}
