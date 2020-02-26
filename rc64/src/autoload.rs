pub mod bin0x0400;
pub mod prg;
pub trait Autloader {
    fn cycle(&mut self);
}
