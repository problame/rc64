use lazy_static::lazy_static;
use std::sync::atomic::AtomicU64;
use structopt::StructOpt;

#[repr(u64)]
#[derive(Clone, Copy, Debug, EnumString, StructOpt)]
pub enum Domain {
    #[strum(serialize = "cia")]
    Cia = 0x1,
    #[strum(serialize = "main")]
    Main = 0x2,
    #[strum(serialize = "vic-sprite-data-collision")]
    VicSpriteDataCollision = 0x4,
}

lazy_static! {
    pub static ref ENABLED: AtomicU64 = AtomicU64::default();
}

macro_rules! unimpl {
    ($retval:expr => $scope:expr) => {{
        let scope: crate::headless_chicken::Domain = $scope;
        if crate::headless_chicken::ENABLED.load(std::sync::atomic::Ordering::Relaxed) & (scope as u64) != 0 {
            $retval
        } else {
            unimplemented!("{:?}", scope);
        }
    }};
    (=> $scope:expr) => {{
        unimpl!(() => $scope)
    }};
}
