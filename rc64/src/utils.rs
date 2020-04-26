pub use super::r2c::R2C;

macro_rules! unimpl {
    ($retval:expr => $scope:literal) => {{
        if cfg!(feature = $scope) {
            $retval
        } else {
            unimplemented!()
        }
    }};
    (=> $scope:literal) => {{
        unimpl!(() => $scope)
    }};
}
