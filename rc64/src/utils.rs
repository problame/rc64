use std::cell::RefCell;
use std::rc::Rc;

pub type R2C<T> = Rc<RefCell<T>>;
macro_rules! r2c_new {
    ($val: expr) => {{
        use std::cell::RefCell;
        use std::rc::Rc;
        Rc::new(RefCell::new($val))
    }};
}

macro_rules! unimpl {
    ($retval: expr) => {
        if cfg!(feature = "headless-chicken") {
            return $retval;
        } else {
            unimplemented!()
        }
    };
    () => {
        if cfg!(not(feature = "headless-chicken")) {
            unimplemented!()
        } else {
            ()
        }
    };
}
