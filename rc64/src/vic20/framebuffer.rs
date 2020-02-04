use super::{Color, Point, SCREEN_HEIGHT, SCREEN_WIDTH};
use std::cell::UnsafeCell;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

/// Buffer is shared between VIC and backend via `Reader` and `Writer`.
///
/// Writer uses atomic writes with relaxed ordering, which we observed to be good enough.
/// YMMV with othe r platforms / weaker consistency models.
///
/// On x86_64, these will be compiled to regular store instructions.
struct Buffer {
    buf: UnsafeCell<Vec<ARGB>>,
}

/// Not cloneable because
pub struct Writer {
    ptr: Arc<Buffer>,
}

unsafe impl Send for Writer {}
unsafe impl Sync for Writer {}

#[derive(Clone)]
pub struct Reader {
    ptr: Arc<Buffer>,
}

unsafe impl Send for Reader {}
unsafe impl Sync for Reader {}

/// Entrypoint to this module: produce a pair of screen buffer reader and updater.
pub fn new() -> (Reader, Writer) {
    let buf = Arc::new(Buffer { buf: UnsafeCell::new(vec![ARGB(0); SCREEN_WIDTH * SCREEN_HEIGHT]) });
    (Reader { ptr: buf.clone() }, Writer { ptr: buf })
}

impl Writer {
    /// Interior mutability done via atomic updates, hence &self not &mut self
    #[inline(always)]
    pub fn set_px(&self, p: Point, col: ARGB) {
        // catch invalid coordinates before computing offset into vec
        // => do not make them debug_assert, we do not do bounds checks below
        assert!(p.0 < SCREEN_WIDTH);
        assert!(p.1 < SCREEN_HEIGHT);
        assert_eq!(std::mem::size_of::<AtomicU32>(), std::mem::size_of::<ARGB>());
        let buf: &mut Vec<ARGB> = unsafe { &mut *self.ptr.buf.get() };
        let entry: &mut ARGB = unsafe { buf.get_unchecked_mut(p.1 * SCREEN_WIDTH + p.0) };
        let entry: &mut AtomicU32 = unsafe { &mut *(entry as *mut ARGB as *mut _ as *mut AtomicU32) };
        entry.store(col.0, Ordering::Relaxed);
    }
}

impl Reader {
    pub const fn width(&self) -> usize {
        SCREEN_WIDTH
    }

    pub const fn height(&self) -> usize {
        SCREEN_HEIGHT
    }

    pub fn as_argb_slice(&self) -> &[ARGB] {
        unsafe {
            let vec = &*self.ptr.buf.get();
            std::slice::from_raw_parts(vec.as_ptr(), vec.len())
        }
    }

    pub fn as_u32_slice(&self) -> &[u32] {
        let argb = self.as_argb_slice();
        assert_eq!(std::mem::size_of::<ARGB>(), std::mem::size_of::<u32>(), "ARGB is just an u32");
        unsafe { std::slice::from_raw_parts(argb.as_ptr() as *const u32, argb.len()) }
    }
}

unsafe impl Send for Buffer {}

unsafe impl Sync for Buffer {}

#[derive(Clone, Copy)]
pub struct ARGB(u32);

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
