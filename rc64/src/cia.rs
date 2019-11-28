use crate::mos6510;
use crate::utils::R2C;
use crate::vic20::VIC20;
use std::rc::Rc;

pub enum CIA<T> {
    Chip1 {
        registers: Vec<Rc<dyn Register>>,

        data_port_a: R2C<DataPortBackend>,
        data_port_b: R2C<DataPortBackend>,
        timer_a: R2C<TimerBackend>,
        timer_b: R2C<TimerBackend>,
        tod: R2C<TimeOfDayBackend>,
    },
    Chip2 {
        registers: Vec<Option<Rc<dyn Register>>>,
        cia_1: R2C<CIA<T>>,
    },
}

// TODO Remove this 'static
impl<T: 'static> CIA<T> {
    pub fn new_chip1() -> Self {
        let data_port_a = r2c_new!(DataPortBackend {});
        let data_port_b = r2c_new!(DataPortBackend {});
        let timer_a = r2c_new!(TimerBackend {});
        let timer_b = r2c_new!(TimerBackend {});
        let tod = r2c_new!(TimeOfDayBackend {});

        CIA::Chip1 {
            registers: vec![
                Rc::new(Data(data_port_a.clone())),
                Rc::new(Data(data_port_b.clone())),
                Rc::new(DataDirection(data_port_a.clone())),
                Rc::new(DataDirection(data_port_b.clone())),
                Rc::new(Timer(timer_a.clone(), ByteHalf::Low)),
                Rc::new(Timer(timer_b.clone(), ByteHalf::High)),
                Rc::new(Timer(timer_a.clone(), ByteHalf::Low)),
                Rc::new(Timer(timer_b.clone(), ByteHalf::High)),
                Rc::new(RTClock(tod.clone(), Precision::DeciSeconds)),
                Rc::new(RTClock(tod.clone(), Precision::Seconds)),
                Rc::new(RTClock(tod.clone(), Precision::Minutes)),
                Rc::new(RTClock(tod.clone(), Precision::Hours)),
                Rc::new(SerialShift),
                Rc::new(IRQStatus),
                Rc::new(ControlTimer(timer_a.clone())),
                Rc::new(ControlTimer(timer_b.clone())),
            ],

            data_port_a,
            data_port_b,
            timer_a,
            timer_b,
            tod,
        }
    }

    pub fn new_chip2(cia_1: R2C<CIA<T>>, vic: R2C<VIC20<T>>) -> Self {
        let serial_bus = SerialBusBackend {};
        let userport = UserportBackend {};
        let rs232 = r2c_new!(RS232Backend {});

        CIA::Chip2 {
            cia_1,
            registers: vec![
                Some(Rc::new(VersatilePortA {
                    vic,
                    serial_bus,
                    rs232: rs232.clone(),
                })),
                Some(Rc::new(VersatilePortB { userport, rs232 })),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(Rc::new(NMILine)),
                None,
                None,
            ],
        }
    }

    pub fn cycle(&self) {
        if let CIA::Chip1 {
            timer_a, timer_b, ..
        } = self
        {
            timer_a.borrow_mut().cycle();
            timer_b.borrow_mut().cycle();
        }
    }
}

pub struct DataPortBackend {}
pub struct TimerBackend {}
pub struct TimeOfDayBackend {}

pub struct SerialBusBackend {}
pub struct UserportBackend {}
pub struct RS232Backend {}

impl TimerBackend {
    fn cycle(&mut self) {
        unimplemented!()
    }
}

pub trait Register {
    fn read(&self) -> u8;
    fn write(&self, val: u8);
}

struct Data(R2C<DataPortBackend>);
struct DataDirection(R2C<DataPortBackend>);
struct Timer(R2C<TimerBackend>, ByteHalf);
struct RTClock(R2C<TimeOfDayBackend>, Precision);
struct SerialShift;
struct IRQStatus;
struct ControlTimer(R2C<TimerBackend>);

struct VersatilePortA<T> {
    vic: R2C<VIC20<T>>,
    serial_bus: SerialBusBackend,
    rs232: R2C<RS232Backend>,
}
struct VersatilePortB {
    userport: UserportBackend,
    rs232: R2C<RS232Backend>,
}
struct NMILine;

enum ByteHalf {
    Low,
    High,
}

enum Precision {
    DeciSeconds,
    Seconds,
    Minutes,
    Hours,
}

impl Register for Data {
    fn read(&self) -> u8 {
        unimplemented!()
    }
    fn write(&self, _val: u8) {
        unimplemented!()
    }
}
impl Register for DataDirection {
    fn read(&self) -> u8 {
        unimplemented!()
    }
    fn write(&self, _val: u8) {
        unimplemented!()
    }
}
impl Register for Timer {
    fn read(&self) -> u8 {
        unimplemented!()
    }
    fn write(&self, _val: u8) {
        unimplemented!()
    }
}
impl Register for ControlTimer {
    fn read(&self) -> u8 {
        unimplemented!()
    }
    fn write(&self, _val: u8) {
        unimplemented!()
    }
}
impl Register for RTClock {
    fn read(&self) -> u8 {
        unimplemented!()
    }
    fn write(&self, _val: u8) {
        unimplemented!()
    }
}
impl Register for SerialShift {
    fn read(&self) -> u8 {
        unimplemented!()
    }
    fn write(&self, _val: u8) {
        unimplemented!()
    }
}
impl Register for IRQStatus {
    fn read(&self) -> u8 {
        unimplemented!()
    }
    fn write(&self, _val: u8) {
        unimplemented!()
    }
}

impl<T> Register for VersatilePortA<T> {
    fn read(&self) -> u8 {
        unimplemented!()
    }
    fn write(&self, _val: u8) {
        unimplemented!()
    }
}
impl Register for VersatilePortB {
    fn read(&self) -> u8 {
        unimplemented!()
    }
    fn write(&self, _val: u8) {
        unimplemented!()
    }
}
impl Register for NMILine {
    fn read(&self) -> u8 {
        unimplemented!()
    }
    fn write(&self, _val: u8) {
        unimplemented!()
    }
}

impl<T> CIA<T> {
    fn get_reg(&self, addr: u16) -> Rc<dyn Register> {
        assert!(addr < 0xff);

        match self {
            CIA::Chip1 { registers, .. } => registers[(addr % 0x10) as usize].clone(),
            CIA::Chip2 {
                cia_1, registers, ..
            } => registers
                .get((addr % 0x10) as usize)
                .and_then(|opt_reg| match opt_reg {
                    // TODO Use #![feature(inner_deref)]
                    Some(reg) => Some(reg),
                    None => None,
                })
                .map(Rc::clone)
                .unwrap_or_else(|| cia_1.borrow().get_reg(addr)),
        }
    }
}

impl<T> mos6510::MemoryArea for CIA<T> {
    fn read(&self, addr: u16) -> u8 {
        self.get_reg(addr).read()
    }

    fn write(&mut self, addr: u16, val: u8) -> mos6510::WriteResult {
        self.get_reg(addr).write(val);
        mos6510::WriteResult::Wrote
    }
}
