use crate::cia::keyboard::MatrixIndex;
use crate::utils::R2C;

use super::backends::*;

pub trait Register {
    fn read(&self) -> u8;
    fn write(&self, val: u8);
}

pub(super) struct DataA<T>(pub R2C<DataPortBackend<T>>);
pub(super) struct DataDirectionA<T>(pub R2C<DataPortBackend<T>>);

pub(super) struct DataB<T>(pub R2C<DataPortBackend<T>>);
pub(super) struct DataDirectionB<T>(pub R2C<DataPortBackend<T>>);

pub(super) struct Timer(pub R2C<TimerBackend>, pub ByteHalf);
pub(super) struct RTClock(pub R2C<TimeOfDayBackend>, pub Precision);
pub(super) struct SerialShift(pub R2C<SerialShiftBackend>);
pub(super) struct InterruptControl(pub R2C<InterruptBackend>);

pub(super) struct ControlTimer(pub R2C<TimerBackend>);

#[derive(Debug)]
pub(super) enum ByteHalf {
    Low,
    High,
}

pub(super) enum Precision {
    DeciSeconds,
    Seconds,
    Minutes,
    Hours,
}

/// 56320         $DC00          CIAPRA
/// Data Port Register A
///
/// Bit 0:  Select to read keyboard column 0
///         Read joystick 2 up direction
/// Bit 1:  Select to read keyboard column 1
///         Read joystick 2 down direction
/// Bit 2:  Select to read keyboard column 2
///         Read joystick 2 left direction
///         Read paddle 1 fire button
/// Bit 3:  Select to read keyboard column 3
///         Read joystick 2 right direction
///         Read paddle 2 fire button
/// Bit 4:  Select to read keyboard column 4
///         Read joystick 2 fire button
/// Bit 5:  Select to read keyboard column 5
/// Bit 6:  Select to read keyboard column 6
///         Select to read paddles on Port A or B
/// Bit 7:  Select to read keyboard column 7
///         Select to read paddles on Port A or B
///
/// 56576         $DD00          CI2PRA
/// Data Port Register A
///
/// Bits 0-1:  Select the 16K VIC-II chip memory bank (11=bank 0, 00=bank 3)
/// Bit 2:  RS-232 data output (Sout)/Pin M of User Port
/// Bit 3:  Serial bus ATN signal output
/// Bit 4:  Serial bus clock pulse output
/// Bit 5:  Serial bus data output
/// Bit 6:  Serial bus clock pulse input
/// Bit 7:  Serial bus data input
impl<T> Register for DataA<T> {
    fn read(&self) -> u8 {
        match *self.0.borrow() {
            DataPortBackend::CIA1 { ref peripherals, .. } => {
                let out = !peripherals.borrow().get_current_joystick2_state().bits();
                out
            }
            DataPortBackend::CIA2 { ref vic, .. } => {
                use crate::vic20::BankingState::*;
                match vic.borrow_mut().get_banking() {
                    Bank0 => 0b11,
                    Bank1 => 0b10,
                    Bank2 => 0b01,
                    Bank3 => 0b00,
                }
            }
        }
    }

    fn write(&self, val: u8) {
        match *self.0.borrow_mut() {
            DataPortBackend::CIA1 { ref mut last_data_a_write, .. } => {
                *last_data_a_write = val;
            }
            DataPortBackend::CIA2 { ref vic, .. } => {
                use crate::vic20::BankingState::*;
                vic.borrow_mut().update_banking(match val & 0b0000_00011 {
                    0b11 => Bank0,
                    0b10 => Bank1,
                    0b01 => Bank2,
                    0b00 => Bank3,
                    _ => unreachable!("4u8..=u8::MAX have been masked out"),
                })
            }
        }
    }
}

/// 56322         $DC02          CIDDRA
/// Data Direction Register A
///
/// Bit 0:  Select Bit 0 of Data Port A for input or output (0=input, 1=output)
/// Bit 1:  Select Bit 1 of Data Port A for input or output (0=input, 1=output)
/// Bit 2:  Select Bit 2 of Data Port A for input or output (0=input, 1=output)
/// Bit 3:  Select Bit 3 of Data Port A for input or output (0=input, 1=output)
/// Bit 4:  Select Bit 4 of Data Port A for input or output (0=input, 1=output)
/// Bit 5:  Select Bit 5 of Data Port A for input or output (0=input, 1=output)
/// Bit 6:  Select Bit 6 of Data Port A for input or output (0=input, 1=output)
/// Bit 7:  Select Bit 7 of Data Port A for input or output (0=input, 1=output)
impl<T> Register for DataDirectionA<T> {
    fn read(&self) -> u8 {
        unimpl!(0 => "headless-chicken--cia")
    }
    fn write(&self, _val: u8) {
        unimpl!(=> "headless-chicken--cia")
    }
}

/// 56321         $DC01          CIAPRB
/// Data Port Register B
///
/// Bit 0:  Read keyboard row 0
///         Read joystick 1 up direction
/// Bit 1:  Read keyboard row 1
///         Read joystick 1 down direction
/// Bit 2:  Read keyboard row 2
///         Read joystick 1 left direction
///         Read paddle 1 fire button
/// Bit 3:  Read keyboard row 3
///         Read joystick 1 right direction
///         Read paddle 2 fire button
/// Bit 4:  Read keyboard row 4
///         Read joystick 1 fire button
/// Bit 5:  Read keyboard row 5
/// Bit 6:  Read keyboard row 6
///         Toggle or pulse data output for Timer A
/// Bit 7:  Read keyboard row 7
///         Toggle or pulse data output for Timer B
///
/// 56577         $DD01          CI2PRB
/// Data Port B
///
/// Bit 0:  RS-232 data input (SIN)/ Pin C of User Port
/// Bit 1:  RS-232 request to send (RTS)/ Pin D of User Port
/// Bit 2:  RS-232 data terminal ready (DTR)/ Pin E of User Port
/// Bit 3:  RS-232 ring indicator (RI)/ Pin F of User Port
/// Bit 4:  RS-232 carrier detect (DCD)/ Pin H of User Port
/// Bit 5:  Pin J of User Port
/// Bit 6:  RS-232 clear to send (CTS)/ Pin K of User Port
///         Toggle or pulse data output for Timer A
/// Bit 7:  RS-232 data set ready (DSR)/ Pin L of User Port
///         Toggle or pulse data output for Timer B
impl<T> Register for DataB<T> {
    fn read(&self) -> u8 {
        match &*self.0.borrow() {
            DataPortBackend::CIA1 { peripherals, last_data_a_write } => {
                let keyboard_matrix = peripherals.borrow().get_current_keyboard_matrix();

                // last_data_a_write selected a set of columns
                // reading B returns one byte,
                // each bit i representing whether the key (i,c) is pressed in any c \elem columns

                assert_eq!(keyboard_matrix.num_rows(), 8);
                assert_eq!(keyboard_matrix.num_cols(), 8);
                let mut out: u8 = 0;
                for row_bitpos in 0..8 {
                    for col_bitpos in 0..8 {
                        let key_pressed = keyboard_matrix[MatrixIndex::rc(row_bitpos, col_bitpos)];
                        let col_requested = (last_data_a_write & (1 << col_bitpos)) == 0; // unset bit means requested
                        out |= ((col_requested && key_pressed) as u8) << row_bitpos;
                    }
                }

                out = !out; // 0 means pressed, 1 means none pressed

                let joy = !peripherals.borrow().get_current_joystick1_state().bits();

                out &= joy;

                out
            }
            DataPortBackend::CIA2 { .. } => unimpl!(0b1000_0000 => "headless-chicken--cia"),
        }
    }
    fn write(&self, _val: u8) {
        unimpl!(=> "headless-chicken--cia")
    }
}

/// 56323         $DC03          CIDDRB
/// Data Direction Register B
///
/// Bit 0:  Select Bit 0 of Data Port B for input or output (0=input, 1=output)
/// Bit 1:  Select Bit 1 of Data Port B for input or output (0=input, 1=output)
/// Bit 2:  Select Bit 2 of Data Port B for input or output (0=input, 1=output)
/// Bit 3:  Select Bit 3 of Data Port B for input or output (0=input, 1=output)
/// Bit 4:  Select Bit 4 of Data Port B for input or output (0=input, 1=output)
/// Bit 5:  Select Bit 5 of Data Port B for input or output (0=input, 1=output)
/// Bit 6:  Select Bit 6 of Data Port B for input or output (0=input, 1=output)
/// Bit 7:  Select Bit 7 of Data Port B for input or output (0=input, 1=output)
impl<T> Register for DataDirectionB<T> {
    fn read(&self) -> u8 {
        unimpl!(0=> "headless-chicken--cia")
    }
    fn write(&self, _val: u8) {
        unimpl!(=> "headless-chicken--cia")
    }
}

/// 56324         $DC04          TIMALO
/// Timer A (low byte)
///
/// 56325         $DC05          TIMAHI
/// Timer A (high byte)
///
/// 56326         $DC06          TIMBLO
/// Timer B (low byte)
///
/// 56327         $DC07          TIMBHI
/// Timer B (high byte)
impl Register for Timer {
    fn read(&self) -> u8 {
        match self {
            Timer(backend, ByteHalf::Low) => backend.borrow().value as u8,
            Timer(backend, ByteHalf::High) => (backend.borrow().value >> 8) as u8,
        }
    }

    fn write(&self, val: u8) {
        match self {
            Timer(backend, half) => {
                let mut be = backend.borrow_mut();
                match half {
                    ByteHalf::Low => be.latch = (be.latch & 0xff00) | val as u16,
                    ByteHalf::High => be.latch = (be.latch & 0x00ff) | ((val as u16) << 8),
                }
            }
        }
    }
}

/// 56334         $DC0E          CIACRA
/// Control Register A
///
/// Bit 0:  Start Timer A (1=start, 0=stop)
/// Bit 1:  Select Timer A output on Port B (1=Timer A output appears on Bit 6 of
///         Port B)
/// Bit 2:  Port B output mode (1=toggle Bit 6, 0=pulse Bit 6 for one cycle)
/// Bit 3:  Timer A run mode (1=one-shot, 0=continuous)
/// Bit 4:  Force latched value to be loaded to Timer A counter (1=force load
///         strobe)
/// Bit 5:  Timer A input mode (0=count microprocessor cycles, 1=count signals on
///         CNT line at pin 4 of User Port)
/// Bit 6:  Serial Port (56332, $DC0C) mode (1=output, 0=input)
/// Bit 7:  Time of Day Clock frequency (1=50 Hz required on TOD pin, 0=60 Hz)
///
/// Bits 0-3.  This nybble controls Timer A.  Bit 0 is set to 1 to start the timer counting down,
/// and set to 0 to stop it.  Bit 3 sets the timer for one-shot or continuous mode.
///
/// In one-shot mode, the timer counts down to 0, sets the counter value back to the latch value,
/// and then sets Bit 0 back to 0 to stop the timer.  In continuous mode, it reloads the latch value
/// and starts all over again.
///
/// Bits 1 and 2 allow you to send a signal on Bit 6 of Data Port B when the timer counts.  Setting
/// Bit 1 to 1 forces this output (which overrides the Data Direction Register B Bit 6, and the
/// normal Data Port B value).  Bit 2 allows you to choose the form this output to Bit 6 of Data
/// Port B will take.  Setting Bit 2 to a value of 1 will cause Bit 6 to toggle to the opposite
/// value when the timer runs down (a value of 1 will change to 0, and a value of 0 will change to
/// 1).  Setting Bit 2 to a value of 0 will cause a single pulse of a one machine-cycle duration
/// (about a millionth of a second) to occur.
///
/// Bit 4.  This bit is used to load the Timer A counter with the value that was previously written
/// to the Timer Low and High Byte Registers.  Writing a 1 to this bit will force the load (although
/// there is no data stored here, and the bit has no significance on a read).
///
/// Bit 5.  Bit 5 is used to control just what it is Timer A is counting.  If this bit is set to 1,
/// it counts the microprocessor machine cycles (which occur at the rate of 1,022,730 cycles per
/// second).  If the bit is set to 0, the timer counts pulses on the CNT line, which is connected to
/// pin 4 of the User Port.  This allows you to use the CIA as a frequency counter or an event
/// counter, or to measure pulse width or delay times of external signals.
///
/// Bit 6.  Whether the Serial Port Register is currently inputting or outputting data (see the
/// entry for that register at 56332 ($DC0C) for more information) is controlled by this bit.
///
/// Bit 7.  This bit allows you to select from software whether the Time of Day Clock will use a 50
/// Hz or 60 Hz signal on the TOD pin in order to keep accurate time (the 64 uses a 60 Hz signal on
/// that pin).
///
/// 56335         $DC0F          CIACRB
/// Control Register B
///
/// Bit 0:  Start Timer B (1=start, 0=stop)
/// Bit 1:  Select Timer B output on Port B (1=Timer B output appears on
///         Bit 7 of Port B)
/// Bit 2:  Port B output mode (1=toggle Bit 7, 0=pulse Bit 7 for one
///         cycle)
/// Bit 3:  Timer B run mode (1=one-shot, 0=continuous)
/// Bit 4:  Force latched value to be loaded to Timer B counter (1=force
///         load strobe)
/// Bits 5-6:  Timer B input mode
///            00 = Timer B counts microprocessor cycles
///            01 = Count signals on CNT line at pin 4 of User Port
///            10 = Count each time that Timer A counts down to 0
///            11 = Count Timer A 0's when CNT pulses are also present
/// Bit 7:  Select Time of Day write (0=writing to TOD registers sets
///         alarm, 1=writing to TOD registers sets clock)
///
/// Bits 0-3.  This nybble performs the same functions for Timer B that Bits 0-3 of Control Register
/// A perform for Timer A, except that Timer B output on Data Port B appears at Bit 7, and not Bit
/// 6.
///
/// Bits 5 and 6.  These two bits are used to select what Timer B counts.  If both bits are set to
/// 0, Timer B counts the microprocessor machine cycles (which occur at the rate of 1,022,730 cycles
/// per second).  If Bit 6 is set to 0 and Bit 5 is set to 1, Timer B counts pulses on the CNT line,
/// which is connected to pin 4 of the User Port.  If Bit 6 is set to 1 and Bit 5 is set to 0, Timer
/// B counts Timer A underflow pulses, which is to say that it counts the number of times that Timer
/// A counts down to 0.  This is used to link the two numbers into one 32-bit timer that can count
/// up to 70 minutes with accuracy to within 1/15 second.  Finally, if both bits are set to 1, Timer
/// B counts the number of times that Timer A counts down to 0 and there is a signal on the CNT line
/// (pin 4 of the User Port).
///
/// Bit 7.  Bit 7 controls what happens when you write to the Time of Day registers.  If this bit is
/// set to 1, writing to the TOD registers sets the ALARM time.  If this bit is cleared to 0,
/// writing to the TOD registers sets the TOD clock.
impl Register for ControlTimer {
    fn read(&self) -> u8 {
        unimpl!(0 => "headless-chicken--cia")
    }

    fn write(&self, val: u8) {
        let mut timer = self.0.borrow_mut();
        match timer.input_mode {
            TimerInputMode::A(_) => {
                let flags = TimerACtrl::from_bits(val).expect("Every bit should be mapped");

                timer.running = flags.contains(TimerACtrl::START);
                // TODO timer output mode
                timer.underflow_mode = if flags.contains(TimerACtrl::RUN_MODE_ONE_SHOT) {
                    TimerUnderflowMode::OneShot
                } else {
                    TimerUnderflowMode::Continuous
                };

                if flags.contains(TimerACtrl::FORCE_LOAD_LATCH) {
                    timer.value = timer.latch;
                }

                timer.input_mode =
                    TimerInputMode::A(if flags.contains(TimerACtrl::INPUT_COUNT_USER_PORT_CNT_LINE) {
                        TimerInputModeA::UserPortCNTLine
                    } else {
                        TimerInputModeA::MosCycles
                    });

                // TODO serial port output mode
                // TODO tod freq

                // if timer.running {
                //     println!("Starting {:#?}", timer);
                // }
            }
            TimerInputMode::B(_) => unimpl!(=> "headless-chicken--cia"),
        }
    }
}

bitflags! {
    struct TimerACtrl: u8 {
        const START                          = 0b0000_0001;
        const OUTPUT_TO_PORT_B               = 0b0000_0010;
        const OUTPUT_PORT_B_TOGGLE_MODE      = 0b0000_0100;
        const RUN_MODE_ONE_SHOT              = 0b0000_1000;
        const FORCE_LOAD_LATCH               = 0b0001_0000;
        const INPUT_COUNT_USER_PORT_CNT_LINE = 0b0010_0000;
        const SERIAL_PORT_MODE_OUTPUT        = 0b0100_0000;
        const TOD_FREQ_50HZ                  = 0b1000_0000;
    }
}

/// 56328         $DC08          TODTEN
/// Time of Day Clock Tenths of Seconds
///
/// Bits 0-3:  Time of Day tenths of second digit (BCD)
/// Bits 4-7:  Unused
///
/// 56329         $DC09          TODSEC
/// Time of Day Clock Seconds
///
/// Bits 0-3:  Second digit of Time of Day seconds (BCD)
/// Bits 4-6:  First digit of Time of Day seconds (BCD)
/// Bit 7:  Unused
///
/// 56330         $DC0A          TODMIN
/// Time of Day Clock Minutes
///
/// Bits 0-3:  Second digit of Time of Day minutes (BCD)
/// Bits 4-6:  First digit of Time of Day minutes (BCD)
/// Bit 7:  Unused
///
/// 56331         $DC0B          TODHRS
/// Time of Day Clock Hours
///
/// Bits 0-3:  Second digit of Time of Day hours (BCD)
/// Bit 4:  First digit of Time of Day hours (BCD)
/// Bits 5-6:  Unused
/// Bit 7:  AM/PM Flag (1=PM, 0=AM)
impl Register for RTClock {
    fn read(&self) -> u8 {
        unimpl!(0=> "headless-chicken--cia")
    }
    fn write(&self, _val: u8) {
        unimpl!(=> "headless-chicken--cia")
    }
}

impl Register for SerialShift {
    fn read(&self) -> u8 {
        unimpl!(0 => "headless-chicken--cia")
    }
    fn write(&self, _val: u8) {
        unimpl!(=> "headless-chicken--cia")
    }
}

/// Bit 0:  Read / did Timer A count down to 0?  (1=yes)
///         Write/ enable or disable Timer A interrupt (1=enable, 0=disable)
/// Bit 1:  Read / did Timer B count down to 0?  (1=yes)
///         Write/ enable or disable Timer B interrupt (1=enable, 0=disable)
/// Bit 2:  Read / did Time of Day Clock reach the alarm time?  (1=yes)
///         Write/ enable or disable TOD clock alarm interrupt (1=enable,
///         0=disable)
/// Bit 3:  Read / did the serial shift register finish a byte? (1=yes)
///         Write/ enable or disable serial shift register interrupt (1=enable,
///         0=disable)
/// Bit 4:  Read / was a signal sent on the flag line?  (1=yes)
///         Write/ enable or disable FLAG line interrupt (1=enable, 0=disable)
/// Bit 5:  Not used
/// Bit 6:  Not used
/// Bit 7:  Read / did any CIA #1 source cause an interrupt?  (1=yes)
///         Write/ set or clear bits of this register (1=bits written with 1 will
///         be set, 0=bits written with 1 will be cleared)
impl Register for InterruptControl {
    fn read(&self) -> u8 {
        const INTERRUPTED: u8 = 0b1000_0000;

        let mut be = self.0.borrow_mut();
        let result = be.occured.bits() | if be.interrupted { INTERRUPTED } else { 0 };
        be.occured.remove(InterruptSources::all());
        be.interrupted = false;

        result
    }

    fn write(&self, val: u8) {
        bitflags! {struct WriteMode: u8 { const SET = 0b1000_0000; }};
        self.0.borrow_mut().enabled.set(
            InterruptSources::from_bits_truncate(val),
            WriteMode::from_bits_truncate(val).contains(WriteMode::SET),
        )
    }
}
