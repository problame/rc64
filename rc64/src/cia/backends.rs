use crate::cia::keyboard::KeyboardMatrix;
use crate::interrupt::Interrupt;
use crate::utils::R2C;
use crate::vic20::VIC20;

pub enum DataPortBackend<T> {
    /// Location Range: 56320-56321 ($DC00-$DC01)
    /// CIA #1 Data Ports A and B
    ///
    /// These registers are where the actual communication with outside devices takes place.  Bits of
    /// data written to these registers can be sent to external devices, while bits of data that those
    /// devices send can be read here.
    ///
    /// The keyboard is so necessary to the computer's operation that you may have a hard time thinking
    /// of it as a peripheral device.  Nonetheless, it cannot be directly read by the 6510
    /// microprocessor.  Instead, the keys are connected in a matrix of eight rows by eight columns to
    /// CIA #1 Ports A and B.  The layout of this matrix is shown below.
    ///
    /// WRITE TO PORT A               READ PORT B (56321, $DC01)
    /// 56320/$DC00
    ///          Bit 7   Bit 6   Bit 5   Bit 4   Bit 3   Bit 2   Bit 1   Bit 0
    ///
    /// Bit 7    STOP    Q       C=      SPACE   2       CTRL    <-      1
    ///
    /// Bit 6    /       ^       =       RSHIFT  HOME    ;       *       LIRA
    ///
    /// Bit 5    ,       @       :       .       -       L       P       +
    ///
    /// Bit 4    N       O       K       M       0       J       I       9
    ///
    /// Bit 3    V       U       H       B       8       G       Y       7
    ///
    /// Bit 2    X       T       F       C       6       D       R       5
    ///
    /// Bit 1    LSHIFT  E       S       Z       4       A       W       3
    ///
    /// Bit 0    CRSR DN F5      F3      F1      F7      CRSR RT RETURN  DELETE
    ///
    /// As you can see, there are two keys which do not appear in the matrix.  The SHIFT LOCK key is not
    /// read as a separate key, but rather is a mechanical device which holds the left SHIFT key switch
    /// in a closed position.  The RESTORE key is not read like the other keys either.  It is directly
    /// connected to the NMI interrupt line of the 6510 microprocessor, and causes an NMI interrupt to
    /// occur whenever it is pressed (not just when it is pressed with the STOP key).
    ///
    /// In order to read the individual keys in the matrix, you must first set Port A for all outputs
    /// (255, $FF), and Port B for all inputs (0), using the Data Direction Registers.  Note that this
    /// is the default condition.  Next, you must write a 0 in the bit of Data Port A that corresponds
    /// to the column that you wish to read, and a 1 to the bits that correspond to columns you wish to
    /// ignore.  You will then be able to read Data Port B to see which keys in that column are being
    /// pushed.
    ///
    /// A 0 in any bit position signifies that the key in the corresponding row of the selected column
    /// is being pressed, while a 1 indicates that the key is not being pressed.  A value of 255 ($FF)
    /// means that no keys in that column are being pressed.
    ///
    /// Fortunately for us all, an interrupt routine causes the keyboard to be read, and the results are
    /// made available to the Operating System automatically every 1/60 second.  And even when the
    /// normal interrupt routine cannot be used, you can use the Kernal SCNKEY routine at 65439 ($FF9F)
    /// to read the keyboard.
    ///
    /// These same data ports are also used to read the joystick controllers.  Although common sense
    /// might lead you to believe that you could read the joystick that is plugged into the port marked
    /// Controller Port 1 from Data Port A, and the second joystick from Data Port B, there is nothing
    /// common about the Commodore 64.  Controller Port 1 is read from Data Port B, and Controller Port
    /// 2 is read from CIA #1 Data Port A.
    ///
    /// Joysticks consist of five switches, one each for up, down, right, and left directions, and
    /// another for the fire button.  The switches are read like the key switches--if the switch is
    /// pressed, the corresponding bit will read 0, and if it is not pressed, the bit will be set to 1.
    /// From BASIC, you can PEEK the ports and use the AND and NOT operators to mask the unused bits and
    /// inverse the logic for easier comprehension.  For example, to read the joystick in Controller
    /// Port 1, you could use the statement:
    ///
    /// S1=NOT PEEK(56321)AND15
    ///
    /// The meaning of the possible numbers returned are:
    ///
    ///  0 = none pressed
    ///  1 = up
    ///  2 = down
    ///  4 = left
    ///  5 = up left
    ///  6 = down left
    ///  8 = right
    ///  9 = up right
    /// 10 = down right
    ///
    /// The same technique can be used for joystick 2, by substituting 56320 as the number to PEEK.  By
    /// the way, the 3 and 7 aren't listed because they represent impossible combinations like up-down.
    ///
    /// To read the fire buttons, you can PEEK the appropriate port and use the AND operator to mask all
    /// but bit 4:
    ///
    /// T1=(PEEK(56321)AND16)/16
    ///
    /// The above will return a 0 if the button is pressed, and a 1 if it is not.  Substitute location
    /// 56320 as the location to PEEK for Trigger Button 2.
    ///
    /// Since CIA #1 Data Port B is used for reading the keyboard as well as joystick 1, some confusion
    /// can result.  The routine that checks the keyboard has no way of telling whether a particular bit
    /// was set to 0 by a keypress or one of the joystick switches.  For example, if you plug the
    /// joystick into Controller Port 1 and push the stick to the right, the routine will interpret this
    /// as the 2 key being pressed, because both set the same bit to 0.  Likewise, when you read the
    /// joystick, it will register as being pushed to the right if the 2 key is being pressed.
    ///
    /// The problem of mistaking the keyboard for the joystick can be solved by turning off the keyscan
    /// momentarily when reading the stick with a POKE 56333, 127:POKE 56320,255, and restoring it after
    /// the read with a POKE 56333,129.  Sometimes you can use the simpler solution of clearing the
    /// keyboard buffer after reading the joystick, with a POKE 198,0.
    ///
    /// The problem of mistaking the joystick for a keypress is much more difficult--there is no real
    /// way to turn off the joystick.  Many commercially available games just use Controller Port 2 to
    /// avoid the conflict.  So, if you can't beat them, sit back and press your joystick to the left in
    /// order to slow down a program listing (the keyscan routine thinks that it is the CTRL key).
    ///
    /// As if all of the above were not enough, Port A is also used to control which set of paddles is
    /// read by the SID chip, and to read the paddle fire buttons.  Since there are two paddles per
    /// joystick Controller Port, and only two SID registers for reading paddle positions, there has to
    /// be a method for switching the paddle read from joystick Port 1 to joystick Port 2.
    ///
    /// When Bit 7 of Port A is set to 1 and Bit 6 is cleared to 0, the SID registers will read the
    /// paddles on Port 1.  When Bit 7 is set to 0 and Bit 6 is set to 1, the paddles on Port 2 are read
    /// by the SID chip registers.  Note that this also conflicts with the keyscan routine, which is
    /// constantly writing different values to CIA #1 Data Port A in order to select the keyboard column
    /// to read (most of the time, the value for the last column is written to this port, which
    /// coincides with the selection of paddles on joystick Port 1).  Therefore, in order to get an
    /// accurate reading, you must turn off the keyscan IRQ and select which joystick port you want to
    /// read.  See POTX at 54297 ($D419), which is the SID register where the paddles are read, for the
    /// exact technique.
    ///
    /// Although the SID chip is used to read the paddle settings, the fire buttons are read at CIA #1
    /// Data Ports A and B.  The fire buttons for the paddles plugged into Controller Port 1 are read at
    /// Data Port B (56321, $DC01), while those for the paddles plugged into Controller Port 2 are read
    /// from Data Port A (56320, $DC00).  The fire buttons are read at Bit 2 and Bit 3 of each port (the
    /// same as the joystick left and joystick right switches), and as usual, the bit will read 0 if the
    /// corresponding button is pushed, and 1 if it is not.
    ///
    /// Although only two of the rout paddle values can be read at any one time, you can always read all
    /// four paddle buttons.  See the game paddle input description at 54297 ($D419) for the BASIC
    /// statements used to read these buttons.
    ///
    /// Finally, Data Port B can also be used as an output by either Timer A or B.  It is possible to
    /// set a mode in which the timers do not cause an interrupt when they run down (see the
    /// descriptions of Control Registers A and B at 56334-5 ($DC0E-F)).  Instead, they cause the output
    /// on Bit 6 or 7 of Data Port B to change.  Timer A can be set either to pulse the output of Bit 6
    /// for one machine cycle, or to toggle that bit from 1 to 0 or 0 to 1.  Timer B can use Bit 7 of
    /// this register for the same purpose.
    ///
    /// Location Range: 56322-56323 ($DC02-$DC03)
    /// CIA #1 Data Direction Registers A and B
    ///
    /// These Data Direction Registers control the direction of data flow over Data Ports A and B.  Each
    /// bit controls the direction of the data on the corresponding bit of the port.  If teh bit of the
    /// Direction Register is set to a 1, the corresponding Data Port bit will be used for data output.
    /// If the bit is set to a 0, the corresponding Data Port bit will be used for data input.  For
    /// example, Bit 7 of Data Direction Register A controls Bit 7 of Data Port A, and if that direction
    /// bit is set to 0, Bit 7 of Data Port A will be used for data input.  If the direction bit is set
    /// to 1, however, data Bit 7 on Port A will be used for data output.
    ///
    /// The default setting for Data Direction Register A is 255 (all outputs), and for Data Direction
    /// Register B it is 0 (all inputs).  This corresponds to the setting used when reading the keyboard
    /// (the keyboard column number is written to Data Port A, and the row number is then read in Data
    /// Port B).
    CIA1 { peripherals: R2C<dyn PeripheralDevicesBackend> },

    /// Location Range: 56576-56577 ($DD00-$DD01)
    /// CIA #2 Data Ports A and B
    ///
    /// These registers are where the communication with the Serial Bus,
    /// RS-232 device, and User Port take place.  The Serial Bus is like the
    /// IEEE bus which is used by the PET, in that it allows more than one
    /// device to be connected to the port at a time, in a daisychain
    /// arrangement.  Since each byte of data is sent one bit at a time,
    /// however, the Serial Bus is at least eight times slower than the IEEE.
    /// It is presently used to control the 1541 Disk Drive and 1525 printer,
    /// and other devices (such as printer interface for Centronics- type
    /// parallel pritners and stringy floppy wafer tape storage units) can be
    /// placed on this bus.
    ///
    /// Bits 0 and 1 of CIA #2 Port A have an extremely important function.
    /// As mentioned in the section on the VIC-II chip (53248, $D000), the
    /// video chip can address only 16K of memory at a time, and all graphics
    /// data must be stored in that 16K block in order to be displayed.
    /// Within this area, sprite graphics data may be placed in any of 256
    /// groups of 64 bytes each.  Character data can be stored in any of eight
    /// 2K blocks.  Text screen memory may be in any of 16 1K areas, and
    /// bitmap screen memory may be in either of two 8K sections.
    ///
    /// When you turn the power on, the VIC-II uses the bottom 16K of memory
    /// for graphics.  Unfortunately, this block of memory is also used
    /// extensively for other important purposes.  Though some means of
    /// eliminating these conflicts are discussed above, in many situations
    /// you will want to change from the default 16K bank at the low end of
    /// memory.
    ///
    /// Bits 0 and 1 select the current 16K bank for video memory from the
    /// four possible choices using the following bit patterns:
    ///
    /// 00 (bit value of 0) Bank 3 (49152-65535, $C000-$FFFF)
    /// 01 (bit value of 1) Bank 2 (32768-49151, $8000-$BFFF)
    /// 10 (bit value of 2) Bank 1 (16384-32767, $4000-$7FFF)
    /// 11 (bit value of 3) Bank 0 (0-16383, $0-$3FFF)
    ///
    /// Changing banks.  Once you have selected a bank of 16K to use, the
    /// procedure for making the change from BASIC is as follows:
    ///
    /// 1.  Set the Data Direction Register if necessary.  In order to use
    /// Bits 0 and 1 of Port A to change banks, these bits must be set as
    /// outputs in Data Direction Register A.  Since this is the default
    /// condition on powering-up, this step normally will not be needed.
    ///
    /// 2.  Select a bank.  Banks 0-3 can be chosen by entering the following
    /// lines:
    ///
    /// POKE 56578,PEEK(56578) OR 3: REM SET FOR OUTPUT IF NOT ALREADY
    /// POKE 56576,(PEEK(56576) AND 252) OR (3-BANK): REM BANK IS BANK #, MUST
    ///   BE 0-3
    ///
    /// 3.  Set the VIC-II register for character memory.  As explained at the
    /// entry for location 53272 ($D018), the formula for this is:
    ///
    /// POKE 53272,(PEEK(53272) AND 240) OR TK: REM TK IS 2 KBYTE OFFSET FROM
    ///   BEGINNING OF BLOCK
    ///
    /// 4.  Set the VIC-II register for display memory.  As explained at the
    /// entry for location 53272 ($D018), the formula for this is:
    ///
    /// POKE 53272,(PEEK(53272) AND 15) OR K*16: REM K IS KBYTE OFFSET FROM
    ///   BEGINNING OF BLOCK
    ///
    /// Since steps 3 and 4 operate on the same register, you could combine
    /// these steps and just POKE 53272,(16*K+TK).
    ///
    /// 5.  Set the Operating System pointer for display memory at 648 ($288).
    /// Even though you have just told the VIC-II chip where to display memory
    /// for the screen, the Operating System does not yet know where to write
    /// its text characters.  Let it know with this statement:
    ///
    /// POKE 648,AD/256: REM AD IS THE ACTUAL ADDRESS OF SCREEN MEMORY
    ///
    /// After you make this change, you must watch out for the STOP/RESTORE
    /// key combination.  The BRK initialization changes the screen display
    /// default to location 1024 in Bank 0, but not the Operating System
    /// pointer at 648 ($288).  As a result, what you are typing will not be
    /// displayed on the screen.  The computer will lock up until you turn the
    /// power off and back on again.  The simplest way to avoid this problem
    /// is to disable the RESTORE key entirely (see the entries for 792 ($318)
    /// and 808 ($328) for more information).
    ///
    /// Below is a sample program which switches to Bank 3.  It includes a
    /// machine language transfer routine to move the ROM character set to
    /// RAM, and a short interrupt routine to correct the RESTORE key problem.
    /// After the switch is made, a loop isused to POKE characters into the
    /// new screen memory area.  Next, the character data is slowly erased, to
    /// show that the character set is now in RAM.  Then, a loop is used to
    /// read the locations of the character set, and write to the same
    /// locations.  This demonstrates that the 6510 reads the Kernal ROM when
    /// you PEEK those locations, but POKEs to the RAM which is being
    /// displayed.  Finally, the machine language move is used again to show
    /// how quickly the set is restored.
    ///
    /// 20 FOR I=1 TO 33:READ A:POKE 49152+I,A:NEXT: REM SET UP ML ROUTINE
    /// 30 GOSUB 200: REM ML COPY OF ROM CHARACTER SET TO RAM
    /// 40 POKE 56576,PEEK(56576) AND 252: REM STEP 1, ENABLE BANK 3
    /// 50 POKE 53272,44: REM STEPS 2-3, POINT VIC-II TO SCREEN AND CHARACTER MEMORY
    /// 60 REM SCREEN OFFSET IS 2*16, CHARACTER OFFSET IS 12
    /// 70 POKE 648,200: REM STEP 4, POINT OS TO SCREEN AT 51200 (200*256)
    /// 80 PRINT CHR$(147): REM CLEAR SCREEN
    /// 90 FOR I=53236 TO 53245:READ A:POKE I,A:NEXT: REM NEW INTERRUPT ROUTINE
    /// 100 POKE 53246,PEEK(792):POKE 53247,PEK(793): REM SAVE OLD NMI VECTOR
    /// 110 POKE 792,244:POKE 793,207: REM ROUTE THE INTERRUPT THROUGH THE NEW ROUTINE
    /// 120 FOR I=0 TO 255:POKE 51400+I,I:POKE 55496+I,1:NEXT
    /// 125 REM POKE CHARACTERS TO SCREEN
    /// 130 FOR J=1 TO 8:FOR I=61439+J TO I+2048 STEP 8
    /// 140 POKE I,0:NEXT I,J: REM ERASE CHARACTER SET
    /// 150 FOR I=61440 TO I+2048:POKE I,PEEK(I):NEXT: REM POKE ROM TO RAM
    /// 160 GOSUB 200:END: REM RESTORE CHARACTER SET
    /// 200 POKE 56334,PEEK(56334) AND 254: REM DISABLE INTERRUPTS
    /// 210 POKE 1,PEEK(1) AND 251:REM SWITCH CHARACTER ROM INTO 6510 MEMORY
    /// 220 SYS 49152: REM COPY ROM CHARACTER SET TO RAM AT 61440
    /// 230 POKE 1,PEEK(1) OR 4: REM SWITCH CHARACTER ROM OUT OF 6510 MEMORY
    /// 240 POKE 56334,PEEK(56334)OR 1: REM ENABLE INTERRUPTS
    /// 250 RETURN
    /// 300 REM DATA FOR ML PROGRAM TO COPY CHARACTER SET TO RAM
    /// 310 DATA169,0,133,251,133,253,169,208,133,252,169,240,133,254,162,16
    /// 320 DATA160,0,177,251,145,253,136,208,249,230,252,230,254,202,208,240,96
    /// 330 REM NEXT IS ML PROGRAM TO MAKE THE RESTORE KEY RESET OS POINTER TO SCREEN
    /// 340 DATA 72,169,4,141,136,02,104,108,254,207
    ///
    /// See also the sample program showing how to configure your 64 like a
    /// PET at location 43 ($2B).
    CIA2 {
        vic: R2C<VIC20<T>>,
        serial_bus: SerialBusBackend,
        rs232: RS232Backend,
        userport: UserportBackend,
    },
}

pub trait PeripheralDevicesBackend {
    fn get_current_keyboard_matrix(&self) -> KeyboardMatrix;
}

impl<T> DataPortBackend<T> {
    pub(super) fn cycle(&mut self) -> Option<Interrupt> {
        // if let DataPortBackend::CIA1 { peripherals } = self {
        //     println!("{}", peripherals.borrow().get_current_keyboard_matrix());
        // }
        None
    }
}

/// Location Range: 56324-56327 ($DC04-$DC07)
/// Timers A and B Low and High Bytes
///
/// These four timer registers (two for each timer) have different functions depending on whether
/// you are reading from them or writing to them.  When you read from these registers, you get the
/// present value of the Timer Counter (which counts down from its initial value to 0).  When you
/// write data to these registers, it is stored in the Timer Latch, and from there it can be used to
/// load the Timer Counter using the Force Load bit of Control Register A or B (see 56334-5
/// ($DC0E-F) below).
///
/// These interval timers can hold a 16-bit number from 0 to 65535, in normal 6510 low-byte,
/// high-byte format (VALUE=LOW BYTE+256*HIGH BYTE).
///
/// Timer B is even more versatile.  In addition to these two sources, Timer B can count the number
/// of times that Timer A goes to 0.  By setting Timer A to count the microprocessor clock, and
/// setting Timer B to count the number of times that Timer A zeros, you effectively link the two
/// timers into one 32-bit timer that can count up to 70 minutes with accuracy within 1/15 second.
///
/// In the 64, CIA #1 Timer A is used to generate the interrupt which drives the routine for reading
/// the keyboard and updating the software clock.  Both Timers A and B are also used for the timing
/// of the routines that read and write tape data.  Normally, Timer A is set for continuous
/// operation, and latched with a value of 149 in the low byte and 66 in the high byte, for a total
/// Latch Value of 17045.  This means that it is set to count to 0 every 17045/1022730 seconds, or
/// approximately 1/60 second.
///
/// For tape reads and writes, the tape routines take over the IRQ vectors.  Even though the tape
/// write routines use the on-chip I/O port at location 1 for the actual data output to the
/// cassette, reading and writing to the cassette uses both CIA #1 Timer A and Timer B for timing
/// the I/O routines.
pub struct TimerBackend {
    pub(super) latch: u16,
    pub(super) value: u16,
    pub(super) running: bool,
    pub(super) underflow_mode: TimerUnderflowMode,
    pub(super) underflow_action: Option<R2C<TimerBackend>>,
    pub(super) input_mode: TimerInputMode,

    pub(super) interrupt_be: R2C<InterruptBackend>,
}

#[derive(Clone, Copy)]
pub(super) enum TimerInputMode {
    A(TimerInputModeA),
    B(TimerInputModeB),
}

#[derive(Clone, Copy)]
pub(super) enum TimerInputModeA {
    MosCycles,
    UserPortCNTLine,
}

#[derive(Clone, Copy)]
pub(super) enum TimerInputModeB {
    MosCycles,
    UserPortCNTLine,
    OtherTimer,
    OtherTimerAndCNT,
}

impl TimerInputMode {
    fn count_mos_cycles(&self) -> bool {
        match self {
            TimerInputMode::A(TimerInputModeA::MosCycles)
            | TimerInputMode::B(TimerInputModeB::MosCycles) => true,
            _ => false,
        }
    }

    fn stepped_by_other_timer(&self) -> bool {
        match self {
            TimerInputMode::B(TimerInputModeB::OtherTimer) => true,
            _ => false,
        }
    }

    fn interrupt_source(&self) -> InterruptSources {
        match self {
            TimerInputMode::A(_) => InterruptSources::TIMER_A,
            TimerInputMode::B(_) => InterruptSources::TIMER_B,
        }
    }
}

#[derive(Clone, Copy)]
pub(super) enum TimerUnderflowMode {
    OneShot,
    Continuous,
}

impl TimerBackend {
    pub(super) fn new(other_timer: Option<R2C<TimerBackend>>, interrupt_be: R2C<InterruptBackend>) -> Self {
        // TODO What are the correct defaults here?
        TimerBackend {
            latch: 0,
            value: 0,
            running: false,
            underflow_mode: TimerUnderflowMode::OneShot,
            input_mode: match &other_timer {
                None => TimerInputMode::A(TimerInputModeA::MosCycles),
                Some(_) => TimerInputMode::B(TimerInputModeB::MosCycles),
            },
            underflow_action: other_timer,
            interrupt_be,
        }
    }

    /// Once the Timer Counter is set to an initial
    /// value, and the timer is started, the timer will count down one number every microprocessor clock
    /// cycle.  Since the clock speed of the 64 (using the American NTSC television standard) is
    /// 1,022,730 cycles per second, every count takes approximately a millionth of a second.  The
    /// formula for calculating the amount of time it will take for the timer to count down from its
    /// latch value to 0 is:
    ///
    /// TIME=LATCH VALUE/CLOCK SPEED
    ///
    /// where LATCH VALUE is the value written to the low and high timer registers (LATCH VALUE=TIMER
    /// LOW+256*TIMER HIGH), and CLOCK SPEED is 1,022,370 cycles per second for American (NTSC) standard
    /// television monitors, or 985,250 for European (PAL) monitors.
    ///
    /// When Timer Counter A or B gets to 0, it will set Bit 0 or 1 in the Interrupt Control Register at
    /// 56333 ($DC0D).  If the timer interrupt has been enabled (see 56333 ($DC0D)), an IRQ will take
    /// place, and the high bit of the Interrupt Control Register will be set to 1.  Alternately, if the
    /// Port B output bit is set, the timer will write data to Bit 6 or 7 of Port B.  After the timer
    /// gets to 0, it will reload the Timer Latch Value, and either stop or count down again, depending
    /// on whether it is in one-shot or continuous mode (determined by Bit 3 of the Control Register).
    ///
    /// Although usually a timer will be used to count the microprocessor cycles, Timer A can count
    /// either the microprocessor clock cycles or external pulses on the CTN line, which is connected to
    /// pin 4 of the User Port.
    pub(super) fn cycle(&mut self) -> Option<Interrupt> {
        if self.running && self.input_mode.count_mos_cycles() {
            self.dec()
        } else {
            None
        }
    }

    fn step(&mut self) -> Option<Interrupt> {
        if self.running && self.input_mode.stepped_by_other_timer() {
            self.dec()
        } else {
            None
        }
    }

    fn dec(&mut self) -> Option<Interrupt> {
        match self.value.checked_sub(1) {
            Some(value) => {
                self.value = value;
                None
            }
            None => {
                self.value = self.latch;
                self.running = match self.underflow_mode {
                    TimerUnderflowMode::OneShot => false,
                    TimerUnderflowMode::Continuous => true,
                };

                if let Some(other_timer) = self.underflow_action.as_ref() {
                    other_timer.borrow_mut().step();
                }

                self.interrupt_be.borrow_mut().generate(self.input_mode.interrupt_source())
            }
        }
    }
}

/// Location Range: 56328-56331 ($DC08-$DC0B)
/// Time of Day Clock (TOD)
///
/// In addition to the two general-purpose timers, the 6526 CIA chip has a special-purpose Time of
/// Day Clock, which keeps time in a format that humans can understand a little more easily than
/// microseconds.
///
/// This Time of Day Clock even has an alarm, which can cause an interrupt at a specific time.  It
/// is organized in four registers, one each for hours, minutes, seconds, and tenths of seconds.
/// Each register reads out in Binary Coded Decimal (BCD) format, for easier conversion to ASCII
/// digits.  A BCD byte is divided into two nybbles, each of which represents a single digit in base
/// 10.  Even though a four-bit nybble can hold a number from 0 to 15, only the base 10 digits of
/// 0-9 are used.  Therefore, 10 0'clock would be represented by a byte in the hours register with
/// the nybbles 0001 and 0000, which stand for the digits 1 and 0.  The binary value of this byte
/// would be 16 (16 times the high nybble plus the low nybble).  Each of the other registers
/// operates in the same manner.  In addition, Bit 7 of the hours register is used as an AM/PM flag.
/// If that bit is set to 1, it indicates PM, and if it is set to 0, the time is AM.
///
/// The Time of Day Clock Registers can be used for two purposes, depending on whether you are
/// reading them or writing to them.  If you are reading them, you will always be reading the time.
/// There is a latching feature associated with reading the hours register in order to solve the
/// problem of the time changing while you are reading the registers.  For example, if you were
/// reading the hours register just as the time was changing from 10:59 to 11:00, it is possible
/// that you would read the 10 in the hours register, and by the time you read the minutes register
/// it would have changed from 59 to 00.  Therefore, you would read 10:00 instead of either 10:59 or
/// 11:00.
///
/// To prevent this kind of mistake, the Time of Day Clock Registers stop updating as soon as you
/// read the hours register, and do not start again until you read the tenths of seconds register.
/// Of course, the clock continues to keep time internally even though it does not update the
/// registers.  If you want to read only minutes, or seconds or tenths of seconds, there is no
/// problem, and no latching will occur.  But anytime you read hours, you must follow it by reading
/// tenths of seconds, even if you don't care about them, or else the registers will not continue to
/// update.
///
/// Writing to these registers either sets the time or the alarm, depending on the setting of Bit 7
/// of Control Register B (56335, $DC0F).  If that bit is set to 1, writing to the Time of Day
/// registers sets the alarm.  If the bit is set to 0, writing to the Time of Day registers sets the
/// Time of Day clock.  In either case, as with reading the registers, there is a latch function.
/// This function stops the clock from updating when you write to the hours register.  The clock
/// will not start again until you write to the tenths of seconds registers.
///
/// The only apparent use of the Time of Day Clock by the 64's Operating System is in the BASIC RND
/// statement.  There, the seconds and tenths of seconds registers are read and their values used as
/// part of the seed value for the RND(0) command.
///
/// Nonetheless, this clock can be an invaluable resource for the 64 user.  It will keep time more
/// accurately than the software clock maintained at locations 60-162 ($A0-$A2) by the Timer A
/// interrupt routine.  And unlike that software clock, the Time of Day Clock will not be disturbed
/// when I/O operations disrupt the Timer A IRQ, or when the IRQ vector is diverted elsewhere.  Not
/// even a cold start RESET will disrupt the time.  For game timers, just set the time for
/// 00:00:00:0 and it will keep track of elapsed time in hours, minutes, seconds and tenths of
/// seconds format.
///
/// The following digital clock program, written in BASIC, will demonstrate the use of these timers:
///
/// 10 PRINT CHR$(147):GOSUB 200
/// 20 H=PEEK(56331):POKE 1238,(H AND 16)/16+48:POKE 1239,(H AND 15)+48
/// 30 M=PEEK(56330):POKE 1241,(M AND 240)/16+48:POKE 1242,(M AND 15)+48
/// 40 S=PEEK(56329):POKE 1244,(S AND 240)/16+48:POKE 1245,(S AND 15)+48
/// 50 T=PEEK(56328)AND15:POKE 1247,T+48:GOTO 20
/// 200 INPUT"WHAT IS THE HOUR";H$:IF H$="" THEN 200
/// 210 H=0:IF LEN(H$)>1 THEN H=16
/// 220 HH=VAL(RIGHT$(H$,1)):H=H+HH:POKE56331,H
/// 230 INPUT "WHAT IS THE MINUTE";M$:IF M$=""THEN 200
/// 240 M=0:IF LEN(M$)>1 THEN M=16*VAL(LEFT$(M$,1))
/// 250 MM=VAL(RIGHT$(M$,1)):M=M+MM:POKE56330,M
/// 260 INPUT "WHAT IS THE SECOND";S$:IF S$=""THEN 200
/// 270 S=0:IF LEN(S$)>1 THEN S=16*VAL(LEFT$(S$,1))
/// 280 SS=VAL(RIGHT$(S$,1)):S=S+SS:POKE56329,S:POKE56328,0
/// 290 POKE 53281,1:PRINT CHR$(147):POKE 53281,6
/// 300 POKE 1240,58:POKE 1243,58:POKE 1246,58:GOTO 20
#[derive(Default)]
pub struct TimeOfDayBackend {}

impl TimeOfDayBackend {
    pub(super) fn cycle(&mut self) -> Option<Interrupt> {
        // TODO self.interrupt_be.generate(InterruptSources::TOD_ALARM)
        None
    }
}

/// 56332         $DC0C          CIASDR
/// Serial Data Port
///
/// The CIA chip has an on-chip serial port, which allows you to send or receive a byte of data one
/// bit at a time, with the most significant bit (Bit 7) being transferred first.  Control Register
/// A at 56334 ($DC0E) allows you to choose input or output modes.  In input mode, a bit of data is
/// read from the SP line (pin 5 of the User Port) whenever a signal on the CNT line (pin 4) appears
/// to let you know that it is time for a read.  After eight bits are received this way, the data is
/// placed in the Serial Port Register, and an interrupt is generated to let you know that the
/// register should be read.
///
/// In output mode, you write data to the Serial Port Register, and it is sent out over the SP line
/// (pin 5 of the User Port), using Timer A for the baud rate generator.  Whenever a byte of data is
/// written to this register, transmission will start as long as Timer A is running and in
/// continuous mode.  Data is sent at half the Timer A rage, and an output will appear on the CNT
/// line (pin 4 of the User Port) whenever a bit is sent.  After all eight bits have been sent, an
/// interrupt is generated to indicate that it is time to load the next byte to send into the Serial
/// Register.
///
/// The Serial Data Register is not used by the 64, which does all of its serial I/O through the
/// regular data ports.
#[derive(Default)]
pub struct SerialShiftBackend {}

/// 56333         $DC0D          CIAICR
/// Interrupt Control Register
///
/// This register is used to control the five interrupt sources on the 6526 CIA chip.  These sources
/// are Timer A, Timer B, the Time of Day Clock, the Serial Register, and the FLAG line.  Timers A
/// and B cause an interrupt when they count down to 0.  The Time of Day Clock generates an
/// interrupt when it reaches the ALARM time.  The Serial Shift Register interrupts when it compiles
/// eight bits of input or output.  An external signal pulling the CIA hardware line called FLAG low
/// will also cause an interrupt (on CIA #1, this FLAG line is connected to the Cassette Read line
/// of the Cassette Port).
///
/// Even if the condition for a particular interrupt is satisfied, the interrupt must still be
/// enabled for an IRQ actually to occur.  This is done by writing to the Interrupt Control
/// Register.  What happens when you write to this register depends on the way that you set Bit 7.
/// If you set it to 0, any other bit that was written to with a 1 will be cleared, and the
/// corresponding interrupt will be disabled.  If you set Bit 7 to 1, any bit written to with a 1
/// will be set, and the corresponding interrupt will be enabled.  In either case, the interrupt
/// enable flags for those bits written to with a 0 will not be affected.
///
/// For example, in order to disable all interrupts from BASIC, you could POKE 56333, 127.  This
/// sets Bit 7 to 0, which clears all of the other bits, since they are all written with 1's.  Don't
/// try this from BASIC immediate mode, as it will turn off Timer A which causes the IRQ for reading
/// the keyboard, so that it will in effect turn off the keyboard.
///
/// To turn on the Timer A interrupt, a program could POKE 56333,129.  Bit 7 is set to 1 and so is
/// Bit 0, so the interrupt which corresponds to Bit 0 (Timer A) is enabled.
///
/// When you read this register, you can tell if any of the conditions for a CIA Interrupt were
/// satisfied because the corresponding bit will be set to a 1.  For example, if Timer A counts down
/// to 0, Bit 0 of this register will be set to 1.  If, in addition, the mask bit that corresponds
/// to that interrupt source is set to 1, and an interrupt occurs, Bit 7 will also be set.  This
/// allows a multi-interrupt system to read one bit and see if the source of a particular interrupt
/// was CIA #1.  You should note, however, that reading this register clears it, so you should
/// preserve its contents in RAM if you want to test more than one bit.
#[derive(Default)]
pub struct InterruptBackend {
    pub occured: InterruptSources,
    pub interrupted: bool,
    pub enabled: InterruptSources,
}

bitflags! {
    #[derive(Default)]
    pub struct InterruptSources: u8 {
        const TIMER_A    = 0b00000001;
        const TIMER_B    = 0b00000010;
        const TOD_ALARM  = 0b00000100;
        const SERIAL_REG = 0b00001000;
        const FLAG_LINE  = 0b00010000;
    }
}

impl InterruptBackend {
    pub fn generate(&mut self, kind: InterruptSources) -> Option<Interrupt> {
        self.occured.insert(kind);
        if self.enabled.contains(kind) {
            // TODO Should this be set even if IRQs are masked in the CPU?
            self.interrupted = true;
            Some(Interrupt)
        } else {
            None
        }
    }
}

/// Data Port A is used for communication with the Serial Bus.  Bits 5 and
/// 7 are used for Serial Bus Data Output and Input, respectively, and
/// Bits 4 and 6 are used for the Serial Bus Clock Pulse Output and Input.
/// Bit 3 of Data Port A is used to send the ATN signal on the Serial Bus.
#[derive(Default)]
pub struct SerialBusBackend {}
/// The 64 has built-in software to handle RS-232 communications through a
/// modem or other device plugged in the RS-232/User Port.  The RS-232
/// device uses Bit 2 of Data Port A for data output (it is the only line
/// from Port A that is connected to the RS-232/User Port jack).  It also
/// makes heavy use of Port B, using Bit 7 for the Data Set Ready (DSR)
/// signal, Bit 6 for the Clear to Send (CTS), Bit 4 for the Carrier
/// Detect (DCD), Bit 3 for the Ring Indicator (RI), Bit 2 for Data
/// Terminal Ready (DTR), Bit 1 for Request to Send (RTS), and Bit 0 for
/// data input.  See locations 659-660 ($293-$294) for more details on the
/// RS-232 device.
///
/// All of the data lines which the RS-232 device uses are also available
/// to the user as part of the User Port.  All of the Port B data lines,
/// and Bit 2 of Port A, are brought out to the User Port connector on the
/// back of the 64.  These data bits are utilized in the normal way:  The
/// port connections are made to TTL-level input or output devices, and
/// the direction of data is determined by the Data Direction Registers.
#[derive(Default)]
pub struct RS232Backend {}
/// In addition, the User Port has pins connected to the two CIA Serial
/// Ports (whose eight-bit shift registers are well-suited for
/// serial-to-parallel and parallel-to-serial conversion),and the two CNT
/// lines which aid in the operation of the Serial Ports.  The CNT lines
/// can also be used in conjunction with the CIA Timers, and allow them to
/// be used as frequency counters, event counters, interval timers, etc.
/// The advanced features of the CIA chip make almost any type of
/// interfacing application possible, and in the near future we will
/// probably see many interesting applications for the User Port on the
/// 64.  A pin description of tthe User Port connector is provided below:
///
/// User         RS-232
/// Port  CIA    DB-25
/// Pin   Line   Pin     Description
///
/// 1                    Ground
/// 2                    +5 Volts (100 milliamps maximum)
/// 3                    RESET (grounding this pin causes a cold start)
/// 4     CNT1           CIA #1 Serial Port and Timer Counter
/// 5     SP1            CIA #1 Serial Data Port
/// 6     CNT2           CIA #2 Serial Port and Timer Counter
/// 7     SP2            CIA #2 Serial Data Port
/// 8     PC2            CIA #2 handshaking line
/// 9                    Connected to the ATN line of the Serial Bus
/// 10                   9 Volts AC (+ phase, 50 milliamps maximum)
/// 11                   9 volts AC (- phase, 50 milliamps maximum)
/// 12                   Ground
/// A            1       Ground
/// B     FLAG2          CIA #2 handshaking line
/// C     PB0    3       Port B Bit 0--RS-232 Received Data (SIN)
/// D     PB1    4       Port B Bit 1--RS-232 Request to Send (RTS)
/// E     PB2    20      Port B Bit 2--RS-232 Data Terminal Ready (DTR)
/// F     PB3    22      Port B Bit 3--RS-232 Ring Indicator (RI)
/// H     PB4    8       Port B Bit 4--RS-232 Carrier Detect (DCD)
/// J     PB5            Port B Bit 5
/// K     PB6    5       Port B Bit 6--RS-232 Clear to Send (CTS)
/// L     PB7    6       Port B Bit 7--RS-232 Data Set Ready (DSR)
/// M     PA2    2       Port A Bit 2--RS-232 Transmitted Data (Sout)
/// N            7       Ground
///
/// One of the handshaking lines on the above chart, PC2, was not covered
/// in the discussion of CIA #1, because that line of CIA #1 is not
/// connected to anything.  The CIA #2 PC line is accessible from the User
/// Port, however.  This line will go low for one cycle following a read
/// or write of Port B on CIA #2.  This signal lets external devices know
/// when data has been read or written.
#[derive(Default)]
pub struct UserportBackend {}
