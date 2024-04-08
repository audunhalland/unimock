//! Mock APIs for `embedded-hal` traits

/// Mock APIs for `embedded_hal::delay` traits
#[cfg(feature = "mock-embedded-hal-1")]
pub mod delay {
    use crate::unimock;

    #[unimock(prefix=crate, api=DelayNsMock, mirror=embedded_hal_1::delay::DelayNs)]
    pub trait DelayNs {
        fn delay_ns(&mut self, ns: u32);

        fn delay_us(&mut self, us: u32) {}
        fn delay_ms(&mut self, ms: u32) {}
    }
}

/// Mock APIs for `embedded_hal::digital` traits
#[cfg(feature = "mock-embedded-hal-1")]
pub mod digital {
    use crate::{private::DefaultImplDelegator, unimock, Unimock};
    use embedded_hal_1::digital::{ErrorKind, ErrorType, PinState};

    #[unimock(prefix=crate, api=ErrorMock, mirror=embedded_hal_1::digital::Error)]
    pub trait Error {
        fn kind(&self) -> ErrorKind;
    }

    #[unimock(prefix=crate, api=InputPinMock, mirror=embedded_hal_1::digital::InputPin)]
    pub trait InputPin: ErrorType {
        fn is_high(&mut self) -> Result<bool, <Self as ErrorType>::Error>;
        fn is_low(&mut self) -> Result<bool, <Self as ErrorType>::Error>;
    }

    #[unimock(prefix=crate, api=OutputPinMock, mirror=embedded_hal_1::digital::OutputPin)]
    pub trait OutputPin: ErrorType {
        // Required methods
        fn set_low(&mut self) -> Result<(), <Self as ErrorType>::Error>;
        fn set_high(&mut self) -> Result<(), <Self as ErrorType>::Error>;

        // Provided method
        fn set_state(&mut self, state: PinState) -> Result<(), <Self as ErrorType>::Error> {}
    }

    #[unimock(prefix=crate, api=StatefulOutputPinMock, mirror=embedded_hal_1::digital::StatefulOutputPin)]
    pub trait StatefulOutputPin: OutputPin {
        // Required methods
        fn is_set_high(&mut self) -> Result<bool, <Self as ErrorType>::Error>;
        fn is_set_low(&mut self) -> Result<bool, <Self as ErrorType>::Error>;

        // Provided method
        fn toggle(&mut self) -> Result<(), <Self as ErrorType>::Error> {}
    }

    impl ErrorType for Unimock {
        type Error = Unimock;
    }
    impl ErrorType for DefaultImplDelegator {
        type Error = Unimock;
    }
}

/// Mock APIs for `embedded_hal::i2c` traits
#[cfg(feature = "mock-embedded-hal-1")]
pub mod i2c {
    use crate::{private::DefaultImplDelegator, unimock, Unimock};
    use embedded_hal_1::i2c::{AddressMode, ErrorKind, ErrorType, Operation};

    #[unimock(prefix=crate, api=ErrorMock, mirror=embedded_hal_1::i2c::Error)]
    pub trait Error {
        fn kind(&self) -> ErrorKind;
    }

    #[unimock(prefix=crate, api=I2cMock, mirror=embedded_hal_1::i2c::I2c)]
    pub trait I2c<A: AddressMode>: ErrorType {
        // Required methods
        fn transaction(
            &mut self,
            address: A,
            operations: &mut [Operation<'_>],
        ) -> Result<(), <Self as ErrorType>::Error>;

        // Provided methods
        fn read(&mut self, address: A, read: &mut [u8]) -> Result<(), <Self as ErrorType>::Error> {}

        fn write(&mut self, address: A, write: &[u8]) -> Result<(), <Self as ErrorType>::Error> {}

        fn write_read(
            &mut self,
            address: A,
            write: &[u8],
            read: &mut [u8],
        ) -> Result<(), <Self as ErrorType>::Error> {
        }
    }

    impl ErrorType for Unimock {
        type Error = Unimock;
    }
    impl ErrorType for DefaultImplDelegator {
        type Error = Unimock;
    }
}

/// Mock APIs for `embedded_hal::pwm` traits
#[cfg(feature = "mock-embedded-hal-1")]
pub mod pwm {
    use crate::{private::DefaultImplDelegator, unimock, Unimock};
    use embedded_hal_1::pwm::{ErrorKind, ErrorType};

    #[unimock(prefix=crate, api=ErrorMock, mirror=embedded_hal_1::pwm::Error)]
    pub trait Error {
        fn kind(&self) -> ErrorKind;
    }

    #[unimock(prefix=crate, api=SetDutyCycleMock, mirror=embedded_hal_1::pwm::SetDutyCycle)]
    pub trait SetDutyCycle: ErrorType {
        // Required methods
        fn max_duty_cycle(&self) -> u16;
        fn set_duty_cycle(&mut self, duty: u16) -> Result<(), <Self as ErrorType>::Error>;

        // Provided methods
        fn set_duty_cycle_fully_off(&mut self) -> Result<(), <Self as ErrorType>::Error> {}

        fn set_duty_cycle_fully_on(&mut self) -> Result<(), <Self as ErrorType>::Error> {}

        fn set_duty_cycle_fraction(
            &mut self,
            num: u16,
            denom: u16,
        ) -> Result<(), <Self as ErrorType>::Error> {
        }

        fn set_duty_cycle_percent(
            &mut self,
            percent: u8,
        ) -> Result<(), <Self as ErrorType>::Error> {
        }
    }

    impl ErrorType for Unimock {
        type Error = Unimock;
    }
    impl ErrorType for DefaultImplDelegator {
        type Error = Unimock;
    }
}

/// Mock APIs for `embedded_hal::spi` traits
#[cfg(feature = "mock-embedded-hal-1")]
pub mod spi {
    use crate::{private::DefaultImplDelegator, unimock, Unimock};
    use embedded_hal_1::spi::{ErrorKind, ErrorType, Operation};

    #[unimock(prefix=crate, api=ErrorMock, mirror=embedded_hal_1::spi::Error)]
    pub trait Error {
        fn kind(&self) -> ErrorKind;
    }

    #[unimock(prefix=crate, api=SpiBusMock, mirror=embedded_hal_1::spi::SpiBus)]
    pub trait SpiBus<Word: Copy + 'static> {
        // Required methods
        fn read(&mut self, words: &mut [Word]) -> Result<(), <Self as ErrorType>::Error>;

        fn write(&mut self, words: &[Word]) -> Result<(), <Self as ErrorType>::Error>;

        fn transfer(
            &mut self,
            read: &mut [Word],
            write: &[Word],
        ) -> Result<(), <Self as ErrorType>::Error>;

        fn transfer_in_place(
            &mut self,
            words: &mut [Word],
        ) -> Result<(), <Self as ErrorType>::Error>;
        fn flush(&mut self) -> Result<(), <Self as ErrorType>::Error>;
    }

    #[unimock(prefix=crate, api=SpiDeviceMock, mirror=embedded_hal_1::spi::SpiDevice)]
    pub trait SpiDevice<Word: Copy + 'static> {
        // Required method
        fn transaction(
            &mut self,
            operations: &mut [Operation<'_, Word>],
        ) -> Result<(), <Self as ErrorType>::Error>;

        // Provided methods
        fn read(&mut self, buf: &mut [Word]) -> Result<(), <Self as ErrorType>::Error> {}

        fn write(&mut self, buf: &[Word]) -> Result<(), <Self as ErrorType>::Error> {}

        fn transfer(
            &mut self,
            read: &mut [Word],
            write: &[Word],
        ) -> Result<(), <Self as ErrorType>::Error> {
        }

        fn transfer_in_place(
            &mut self,
            buf: &mut [Word],
        ) -> Result<(), <Self as ErrorType>::Error> {
        }
    }

    impl ErrorType for Unimock {
        type Error = Unimock;
    }
    impl ErrorType for DefaultImplDelegator {
        type Error = Unimock;
    }
}
