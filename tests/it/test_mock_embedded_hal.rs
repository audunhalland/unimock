use embedded_hal_1::digital::{InputPin, OutputPin};
use unimock::Unimock;

use unimock::{mock::embedded_hal_1::*, *};

#[test]
fn test_pins() {
    let mut u = Unimock::new((
        digital::InputPinMock::is_high
            .next_call(matching!())
            .returns(Ok(false)),
        digital::OutputPinMock::set_high
            .next_call(matching!())
            .returns(Ok(())),
        digital::InputPinMock::is_high
            .next_call(matching!())
            .returns(Ok(true)),
    ));

    assert!(!u.is_high().unwrap());
    u.set_high().unwrap();
    assert!(u.is_high().unwrap());
}

#[test]
fn test_pin_fail() {
    let mut u = Unimock::new(
        digital::OutputPinMock::set_high
            .next_call(matching!())
            .returns(Err(Unimock::new(()))),
    );

    u.set_high().unwrap_err();
}
