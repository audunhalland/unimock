use unimock::*;

#[unimock(api=InOptionMock)]
trait InOption {
    fn str(&self, a: &str) -> Option<&str>;
}

#[derive(Eq, PartialEq, Debug)]
pub struct Error;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ErrorClone;

#[unimock(api=InResultMock)]
trait InResult {
    fn bytes(&self) -> Result<&[u8], Error>;
    fn u32_clone(&self) -> Result<&u32, ErrorClone>;
}

// Note: This should only be mockable with static lifetimes
// It should not pick a Mixed output mediator.
#[unimock]
trait InResultWithComplexLifetimes {
    fn foo<'s, 'i>(&'s self, a: &'i str) -> Result<&'s str, &'i str>;
}

#[test]
fn in_option() {
    let u = Unimock::new((
        InOptionMock::str
            .next_call(matching!(_))
            .returns(Some("1".to_string())),
        InOptionMock::str.next_call(matching!(_)).returns(Some("2")),
    ));
    assert_eq!(Some("1"), <Unimock as InOption>::str(&u, ""));
    assert_eq!(Some("2"), <Unimock as InOption>::str(&u, ""));
}

#[test]
fn in_result() {
    let u = Unimock::new((
        InResultMock::bytes
            .next_call(matching!())
            .returns(Ok(vec![42])),
        InResultMock::bytes
            .next_call(matching!())
            .returns(Result::<&[u8], _>::Err(Error)),
        InResultMock::u32_clone
            .each_call(matching!())
            .returns(Ok(42))
            .at_least_times(2),
    ));

    assert_eq!(Ok(vec![42].as_slice()), <Unimock as InResult>::bytes(&u));
    assert_eq!(Err(Error), <Unimock as InResult>::bytes(&u));
    assert_eq!(Ok(&42), <Unimock as InResult>::u32_clone(&u));
    assert_eq!(Ok(&42), <Unimock as InResult>::u32_clone(&u));
}
