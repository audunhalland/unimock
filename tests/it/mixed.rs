//! tests unimock support for borrowed generic parameters, e.g.:
//!
//! `Option<&T>`
//! `Poll<Result<&T, E>>`

use core::task::Poll;

use unimock::alloc::{vec, ToString, Vec};
use unimock::*;

mod clone {

    #[derive(Eq, PartialEq, Debug)]
    pub struct Nope;

    #[derive(Clone, Eq, PartialEq, Debug)]
    pub struct Sure;
}

#[unimock(api=InOptionMock)]
trait InOption {
    fn str(&self, a: &str) -> Option<&str>;
    fn not_clone(&self) -> Option<&clone::Nope>;
}

#[test]
fn in_option() {
    let u = Unimock::new((
        InOptionMock::str
            .next_call(matching!(_))
            .returns(Some("1".to_string())),
        InOptionMock::str.next_call(matching!(_)).returns(Some("2")),
        InOptionMock::str
            .next_call(matching!(_))
            .returns(None::<&str>),
    ));
    assert_eq!(Some("1"), u.str(""));
    assert_eq!(Some("2"), u.str(""));
    assert_eq!(None, u.str(""));
}

// This test demonstrates that a `T: Clone` bound is not necessary
// on each_call for `Option<&T>`, since the outer option
// can be generically reconstructed from the inner borrowed one
#[test]
fn in_option_not_clone_can_clone_anyway() {
    let u = Unimock::new(
        InOptionMock::not_clone
            .each_call(matching!())
            .returns(Some(clone::Nope)),
    );
    assert_eq!(Some(&clone::Nope), <Unimock as InOption>::not_clone(&u));
    assert_eq!(Some(&clone::Nope), <Unimock as InOption>::not_clone(&u));
}

#[unimock(api=InResultMock)]
trait InResult {
    fn bytes(&self) -> Result<&[u8], clone::Nope>;
    fn u32_clone(&self) -> Result<&u32, clone::Sure>;
    fn ok_no_clone(&self, ok: bool) -> Result<&clone::Nope, clone::Sure>;
    fn err_no_clone(&self) -> Result<&clone::Nope, clone::Nope>;
}

// Note: This should only be mockable with static lifetimes
// It should not pick a Mixed output mediator.
#[unimock]
trait InResultWithComplexLifetimes {
    fn foo<'s, 'i>(&'s self, a: &'i str) -> Result<&'s str, &'i str>;
}

#[test]
fn in_result() {
    let u = Unimock::new((
        InResultMock::bytes
            .next_call(matching!())
            .returns(Ok::<_, clone::Nope>(vec![42])),
        InResultMock::bytes
            .next_call(matching!())
            .returns(Err::<&[u8], _>(clone::Nope)),
        InResultMock::u32_clone
            .each_call(matching!())
            .returns(Ok::<_, clone::Sure>(42))
            .at_least_times(2),
    ));

    assert_eq!(Ok(vec![42].as_slice()), <Unimock as InResult>::bytes(&u));
    assert_eq!(Err(clone::Nope), <Unimock as InResult>::bytes(&u));
    assert_eq!(Ok(&42), <Unimock as InResult>::u32_clone(&u));
    assert_eq!(Ok(&42), <Unimock as InResult>::u32_clone(&u));
}

#[test]
fn in_result_clone_acrobatics() {
    let u = Unimock::new((
        InResultMock::ok_no_clone
            .each_call(matching!(true))
            .returns(Ok::<_, clone::Sure>(clone::Nope)),
        InResultMock::ok_no_clone
            .each_call(matching!(false))
            .returns(Err::<&clone::Nope, _>(clone::Sure)),
        InResultMock::err_no_clone
            .some_call(matching!()) // note: .each_call is impossible
            .returns(Err::<&clone::Nope, _>(clone::Nope)),
    ));

    for _ in 0..3 {
        assert_eq!(Ok(&clone::Nope), u.ok_no_clone(true));
        assert_eq!(Err(clone::Sure), u.ok_no_clone(false));
    }

    assert_eq!(Err(clone::Nope), u.err_no_clone());
}

#[test]
#[should_panic(
    expected = "InResult::ok_no_clone: Expected InResult::ok_no_clone(_) at tests/it/mixed.rs:121 to match exactly 1 call, but it actually matched 2 calls."
)]
fn in_result_may_multi_respond_on_ok_no_clone() {
    let u = Unimock::new(
        InResultMock::ok_no_clone
            .some_call(matching!(_))
            .returns(Ok::<_, clone::Sure>(clone::Nope)),
    );

    assert_eq!(Ok(&clone::Nope), u.ok_no_clone(true));
    assert_eq!(Ok(&clone::Nope), u.ok_no_clone(true));
}

#[unimock(api = InVecMock)]
trait InVec {
    fn vector(&self) -> Vec<&i32>;
}

#[test]
fn in_vec() {
    let u = Unimock::new(
        InVecMock::vector
            .each_call(matching!())
            .returns(vec![1, 2, 3]),
    );

    assert_eq!(vec![&1, &2, &3], u.vector());
}

#[unimock(api = MixedTupleMock)]
trait MixedTuple {
    fn tuple1(&self) -> (&i32,);
    fn tuple2a(&self) -> (&i32, i32);
    fn tuple2b(&self) -> (i32, &i32);
    fn tuple4(&self) -> (&clone::Nope, clone::Nope, &clone::Sure, clone::Sure);
}

#[test]
fn mixed_tuple1() {
    let u = Unimock::new((
        MixedTupleMock::tuple2a
            .next_call(matching!())
            .returns((1, 2)),
        MixedTupleMock::tuple2b
            .next_call(matching!())
            .returns((1, 2)),
    ));

    assert_eq!((&1, 2), u.tuple2a());
    assert_eq!((1, &2), u.tuple2b());
}

#[test]
fn mixed_tuple_clone_combinatorics_once() {
    let u = Unimock::new(MixedTupleMock::tuple4.next_call(matching!()).returns((
        clone::Nope,
        clone::Nope,
        clone::Sure,
        clone::Sure,
    )));

    assert_eq!(
        (&clone::Nope, clone::Nope, &clone::Sure, clone::Sure),
        u.tuple4()
    );
}

#[test]
fn mixed_tuple_clone_combinatorics_many() {
    let u = Unimock::new(
        MixedTupleMock::tuple4
            .each_call(matching!())
            .applies(&|| respond((clone::Nope, clone::Nope, clone::Sure, clone::Sure))),
    );

    for _ in 0..3 {
        assert_eq!(
            (&clone::Nope, clone::Nope, &clone::Sure, clone::Sure),
            u.tuple4()
        );
    }
}

#[unimock(api=InPollMock)]
trait InPoll {
    fn poll_result(&self) -> Poll<Result<&i32, ()>>;
    fn poll_option(&self) -> Poll<Option<&i32>>;
}

#[test]
fn in_poll() {
    let u = Unimock::new((
        InPollMock::poll_result
            .next_call(matching!())
            .returns(Poll::Ready(Ok::<_, ()>(42))),
        InPollMock::poll_option
            .next_call(matching!())
            .returns(Poll::<Option<&i32>>::Pending),
    ));

    assert_eq!(Poll::Ready(Ok(&42)), <Unimock as InPoll>::poll_result(&u));
    assert_eq!(Poll::Pending, <Unimock as InPoll>::poll_option(&u));
}

mod shallow {
    use unimock::*;

    trait Lol {
        fn f(&self) -> Result<&u32, u32>;
    }

    struct Mock;

    type MyResult<T> = Result<T, u32>;

    impl MockFn for Mock {
        type Inputs<'i> = ();
        type OutputKind = unimock::output::Shallow<MyResult<&'static u32>>;
        type ApplyFn = ();

        fn info() -> MockFnInfo {
            MockFnInfo::new::<Self>()
        }
    }

    impl Lol for Unimock {
        fn f(&self) -> Result<&u32, u32> {
            unimock::private::eval::<Mock>(self, ()).unwrap(self)
        }
    }

    #[test]
    fn test() {
        let u = Unimock::new(Mock.each_call(matching!()).returns(Ok::<_, u32>(42)));

        assert_eq!(<Unimock as Lol>::f(&u), Ok(&42));
    }
}
