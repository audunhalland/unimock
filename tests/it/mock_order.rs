use unimock::*;

#[unimock(api=T1Mock)]
trait T1 {
    fn a(&self, i: i32) -> i32;
    fn b(&self, i: i32) -> i32;
}

#[unimock(api=T2Mock)]
trait T2 {
    fn c(&self, i: i32) -> i32;
}

#[test]
#[should_panic(
    expected = "T2::c(2): Method matched in wrong order. Expected a call matching T1::a(2) at tests/it/mock_order.rs:25."
)]
fn two_fns_in_incorrect_order_should_fail_and_presence_of_a_stub_should_not_influence_order() {
    let m = Unimock::new((
        T1Mock::a.next_call(matching!(0)).returns(0),
        T1Mock::b.stub(|each| {
            each.call(matching!(_)).returns(1337);
        }),
        T2Mock::c.next_call(matching!(1)).returns(1),
        T1Mock::a.next_call(matching!(2)).returns(2),
        T2Mock::c.next_call(matching!(3)).returns(3).n_times(2),
    ));

    assert_eq!(1337, m.b(33));
    assert_eq!(0, m.a(0));
    assert_eq!(1337, m.b(33));
    assert_eq!(1, m.c(1));
    m.c(2);
}

#[test]
#[should_panic(
    expected = "T1::a(2): Ordered call (3) out of range: There were no more ordered call patterns in line for selection."
)]
fn should_give_sensible_panic_message_when_calling_beyond_mocked_order() {
    let m = Unimock::new((
        T1Mock::a.next_call(matching!(0)).returns(0),
        T1Mock::b.next_call(matching!(1)).returns(1),
    ));

    assert_eq!(0, m.a(0));
    assert_eq!(1, m.b(1));
    m.a(2);
}

#[test]
#[should_panic(
    expected = "T1::a(0): Method invoked in the correct order (3), but inputs didn't match T1::a(1) at tests/it/mock_order.rs:58."
)]
fn calling_expired_pattern_should_fail() {
    let m = Unimock::new((
        T1Mock::a.next_call(matching!(0)).returns(0).n_times(2),
        T1Mock::a.next_call(matching!(1)).returns(1),
    ));

    assert_eq!(0, m.a(0));
    assert_eq!(0, m.a(0));
    m.a(0);
}

#[test]
#[should_panic(
    expected = "T1::a(0): Method invoked in the correct order (1), but inputs didn't match T1::a(1 | 2) at tests/it/mock_order.rs:71."
)]
fn call_order_error_with_complex_pattern() {
    let m = Unimock::new(T1Mock::a.next_call(matching!((1 | 2))).returns(0));
    m.a(0);
}

#[test]
#[should_panic(
    expected = "T1::a(0): Method invoked in the correct order (1), but inputs didn't match T1::a(x) if {guard} at tests/it/mock_order.rs:80."
)]
fn call_order_error_with_guard_pattern() {
    let m = Unimock::new(T1Mock::a.next_call(matching!(x if x * 2 == 7)).returns(0));
    m.a(0);
}
