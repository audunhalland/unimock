use unimock::*;

#[unimock]
trait T1 {
    fn a(&self, i: i32) -> i32;
    fn b(&self, i: i32) -> i32;
}

#[unimock]
trait T2 {
    fn c(&self, i: i32) -> i32;
}

#[test]
#[should_panic(
    expected = "T2::c(0): Matched in wrong order. It supported the call order ranges [2, 4..6], but actual call order was 3."
)]
fn two_fns_in_incorrect_order_should_fail_and_presence_of_a_stub_should_not_influence_order() {
    let m = mock([
        T1__a.next_call(matching!(0)).returns(0).once().in_order(),
        T1__b.stub(|each| {
            each.call(matching!(_)).returns(0);
        }),
        T2__c.next_call(matching!(0)).returns(0).once().in_order(),
        T1__a.next_call(matching!(0)).returns(0).once().in_order(),
        T2__c
            .next_call(matching!(0))
            .returns(0)
            .n_times(2)
            .in_order(),
    ]);

    assert_eq!(0, m.b(33));
    assert_eq!(0, m.a(0));
    assert_eq!(0, m.b(33));
    assert_eq!(0, m.c(0));
    m.c(0);
}

#[test]
#[should_panic(
    expected = "T1::a(0): Invoked in the correct order (3), but inputs didn't match call pattern #1."
)]
fn calling_expired_pattern_should_fail() {
    let m = mock([
        T1__a
            .next_call(matching!(0))
            .returns(0)
            .n_times(2)
            .in_order(),
        T1__a.next_call(matching!(1)).returns(1).once().in_order(),
    ]);

    assert_eq!(0, m.a(0));
    assert_eq!(0, m.a(0));
    m.a(0);
}
