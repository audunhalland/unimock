use unimock::*;

#[unimock(api=SingleArgMock)]
trait SingleArg {
    fn method1<'s>(&'s self, a: &'s str) -> &'s str;
}

#[test]
#[should_panic(expected = "No mock implementation found for SingleArg::method1")]
fn should_panic_for_nonexisting_mock() {
    Unimock::new(()).method1("hoi");
}

#[test]
#[should_panic(
    expected = "Mock for SingleArg::method1 was never called. Dead mocks should be removed."
)]
fn should_panic_for_unused_stub() {
    Unimock::new(SingleArgMock::method1.stub(|each| {
        each.call(matching!(_));
    }));
}

#[test]
#[should_panic(
    expected = "A clause for SingleArg::method1 has already been registered as InAnyOrder, but got re-registered as InOrder. They cannot be mixed for the same MockFna."
)]
fn should_complain_about_mismatched_modes() {
    Unimock::new((
        SingleArgMock::method1
            .each_call(matching!(_))
            .returns_ref("a"),
        SingleArgMock::method1
            .next_call(matching!(_))
            .returns_ref("b")
            .once(),
    ));
}

#[test]
#[should_panic(expected = "Stub contained no call patterns")]
fn should_panic_for_empty_stub_closure() {
    let _ = Unimock::new(SingleArgMock::method1.stub(|_| {}));
}

#[test]
#[should_panic(
    expected = "SingleArg::method1(\"whatever\"): No output available for matching call pattern #0"
)]
fn call_pattern_without_output_factory_should_crash() {
    Unimock::new(SingleArgMock::method1.stub(|each| {
        each.call(matching!(_));
    }))
    .method1("whatever");
}

#[test]
#[should_panic(expected = "SingleArg::method1(\"anything\"): No matching call patterns.")]
fn should_panic_if_no_call_patterns_in_stub_are_matched() {
    Unimock::new(SingleArgMock::method1.stub(|each| {
        each.call(matching!("something"));
    }))
    .method1("anything");
}

#[test]
#[should_panic(
    expected = "SingleArg::method1: Expected call pattern #0 to match exactly 1 call, but it actually matched no calls."
)]
fn call_pattern_with_count_expectation_should_panic_if_not_met() {
    Unimock::new(SingleArgMock::method1.stub(|each| {
        each.call(matching!("a")).returns_ref(String::new()).once();
        each.call(matching!(_)).returns_ref(String::new());
    }))
    .method1("b");
}

#[test]
#[should_panic(expected = "SingleArg::method1(\"b\"): Explicit panic for call pattern #0: foobar!")]
fn should_panic_with_explicit_message() {
    Unimock::new(SingleArgMock::method1.stub(|each| {
        each.call(matching!(_)).panics("foobar!");
    }))
    .method1("b");
}

#[test]
#[should_panic(
    expected = "Unimock cannot verify calls, because the original instance got dropped while there are clones still alive."
)]
fn should_crash_when_the_original_instance_disappears_before_the_clone() {
    let _ = {
        let original = Unimock::new(());
        let clone = original.clone();
        drop(original);
        clone
    };
}

#[test]
#[should_panic(expected = "No mock implementation found for SingleArg::method1")]
fn multithread_error_reporting_works() {
    let unimock = Unimock::new(());

    #[allow(clippy::redundant_clone)]
    let clone = unimock.clone();

    std::thread::spawn(move || {
        clone.method1("");
    })
    .join()
    .expect_err("");
}

#[test]
#[should_panic(
    expected = "Foo::foo(2): Cannot return value more than once for call pattern #0, because of missing Clone bound. Try using `.each_call()` or explicitly quantifying the response."
)]
fn should_complain_when_returning_unquantified_value_more_then_once() {
    #[unimock(api=FooMock)]
    trait Foo {
        fn foo(&self, arg: i32) -> i32;
    }

    let unimock = Unimock::new(FooMock::foo.some_call(matching!(_)).returns(42));

    assert_eq!(42, unimock.foo(1));
    unimock.foo(2);
}
