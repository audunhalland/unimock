use unimock::lib::String;
use unimock::*;

#[unimock(api=SingleArgMock)]
trait SingleArg {
    fn method1<'s>(&'s self, a: &'s str) -> &'s str;
}

#[test]
#[should_panic(expected = "SingleArg::method1(\"hoi\"): No mock implementation found.")]
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
    expected = "A clause for SingleArg::method1 has already been registered as InAnyOrder, but got re-registered as InOrder. They cannot be mixed for the same MockFn."
)]
fn should_complain_about_mismatched_modes() {
    Unimock::new((
        SingleArgMock::method1.each_call(matching!(_)).returns("a"),
        SingleArgMock::method1
            .next_call(matching!(_))
            .returns("b")
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
    expected = "SingleArg::method1(\"whatever\"): No output available for after matching SingleArg::method1(_) at tests/it/errors.rs:51."
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
    expected = "SingleArg::method1: Expected SingleArg::method1(\"a\") at tests/it/errors.rs:71 to match exactly 1 call, but it actually matched no calls."
)]
fn call_pattern_with_count_expectation_should_panic_if_not_met() {
    Unimock::new(SingleArgMock::method1.stub(|each| {
        each.call(matching!("a")).returns(String::new()).once();
        each.call(matching!(_)).returns(String::new());
    }))
    .method1("b");
}

#[test]
#[should_panic(
    expected = "SingleArg::method1(\"b\"): Explicit panic from SingleArg::method1(_) at tests/it/errors.rs:83: foobar!"
)]
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

#[cfg(feature = "std")]
#[test]
#[should_panic(expected = "SingleArg::method1(\"\"): No mock implementation found.")]
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
    expected = "Foo::foo(2): Cannot return value more than once from Foo::foo(_) at tests/it/errors.rs:127, because of missing Clone bound. Try using `.each_call()` or explicitly quantifying the response."
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

#[test]
#[should_panic(
    expected = "Foo::foo: Expected Foo::foo(2) at tests/it/errors.rs:145 to match exactly 1 call, but it actually matched no calls."
)]
fn should_require_both_calls_2_some_call() {
    #[unimock(api=FooMock)]
    trait Foo {
        fn foo(&self, arg: i32) -> i32;
    }

    let unimock = Unimock::new((
        FooMock::foo.some_call(matching!(1)).returns(42),
        FooMock::foo.some_call(matching!(2)).returns(1337),
    ));

    assert_eq!(42, unimock.foo(1));
}

#[cfg(feature = "std")]
#[test]
#[should_panic(
    expected = "Original Unimock instance destroyed on a different thread than the one it was created on. To solve this, clone the object before sending it to the other thread."
)]
fn should_crash_when_sending_original_unimock_to_another_thread() {
    let u = Unimock::new(());
    let drop_u = move || drop(u);
    let err = std::thread::spawn(drop_u).join().expect_err("Must error");
    std::panic::resume_unwind(err);
}

#[test]
#[should_panic(
    expected = "SingleArg::method1(\"\"): No function supplied for matching inputs for call pattern SingleArg::method1[#0]."
)]
fn no_matcher_function() {
    let u = Unimock::new(SingleArgMock::method1.next_call(&|_| ()).returns(""));
    u.method1("");
}

#[test]
#[should_panic = "SingleArg::method1 has not been set up with default implementation delegation."]
fn no_default_impl() {
    let u = Unimock::new(
        SingleArgMock::method1
            .next_call(matching!())
            .default_implementation(),
    );
    u.method1("");
}
