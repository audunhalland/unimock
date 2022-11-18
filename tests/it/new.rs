use unimock::*;

trait Mockable {
    fn owned(&self) -> String;
    fn borrowed(&self) -> &str;
    fn borrowed_param<'i>(&self, i: &'i str) -> &'i str;
    fn statik(&self) -> &'static str;
    fn mixed(&self) -> Option<&str>;
}

struct MockOwned;
struct MockBorrowed;
struct MockBorrowedParam;
struct MockStatic;
struct MockMixed;

impl MockFn for MockOwned {
    type Inputs<'i> = ();
    type Output = output::Owned<String>;
    type OutputSig<'u> = output::Owned<String>;
    type OutputOld = ();
    const NAME: &'static str = "";

    fn debug_inputs(_: &Self::Inputs<'_>) -> String {
        String::new()
    }
}

impl MockFn for MockBorrowed {
    type Inputs<'i> = ();
    type Output = output::Borrowed<str>;
    type OutputSig<'u> = output::Borrowed<str>;
    type OutputOld = ();
    const NAME: &'static str = "";

    fn debug_inputs(_: &Self::Inputs<'_>) -> String {
        String::new()
    }
}

impl MockFn for MockBorrowedParam {
    type Inputs<'i> = &'i str;
    // There is now way to store an "owned" version of something borrowed from inputs
    type Output = output::StaticRef<str>;
    type OutputSig<'u> = output::StaticRef<str>;
    type OutputOld = ();
    const NAME: &'static str = "";

    fn debug_inputs(_: &Self::Inputs<'_>) -> String {
        String::new()
    }
}

impl MockFn for MockStatic {
    type Inputs<'i> = ();
    type Output = output::StaticRef<str>;
    type OutputSig<'u> = output::StaticRef<str>;
    type OutputOld = ();
    const NAME: &'static str = "";

    fn debug_inputs(_: &Self::Inputs<'_>) -> String {
        String::new()
    }
}

impl MockFn for MockMixed {
    type Inputs<'i> = ();
    type Output = output::Mixed<Option<&'static str>>;
    type OutputSig<'u> = output::Mixed<Option<&'u str>>;
    type OutputOld = ();
    const NAME: &'static str = "";

    fn debug_inputs(_: &Self::Inputs<'_>) -> String {
        String::new()
    }
}

#[test]
fn test_owned() {
    MockOwned.some_call(&|_| ()).returns("foo");
    MockOwned.some_call(&|_| ()).returns("too".to_string());
    MockBorrowed.some_call(&|_| ()).returns("foo");
    MockBorrowed.some_call(&|_| ()).returns("foo".to_string());
    MockBorrowedParam.some_call(&|_| ()).returns("foo");
    MockStatic.some_call(&|_| ()).returns("foo");
    MockMixed
        .some_call(&|_| ())
        .returns(Some("foo".to_string()));
    MockMixed.some_call(&|_| ()).returns(None);
}

fn test_borrow_self_compiles<'u>(unimock: &Unimock) -> &str {
    unimock::macro_api::eval2::<MockBorrowed>(unimock, ()).unwrap(unimock)
}

fn test_borrow_param_compiles<'i>(unimock: &Unimock, input: &'i str) -> &'i str {
    unimock::macro_api::eval2::<MockBorrowedParam>(unimock, input).unwrap(unimock)
}
