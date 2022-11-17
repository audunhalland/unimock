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

impl MockFn2 for MockOwned {
    type Inputs<'i> = ();
    type Output = output::Owned<String>;
    type OutputSig<'u> = output::Owned<String>;
    const NAME: &'static str = "";
}

impl MockFn2 for MockBorrowed {
    type Inputs<'i> = ();
    type Output = output::Borrowed<str>;
    type OutputSig<'u> = output::Borrowed<str>;
    const NAME: &'static str = "";
}

impl MockFn2 for MockBorrowedParam {
    type Inputs<'i> = &'i str;
    // There is now way to store an "owned" version of something borrowed from inputs
    type Output = output::StaticRef<str>;
    type OutputSig<'u> = output::StaticRef<str>;
    const NAME: &'static str = "";
}

impl MockFn2 for MockStatic {
    type Inputs<'i> = ();
    type Output = output::StaticRef<str>;
    type OutputSig<'u> = output::StaticRef<str>;
    const NAME: &'static str = "";
}

impl MockFn2 for MockMixed {
    type Inputs<'i> = ();
    type Output = output::Mixed<Option<&'static str>>;
    type OutputSig<'u> = output::Mixed<Option<&'u str>>;
    const NAME: &'static str = "";
}

#[test]
fn test_owned() {
    MockOwned.some_call2().returns("foo");
    MockOwned.some_call2().returns("too".to_string());
    MockBorrowed.some_call2().returns_borrow("foo");
    MockBorrowed.some_call2().returns_borrow("foo".to_string());
    MockBorrowedParam.some_call2().returns("foo");
    MockStatic.some_call2().returns("foo");
    MockMixed.some_call2().returns(Some("foo".to_string()));
    MockMixed.some_call2().returns(None);
}

fn test_borrow_self_compiles<'u>(unimock: &Unimock) -> &str {
    unimock::macro_api::eval2::<MockBorrowed>(unimock, ()).unwrap(unimock)
}

fn test_borrow_param_compiles<'i>(unimock: &Unimock, input: &'i str) -> &'i str {
    unimock::macro_api::eval2::<MockBorrowedParam>(unimock, input).unwrap(unimock)
}
