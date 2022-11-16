use unimock::*;

trait Mockable {
    fn owned(&self) -> String;
    fn borrowed(&self) -> &str;
    fn borrowed_param<'i>(&self, i: &'i str) -> &'i str;
    fn statik(&self) -> &'static str;
    fn complex(&self) -> Option<&str>;
}

struct MockOwned;
struct MockBorrowed;
struct MockBorrowedParam;
struct MockStatic;
struct MockComplex;

impl MockFn2 for MockOwned {
    type Inputs<'i> = ();
    type Output = output::Owned<String>;
    type OutputSig<'u, 'i> = output::Owned<String>;
    const NAME: &'static str = "";
}

impl MockFn2 for MockBorrowed {
    type Inputs<'i> = ();
    type Output = output::Ref<str>;
    type OutputSig<'u, 'i> = output::RefSig<'u, str>;
    const NAME: &'static str = "";
}

impl MockFn2 for MockBorrowedParam {
    type Inputs<'i> = ();
    type Output = output::Ref<str>;
    type OutputSig<'u, 'i> = output::RefSig<'u, str>;
    const NAME: &'static str = "";
}

impl MockFn2 for MockStatic {
    type Inputs<'i> = ();
    type Output = output::StaticRef<str>;
    type OutputSig<'u, 'i> = output::StaticRef<str>;
    const NAME: &'static str = "";
}

impl MockFn2 for MockComplex {
    type Inputs<'i> = ();
    type Output = output::Complex<Option<&'static str>>;
    type OutputSig<'u, 'i> = output::ComplexSig<Option<&'u str>>;
    const NAME: &'static str = "";
}

#[test]
fn test_owned() {
    MockOwned.some_call().returns("foo");
    MockOwned.some_call().returns("too".to_string());
    MockBorrowed.some_call().returns_ref("foo");
    MockBorrowed.some_call().returns_ref("foo".to_string());
    MockStatic.some_call().returns("foo");
    MockComplex.some_call().returns(Some("foo".to_string()));
    MockComplex.some_call().returns(None);
}
