use unimock::*;

trait Mockable {
    fn owned(&self) -> String;
    fn borrowed(&self) -> &str;
    fn statik(&self) -> &'static str;
    fn complex(&self) -> Option<&str>;
}

struct MockOwned;
struct MockBorrowed;
struct MockStatic;
struct MockComplex;

impl MockFn2 for MockOwned {
    type Inputs<'i> = ();
    type Output = output::Owned<String>;
    type OutputSig<'u> = output::Owned<String>;
    type OutputOld<'u> = output::Owned<String>;
}

impl MockFn2 for MockBorrowed {
    type Inputs<'i> = ();
    type Output = output::Ref<str>;
    type OutputSig<'u> = output::RefSig<'u, str>;
    type OutputOld<'u> = output::RefOld<'u, str>;
}

impl MockFn2 for MockStatic {
    type Inputs<'i> = ();
    type Output = output::StaticRef<str>;
    type OutputSig<'u> = output::StaticRef<str>;
    type OutputOld<'u> = output::StaticRef<str>;
}

impl MockFn2 for MockComplex {
    type Inputs<'i> = ();
    type Output = output::Complex<Option<&'static str>>;
    type OutputSig<'u> = output::ComplexSig<Option<&'u str>>;
    type OutputOld<'u> = output::ComplexOld<'u, Option<&'u str>>;
}

#[test]
fn test_owned() {
    MockOwned.some_call().returns("");
    MockBorrowed.some_call().returns("");
    MockStatic.some_call().returns("");
    MockComplex.some_call().returns(Some(String::new()));
    MockComplex.some_call().returns(None);
}
