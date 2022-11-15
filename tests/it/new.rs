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
    type Output<'s> = output::Owned<String>;
}

impl MockFn2 for MockBorrowed {
    type Inputs<'i> = ();
    type Output<'s> = output::Ref<'s, str>;
}

impl MockFn2 for MockStatic {
    type Inputs<'i> = ();
    type Output<'s> = output::StaticRef<str>;
}

impl MockFn2 for MockComplex {
    type Inputs<'i> = ();
    type Output<'s> = output::Complex<Option<&'s str>>;
}

#[test]
fn test_owned() {
    MockOwned.some_call().returns("");
    MockBorrowed.some_call().returns("");
    MockStatic.some_call().returns("");
    MockComplex.some_call().returns(Some(String::new()));
    MockComplex.some_call().returns(None);
}
