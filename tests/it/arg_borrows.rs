use unimock::*;

#[unimock(api=TraitMock)]
trait Trait {
    fn arg_borrow<'a>(&self, s: &'a str) -> &'a str;
}

#[test]
fn arg_borrows() {
    let u = Unimock::new(
        TraitMock::arg_borrow
            .next_call(matching!())
            .answers(&|_, s| &s[1..]),
    );

    assert_eq!("bc", u.arg_borrow("abc"));
}
