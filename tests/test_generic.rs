use unimock::*;

use std::fmt::Debug;

#[unimock]
trait GenericOutput<T> {
    fn generic_output(&self) -> T;
}

#[test]
fn test_generic_return() {
    let deps = mock([
        GenericOutput__generic_output
            .with_types::<String>()
            .each_call(matching!())
            .returns("success".to_string())
            .in_any_order(),
        GenericOutput__generic_output
            .with_types::<i32>()
            .each_call(matching!())
            .returns(42)
            .in_any_order(),
    ]);

    let output = <Unimock as GenericOutput<String>>::generic_output(&deps);
    assert_eq!("success", output);

    let output = <Unimock as GenericOutput<i32>>::generic_output(&deps);
    assert_eq!(42, output);
}

#[unimock]
trait GenericParam<T> {
    fn generic_param(&self, param: T) -> &'static str;
}

#[test]
fn test_generic_param() {
    let deps = mock([
        GenericParam__generic_param
            .with_types::<&'static str>()
            .each_call(matching!("foobar"))
            .returns_static("str")
            .in_any_order(),
        GenericParam__generic_param
            .with_types::<i32>()
            .each_call(matching!(42))
            .returns_static("i32")
            .in_any_order(),
    ]);

    let output = <Unimock as GenericParam<&'static str>>::generic_param(&deps, "foobar");
    assert_eq!("str", output);

    let output = <Unimock as GenericParam<i32>>::generic_param(&deps, 42);
    assert_eq!("i32", output);
}

#[unimock]
trait GenericBounds<I: Debug, O: Clone> {
    fn generic_bounds(&self, param: I) -> O;
}

#[unimock]
trait GenericWhereBounds<I, O>
where
    I: Debug,
    O: Clone,
{
    fn generic_where_bounds(&self, param: I) -> O;
}
