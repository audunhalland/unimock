use unimock::*;

#[unimock]
trait GenericReturn<T> {
    fn generic_return(&self) -> T;
}

#[test]
fn test() {
    let _ = GenericReturn__generic_return
        .with_types::<String>()
        .each_call(matching!())
        .returns("".to_string())
        .in_any_order();
}
