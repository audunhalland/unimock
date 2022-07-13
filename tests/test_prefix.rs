use unimock as mocknroll;

#[mocknroll::unimock(prefix = mocknroll)]
trait Test {
    fn test(&self);
}

#[test]
fn test() {
    use mocknroll::MockFn;

    let roll = mocknroll::mock(Some(Test__test.stub(|each| {
        each.call(mocknroll::matching!()).returns(());
    })));

    roll.test();
}
