use unimock as mocknroll;

#[mocknroll::unimock(api=TestMock, prefix=mocknroll)]
trait Test {
    fn test(&self);
}

#[test]
fn test() {
    use mocknroll::MockFn;

    let roll = mocknroll::Unimock::new(TestMock::test.stub(|each| {
        each.call(mocknroll::matching!()).returns(());
    }));

    roll.test();
}
