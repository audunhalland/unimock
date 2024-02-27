#[rustversion::since(1.75)]
mod r#async {
    use unimock::*;

    use crate::AsyncTest;

    #[unimock(api = TraitMock)]
    trait Trait {
        async fn a(&self, arg: i32) -> i32;
        async fn b(&self) -> &i32;
        async fn c(&self) -> Option<&i32>;
    }

    #[test]
    fn test_it() {
        async {
            let deps = Unimock::new((
                TraitMock::a.next_call(matching!(_)).returns(42),
                TraitMock::b.next_call(matching!()).returns(42),
                TraitMock::c.next_call(matching!()).returns(Some(42)),
            ));

            assert_eq!(42, deps.a(5).await);
            assert_eq!(&42, deps.b().await);
            assert_eq!(Some(&42), deps.c().await);
        }
        .test()
    }
}

#[rustversion::since(1.75)]
mod rpit_future {
    use std::future::Future;

    use unimock::*;

    use crate::AsyncTest;

    #[unimock(api = RpitFutureMock)]
    trait RpitFuture {
        fn m1(&self) -> impl Future<Output = i32>;
        fn m2(&self, arg: i32) -> impl Send + Future<Output = i32>;
        fn m3(&self) -> impl Future<Output = i32> + Send;
        fn m4(&self) -> impl core::future::Future<Output = i32> + Send;
    }

    #[test]
    fn rpit() {
        async {
            let u = Unimock::new((
                RpitFutureMock::m1.next_call(matching!()).returns(1337),
                RpitFutureMock::m2.next_call(matching!(42)).returns(1338),
            ));

            assert_eq!(u.m1().await, 1337);
            assert_eq!(u.m2(42).await, 1338);
        }
        .test()
    }
}
