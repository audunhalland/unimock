mod default_impl_owned {
    use unimock::*;

    #[unimock(api = DefaultBodyMock)]
    trait DefaultBody: Sized {
        fn core(self, arg: i32) -> i32;

        fn default_body(self, arg: i32) -> i32 {
            self.core(arg * 2)
        }
    }
}

mod default_impl_borrowed {
    use unimock::*;

    #[unimock(api = DefaultBodyMock)]
    trait DefaultBody {
        fn core(&self, arg: i32) -> i32;

        fn default_body(&self, arg: i32) -> i32 {
            self.core(arg * 2)
        }
    }

    #[test]
    fn mock_default_body() {
        assert_eq!(
            777,
            Unimock::new(
                DefaultBodyMock::default_body
                    .next_call(matching!(21))
                    .answers(|_| 777)
            )
            .default_body(21)
        );
    }

    #[test]
    fn delegate_through_default_body() {
        assert_eq!(
            666,
            Unimock::new(
                DefaultBodyMock::core
                    .next_call(matching!(42))
                    .answers(|_| 666)
            )
            .default_body(21)
        );
    }
}

mod default_impl_mut {
    use unimock::*;

    #[unimock(api = DefaultBodyMock)]
    trait DefaultBody {
        fn core(&mut self, arg: i32) -> i32;

        fn default_body(&mut self, arg: i32) -> i32 {
            self.core(arg * 2)
        }
    }

    #[test]
    fn mock_default_body() {
        assert_eq!(
            777,
            Unimock::new(
                DefaultBodyMock::default_body
                    .next_call(matching!(21))
                    .answers(|_| 777)
            )
            .default_body(21)
        );
    }

    #[test]
    fn delegate_through_default_body() {
        assert_eq!(
            666,
            Unimock::new(
                DefaultBodyMock::core
                    .next_call(matching!(42))
                    .answers(|_| 666)
            )
            .default_body(21)
        );
    }
}

#[cfg(feature = "std")]
mod default_impl_rc {
    use std::rc::Rc;
    use unimock::*;

    #[unimock(api = DefaultBodyMock)]
    trait DefaultBody: Sized {
        fn core(self: Rc<Self>, arg: i32) -> i32;

        fn default_body(self: Rc<Self>, arg: i32) -> i32 {
            self.core(arg * 2)
        }
    }

    #[test]
    fn mock_default_body() {
        assert_eq!(
            777,
            Rc::new(Unimock::new(
                DefaultBodyMock::default_body
                    .next_call(matching!(21))
                    .answers(|_| 777)
            ))
            .default_body(21)
        );
    }
}

#[cfg(feature = "std")]
mod default_impl_arc {
    use std::sync::Arc;
    use unimock::*;

    #[unimock(api = DefaultBodyMock)]
    trait DefaultBody: Sized {
        fn core(self: Arc<Self>, arg: i32) -> i32;

        fn default_body(self: Arc<Self>, arg: i32) -> i32 {
            self.core(arg * 2)
        }
    }

    #[test]
    fn mock_default_body() {
        assert_eq!(
            777,
            Arc::new(Unimock::new(
                DefaultBodyMock::default_body
                    .next_call(matching!(21))
                    .answers(|_| 777)
            ))
            .default_body(21)
        );
    }
}

#[cfg(feature = "std")]
mod default_impl_async_trait {
    use unimock::*;

    type TestResult<T> = Result<T, ()>;

    // A point of these tests is that the method names
    // in both traits are the same.
    // This tests that the delegation is done with fully-qualified syntax.

    #[unimock(api = AsyncTraitDefault1Mock)]
    #[async_trait::async_trait]
    trait AsyncTraitDefault1 {
        async fn normal_method(&self) -> TestResult<i32>;

        async fn default_method(&self) -> TestResult<i32> {
            Ok(42)
        }
    }

    #[unimock(api = AsyncTraitDefault2Mock)]
    #[async_trait::async_trait]
    trait AsyncTraitDefault2 {
        async fn normal_method(&self) -> TestResult<i32>;

        async fn default_method(&self) -> TestResult<i32> {
            Ok(42)
        }
    }
}
