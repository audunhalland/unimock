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
                    .answers(&|_, _| 777)
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
                    .answers(&|_, _| 666)
            )
            .default_body(21)
        );
    }
}

mod default_impl_mut {
    use unimock::*;

    #[unimock(api = MutDefaultBodyMock)]
    trait MutDefaultBody {
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
                MutDefaultBodyMock::default_body
                    .next_call(matching!(21))
                    .answers(&|_, _arg| 777)
            )
            .default_body(21)
        );
    }

    #[test]
    fn delegate_through_default_body() {
        assert_eq!(
            666,
            Unimock::new(
                MutDefaultBodyMock::core
                    .next_call(matching!(42))
                    .answers(&|_, _| 666)
            )
            .default_body(21)
        );
    }

    // Requires polonius_the_crab
    #[unimock(api = MutDefaultBodyBorrowMock)]
    trait MutDefaultBodyBorrow {
        fn borrow_ret(&mut self, arg: i32) -> &i32;

        fn default_borrow_ret(&mut self, arg: i32) -> &i32 {
            self.borrow_ret(arg * 2)
        }
    }

    #[test]
    fn mut_return_borrow() {
        assert_eq!(
            &666,
            Unimock::new(
                MutDefaultBodyBorrowMock::borrow_ret
                    .next_call(matching!(42))
                    .answers(&|_, _| &666)
            )
            .default_borrow_ret(21)
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
                    .answers(&|_, _| 777)
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
                    .answers(&|_, _| 777)
            ))
            .default_body(21)
        );
    }
}

#[rustversion::since(1.75)]
mod default_impl_async {
    use unimock::*;

    type TestResult<T> = Result<T, ()>;

    // A point of these tests is that the method names
    // in both traits are the same.
    // This tests that the delegation is done with fully-qualified syntax.

    #[unimock(api = AsyncTraitDefault1Mock)]
    trait AsyncTraitDefault1 {
        async fn normal_method(&self) -> TestResult<i32>;

        async fn default_method(&self) -> TestResult<i32> {
            Ok(42)
        }
    }

    #[unimock(api = AsyncTraitDefault2Mock)]
    trait AsyncTraitDefault2 {
        async fn normal_method(&self) -> TestResult<i32>;

        async fn default_method(&self) -> TestResult<i32> {
            Ok(42)
        }
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

mod default_impl_pin {
    use core::pin::Pin;

    use unimock::*;

    #[unimock(api = PinDefault1Mock)]
    trait PinDefault1 {
        fn pinned(self: Pin<&mut Self>, arg: i32) -> &i32;

        fn pinned_default(self: Pin<&mut Self>) -> &i32 {
            self.pinned(123)
        }
    }

    #[unimock(api = PinDefault2Mock)]
    trait PinDefault2 {
        fn pinned(self: Pin<&mut Self>, arg: i32) -> &i32;

        fn pinned_default(self: Pin<&mut Self>) -> &i32 {
            self.pinned(123)
        }
    }

    #[test]
    fn mock_pin_default() {
        let mut unimock = Unimock::new(
            PinDefault1Mock::pinned
                .next_call(matching!(123))
                .answers(&|_, _| &666),
        );
        assert_eq!(
            &666,
            <Unimock as PinDefault1>::pinned_default(Pin::new(&mut unimock))
        );
    }
}
