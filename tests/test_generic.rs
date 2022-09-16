use unimock::*;

use std::fmt::Debug;

mod output {
    use super::*;

    #[unimock]
    trait GenericOutput<T> {
        fn generic_output(&self) -> T;
    }

    #[test]
    fn test_generic_return() {
        let deps = mock([
            GenericOutputMock::generic_output
                .with_types::<String>()
                .each_call(matching!())
                .returns("success".to_string())
                .in_any_order(),
            GenericOutputMock::generic_output
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
}

mod param {
    use super::*;

    #[unimock]
    trait GenericParam<T> {
        fn generic_param(&self, param: T) -> &'static str;
    }

    #[test]
    fn test_generic_param() {
        let deps = mock([
            GenericParamMock::generic_param
                .with_types::<&'static str>()
                .each_call(matching!("foobar"))
                .returns_static("a string")
                .in_any_order(),
            GenericParamMock::generic_param
                .with_types::<i32>()
                .each_call(matching!(42))
                .returns_static("a number")
                .in_any_order(),
        ]);

        assert_eq!("a string", deps.generic_param("foobar"));
        assert_eq!("a number", deps.generic_param(42_i32));
    }

    #[test]
    #[should_panic(
        // Since the generic parameter has no Debug bound, we cannot see the parameter:
        expected = "GenericParam::generic_param(?): No matching call patterns."
    )]
    fn test_generic_param_panic_no_debug() {
        let deps = mock(Some(
            GenericParamMock::generic_param
                .with_types::<i32>()
                .each_call(matching!(1337))
                .returns_static("a number")
                .in_any_order(),
        ));

        deps.generic_param(42_i32);
    }

    #[unimock]
    trait GenericParamDebug<T: Debug> {
        fn generic_param_debug(&self, param: T) -> &'static str;
    }

    #[test]
    #[should_panic(
        // When it has a debug bound, we should see it:
        expected = "GenericParamDebug::generic_param_debug(42): No matching call patterns."
    )]
    fn test_generic_param_panic_debug() {
        let deps = mock(Some(
            GenericParamDebugMock::generic_param_debug
                .with_types::<i32>()
                .each_call(matching!(1337))
                .returns_static("a number")
                .in_any_order(),
        ));

        deps.generic_param_debug(42_i32);
    }
}

mod combined {
    use super::*;

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
}

mod async_generic {
    use super::*;

    #[unimock]
    #[async_trait::async_trait]
    trait AsyncTraitGenericBounds<I: Debug, O: Clone> {
        async fn generic_bounds(&self, param: I) -> O;
    }
}

mod generic_without_module {
    use super::*;

    #[unimock(flatten=[Func])]
    trait WithModule<T: Debug> {
        fn func(&self) -> T;
    }

    #[test]
    fn mock() {
        Func.with_types::<String>()
            .each_call(matching!())
            .returns("".to_string());
    }
}

mod generic_with_unmock {
    use super::*;

    #[unimock(unmock_with=[gen_default(self)])]
    trait UnmockMe<T: Default> {
        fn unmock_me(&self) -> T;
    }

    #[unimock(unmock_with=[gen_default(self)])]
    trait UnmockMeWhere<T>
    where
        T: Default,
    {
        fn unmock_me_where(&self) -> T;
    }

    fn gen_default<D, T: Default>(_: &D) -> T {
        T::default()
    }
}
