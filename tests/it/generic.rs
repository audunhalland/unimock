use unimock::private::lib::{String, ToString};
use unimock::*;

use core::fmt::Debug;

mod output {
    use super::*;

    #[unimock(api=GenericOutputMock)]
    trait GenericOutput<T> {
        fn generic_output(&self) -> T;
    }

    #[test]
    fn test_generic_return() {
        let deps = Unimock::new((
            GenericOutputMock::generic_output
                .with_types::<String>()
                .each_call(matching!())
                .returns("success".to_string()),
            GenericOutputMock::generic_output
                .with_types::<i32>()
                .each_call(matching!())
                .returns(42),
        ));

        let output = <Unimock as GenericOutput<String>>::generic_output(&deps);
        assert_eq!("success", output);

        let output = <Unimock as GenericOutput<i32>>::generic_output(&deps);
        assert_eq!(42, output);
    }
}

mod param {
    use super::*;

    #[unimock(api=GenericParamMock)]
    trait GenericParam<T> {
        fn generic_param(&self, param: T) -> &'static str;
    }

    #[test]
    fn test_generic_param() {
        let deps = Unimock::new((
            GenericParamMock::generic_param
                .with_types::<&'static str>()
                .each_call(matching!("foobar"))
                .returns("a string"),
            GenericParamMock::generic_param
                .with_types::<i32>()
                .each_call(matching!(42))
                .returns("a number"),
        ));

        assert_eq!("a string", deps.generic_param("foobar"));
        assert_eq!("a number", deps.generic_param(42_i32));
    }

    #[test]
    #[should_panic(
        // Since the generic parameter has no Debug bound, we cannot see the parameter:
        expected = "GenericParam::generic_param(?): No matching call patterns."
    )]
    fn test_generic_param_panic_no_debug() {
        let deps = Unimock::new(
            GenericParamMock::generic_param
                .with_types::<i32>()
                .each_call(matching!(1337))
                .returns("a number"),
        );

        deps.generic_param(42_i32);
    }

    #[unimock(api=GenericParamDebugMock)]
    trait GenericParamDebug<T: Debug> {
        fn generic_param_debug(&self, param: T) -> &'static str;
    }

    #[test]
    #[should_panic(
        // When it has a debug bound, we should see it:
        expected = "GenericParamDebug::generic_param_debug(42): No matching call patterns."
    )]
    fn test_generic_param_panic_debug() {
        let deps = Unimock::new(
            GenericParamDebugMock::generic_param_debug
                .with_types::<i32>()
                .each_call(matching!(1337))
                .returns("a number"),
        );

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

#[cfg(feature = "std")]
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

    #[unimock(api=[Func])]
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

#[cfg(any(feature = "std", feature = "spin-lock"))]
mod method_generics {
    use super::*;

    #[unimock(api=G1)]
    trait ParamInlineBound {
        fn m1<T: 'static>(&self, a: T) -> i32;
    }

    #[unimock(api=G2)]
    trait ParamWhereBound {
        fn m2<T>(&self, a: T) -> i32
        where
            T: 'static;
    }

    #[unimock(api=G3)]
    trait ReturnInlineBound {
        fn m3<T: 'static>(&self) -> T;
    }

    #[unimock(api=G4)]
    trait ReturnWhereBound {
        fn m4<T>(&self) -> (T, &T)
        where
            T: 'static;
    }

    #[test]
    fn method_generics() {
        let u = Unimock::new((
            G1::m1
                .with_types::<&str>()
                .next_call(matching!("g1"))
                .returns(1),
            G2::m2
                .with_types::<String>()
                .next_call(matching!("g2"))
                .returns(2),
            G3::m3
                .with_types::<&str>()
                .next_call(matching!())
                .returns("g3"),
            G4::m4
                .with_types::<i32>()
                .next_call(matching!())
                .returns((4, 4)),
        ));

        assert_eq!(1, u.m1("g1"));
        assert_eq!(2, u.m2("g2".to_string()));
        assert_eq!("g3", u.m3::<&str>());
        assert_eq!((4, &4), u.m4::<i32>());
    }
}

mod impl_trait {
    use super::*;
    use core::any::Any;

    #[unimock(api=G1)]
    trait ImplTrait1 {
        fn m1(&self, a: impl Any + 'static) -> i32;
    }

    #[unimock(api=G2)]
    trait ImplTrait2 {
        fn m2(&self, a: impl Any + 'static, b: impl Any + 'static) -> i32;
    }

    #[unimock(api=G3)]
    trait Mixed {
        fn mixed<T>(&self, a: impl Any + 'static, t: T) -> i32
        where
            T: 'static;
    }

    #[test]
    fn impl_trait_generics() {
        let u = Unimock::new((
            G1::m1
                .with_types::<i32>()
                .each_call(matching!(1))
                .returns(1),
            G2::m2
                .with_types::<i32, &str>()
                .each_call(matching!(1, "1"))
                .returns(2),
            G3::mixed
                .with_types::<i32, &str>()
                .each_call(matching!("1", 1))
                .returns(3),
        ));

        assert_eq!(1, u.m1(1));
        assert_eq!(2, u.m2(1, "1"));
        assert_eq!(3, u.mixed("1", 1));
    }
}

mod generic_combo {
    use core::any::Any;

    use super::*;

    #[unimock(api=MockCombo)]
    trait ComboRet<T: 'static> {
        fn ret<U>(&self, u: U, a: impl Any + 'static) -> (T, &U)
        where
            U: 'static;
    }

    #[cfg(feature = "std")]
    #[unimock(api=MockAsyncCombo)]
    #[async_trait::async_trait]
    trait AsyncTraitGenerics<T: 'static + Send> {
        async fn ret<U: 'static + Send>(&self, u: U, a: impl Any + Send + 'static) -> T;
    }
}

mod self_type {
    use super::*;

    pub struct Generic<T>(T);

    #[unimock(api=SelfParamMock)]
    trait SelfParam {
        fn self_param(&self, p: Generic<Self>)
        where
            Self: Sized;
    }

    #[unimock(api=SelfReturnMock)]
    trait SelfReturn {
        fn self_return(&self) -> Generic<Self>
        where
            Self: Sized;
    }
}

mod issue_37_mutation_with_generics {
    use super::*;

    trait Bound: 'static {}

    pub struct MyFoo {
        baz: u32,
    }

    #[unimock(api=MockMock)]
    trait Mock {
        fn func<T>(&self, nasty: T, foo: &mut MyFoo)
        where
            T: Bound;
    }

    #[test]
    fn test() {
        MockMock::func
            .with_types::<()>()
            .next_call(matching!())
            .mutates(|foo, _| {
                foo.baz += 1;
            });
    }
}
