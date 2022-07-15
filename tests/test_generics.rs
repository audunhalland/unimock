use unimock::*;

// #[unimock]
trait GenericReturn<T> {
    fn generic_return(&self) -> T;
}

#[allow(non_camel_case_types)]
#[doc = "MockFn for `GenericReturn::generic_return() -> T`."]
struct GenericReturn__generic_return;

impl GenericReturn__generic_return {
    pub fn generic<T>(self) -> __GenericGenericReturn__generic_return<T> {
        __GenericGenericReturn__generic_return(::core::marker::PhantomData)
    }
}
#[allow(non_camel_case_types)]
struct __GenericGenericReturn__generic_return<T>(::core::marker::PhantomData<T>);

impl<'__i, T> ::unimock::MockInputs<'__i> for __GenericGenericReturn__generic_return<T> {
    type Inputs = ();
}
impl<T: 'static> ::unimock::MockFn for __GenericGenericReturn__generic_return<T> {
    type Output = T;
    const NAME: &'static str = "GenericReturn::generic_return";
    fn debug_inputs<'i>((): &<Self as ::unimock::MockInputs<'i>>::Inputs) -> String {
        ::unimock::macro_api::format_inputs(&[])
    }
}
impl<T: 'static> GenericReturn<T> for ::unimock::Unimock {
    fn generic_return(&self) -> T {
        ::unimock::macro_api::eval::<__GenericGenericReturn__generic_return<T>>(self, ())
            .unwrap(self)
    }
}
