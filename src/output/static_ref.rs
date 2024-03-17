use super::*;

#[doc(hidden)]
pub struct StaticRef<T: ?Sized>(core::marker::PhantomData<fn() -> T>);

impl<T: ?Sized + Send + Sync + 'static> Kind for StaticRef<T> {
    type Return = Reference<T>;
    type Respond = Reference<T>;
}

impl<T: ?Sized + Send + Sync + 'static> Return for StaticRef<T> {
    type Type = &'static T;
}

impl<T: ?Sized + Send + Sync + 'static> IntoReturnOnce<StaticRef<T>> for &'static T {
    fn into_return_once(self) -> OutputResult<Reference<T>> {
        Ok(Reference(self))
    }
}

impl<T: ?Sized + Send + Sync + 'static> IntoReturn<StaticRef<T>> for &'static T {
    fn into_return(self) -> OutputResult<Reference<T>> {
        Ok(Reference(self))
    }
}

impl<T: ?Sized + Send + Sync + 'static> IntoRespond<StaticRef<T>> for &'static T {
    fn into_respond(self) -> OutputResult<Reference<T>> {
        Ok(Reference(self))
    }
}

pub struct Reference<T: ?Sized + 'static>(pub(crate) &'static T);

impl<T: ?Sized + Send + Sync + 'static> GetOutput for Reference<T> {
    type Output<'u> = &'static T;

    fn output(&self) -> Option<Self::Output<'_>> {
        Some(self.0)
    }
}

impl<T: ?Sized + Send + Sync + 'static> IntoOutput for Reference<T> {
    type Output<'u> = &'static T;

    fn into_output(self, _value_chain: &ValueChain) -> Self::Output<'_> {
        self.0
    }
}
