use core::borrow::Borrow;

use crate::output::*;

type Mix<T> = Shallow<Option<&'static T>>;

type Response<T> = Option<Box<dyn Borrow<T> + Send + Sync>>;

impl<T: ?Sized> Kind for Mix<T> {
    type Return = Response<T>;
    type Respond = Response<T>;
}

impl<T: ?Sized> Return for Mix<T> {
    type Type = Option<Box<dyn Borrow<T> + Send + Sync>>;
}

impl<T: ?Sized + 'static> GetOutput for Response<T> {
    type Output<'u> = Option<&'u T>
        where
            Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        Some(self.as_ref().map(|val| val.as_ref().borrow()))
    }
}

impl<T: ?Sized + 'static> IntoOutput for Response<T> {
    type Output<'u> = Option<&'u T>
        where
            Self: 'u;

    fn into_output(self, value_chain: &ValueChain) -> Self::Output<'_> {
        self.map(|val| value_chain.add(val).as_ref().borrow())
    }
}

macro_rules! into {
    ($trait:ident, $method:ident) => {
        impl<T0, T: ?Sized + 'static> $trait<Mix<T>> for Option<T0>
        where
            T0: Borrow<T> + Send + Sync + 'static,
        {
            fn $method(self) -> OutputResult<Response<T>> {
                Ok(match self {
                    Some(val) => Some(Box::new(val)),
                    None => None,
                })
            }
        }
    };
}

into!(IntoReturnOnce, into_return_once);
into!(IntoReturn, into_return);
into!(IntoRespond, into_respond);
