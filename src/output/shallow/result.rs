//! Covers a shallow Result<&T, E>

use core::borrow::Borrow;

use crate::output::{owning::Owned, *};

type Mix<T, E> = Shallow<Result<&'static T, E>>;

type Response<T, E> = Result<Box<dyn Borrow<T> + Send + Sync>, Owned<E>>;

impl<T: ?Sized, E: 'static> Kind for Mix<T, E> {
    type Return = Response<T, E>;
}

impl<T: ?Sized, E: 'static> Return for Mix<T, E>
where
    Owning<E>: Return,
{
    type Type = Result<Box<dyn Borrow<T> + Send + Sync>, <Owning<E> as Return>::Type>;
}

impl<T: ?Sized + 'static, E: 'static> GetOutput for Response<T, E> {
    type Output<'u> = Result<&'u T, E>
        where
            Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        Some(match self {
            Ok(val) => Ok(val.as_ref().borrow()),
            Err(err) => Err(err.output()?),
        })
    }
}

macro_rules! into {
    ($trait:ident, $method:ident) => {
        impl<T0, T: ?Sized + 'static, E0, E: 'static> $trait<Mix<T, E>> for Result<T0, E0>
        where
            T0: Borrow<T> + Send + Sync + 'static,
            E0: $trait<Owning<E>>,
        {
            fn $method(self) -> OutputResult<Response<T, E>> {
                Ok(match self {
                    Ok(val) => Ok(Box::new(val)),
                    Err(err) => Err(err.$method()?),
                })
            }
        }
    };
}

into!(IntoReturnOnce, into_return_once);
into!(IntoReturn, into_return);
