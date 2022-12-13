use crate::{call_pattern::DynResponder, value_chain::ValueChain, MockFn, Responder};
use std::borrow::Borrow;

/// Trait for responding to function calls.
pub trait Respond {
    /// The type of the response, as stored temporarily inside Unimock.
    type Type: 'static;
}

/// Trait for values that can be converted into responses.
///
/// This can be implemented by types that do not implement `Clone`.
pub trait IntoResponseOnce<R: Respond> {
    // Convert this type into the output type.
    #[doc(hidden)]
    fn into_response(self) -> <R as Respond>::Type;

    // Convert this type directly into a responder that can respond (at least) once.
    #[doc(hidden)]
    fn into_once_responder<F: MockFn<Response = R>>(self) -> Responder;
}

/// Trait for `Clone` values which can be converted into a reusable multi-value responder.
pub trait IntoResponseClone<R: Respond>: IntoResponseOnce<R> {
    #[doc(hidden)]
    fn into_clone_responder<F: MockFn<Response = R>>(self) -> Responder;
}

/// Trait that describes the output of a mocked function, and how responses are converted into that type.
///
/// The trait uses the 'u lifetime, which is the lifetime of unimock itself.
/// This way it's possible to borrow values stored inside the instance.
pub trait Output<'u, R: Respond> {
    /// The type of the output compatible with the function signature.
    type Type;

    #[doc(hidden)]
    fn from_response(response: R::Type, value_chain: &'u ValueChain) -> Self::Type;

    #[doc(hidden)]
    fn try_from_borrowed_response(response: &'u R::Type) -> Result<Self::Type, SignatureError>;
}

#[doc(hidden)]
pub enum SignatureError {
    NotOwned,
    NotBorrowed,
}

#[doc(hidden)]
pub struct Owned<T>(std::marker::PhantomData<T>);

// This type describes a function response that is a reference borrowed from `Self`.
#[doc(hidden)]
pub struct Borrowed<T: ?Sized + 'static>(std::marker::PhantomData<T>);

#[doc(hidden)]
pub struct StaticRef<T: ?Sized>(std::marker::PhantomData<T>);

// This type describes a function response that is a mix of owned and borrowed data.
//
// The typical example is `Option<&T>`.
#[doc(hidden)]
pub struct Mixed<T>(std::marker::PhantomData<T>);

type BoxBorrow<T> = Box<dyn Borrow<T> + Send + Sync>;

mod owned {
    use super::*;

    impl<T: 'static> Respond for Owned<T> {
        type Type = T;
    }

    impl<T0, T: Send + Sync + 'static> IntoResponseOnce<Owned<T>> for T0
    where
        T0: Into<T>,
    {
        fn into_response(self) -> <Owned<T> as Respond>::Type {
            self.into()
        }

        fn into_once_responder<F: MockFn<Response = Owned<T>>>(self) -> Responder {
            let response = <T0 as IntoResponseOnce<Owned<T>>>::into_response(self);
            Responder(DynResponder::new_cell::<F>(response))
        }
    }

    impl<T0, T: Clone + Send + Sync + 'static> IntoResponseClone<Owned<T>> for T0
    where
        T0: Into<T>,
    {
        fn into_clone_responder<F: MockFn<Response = Owned<T>>>(self) -> Responder {
            let response = <T0 as IntoResponseOnce<Owned<T>>>::into_response(self);
            Responder(DynResponder::new_clone_cell::<F>(response))
        }
    }

    impl<'u, T: 'static> Output<'u, Self> for Owned<T> {
        type Type = T;

        fn from_response(response: <Self as Respond>::Type, _: &'u ValueChain) -> Self::Type {
            response
        }

        fn try_from_borrowed_response(
            _: &'u <Self as Respond>::Type,
        ) -> Result<Self::Type, SignatureError> {
            Err(SignatureError::NotOwned)
        }
    }
}

mod borrowed {
    use super::*;

    impl<T: ?Sized + 'static> Respond for Borrowed<T> {
        type Type = Box<dyn Borrow<T> + Send + Sync>;
    }

    impl<T0, T> IntoResponseOnce<Borrowed<T>> for T0
    where
        T0: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_response(self) -> <Borrowed<T> as Respond>::Type {
            Box::new(self)
        }

        fn into_once_responder<F: MockFn<Response = Borrowed<T>>>(self) -> Responder {
            let response = <T0 as IntoResponseOnce<Borrowed<T>>>::into_response(self);
            Responder(DynResponder::new_borrow::<F>(response))
        }
    }

    impl<T0, T> IntoResponseClone<Borrowed<T>> for T0
    where
        T0: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_clone_responder<F: MockFn<Response = Borrowed<T>>>(self) -> Responder {
            <T0 as IntoResponseOnce<Borrowed<T>>>::into_once_responder::<F>(self)
        }
    }

    impl<'u, T: ?Sized + 'static> Output<'u, Self> for Borrowed<T> {
        type Type = &'u T;

        fn from_response(
            response: <Borrowed<T> as Respond>::Type,
            value_chain: &'u ValueChain,
        ) -> Self::Type {
            let value_ref = value_chain.add(response);

            value_ref.as_ref().borrow()
        }

        fn try_from_borrowed_response(
            response: &'u <Borrowed<T> as Respond>::Type,
        ) -> Result<Self::Type, SignatureError> {
            Ok(response.as_ref().borrow())
        }
    }
}

mod static_ref {
    use super::*;

    impl<T: ?Sized + 'static> Respond for StaticRef<T> {
        type Type = &'static T;
    }

    impl<T: ?Sized + Send + Sync + 'static> IntoResponseOnce<StaticRef<T>> for &'static T {
        fn into_response(self) -> <StaticRef<T> as Respond>::Type {
            self
        }

        fn into_once_responder<F: MockFn<Response = StaticRef<T>>>(self) -> Responder {
            let response = <Self as IntoResponseOnce<StaticRef<T>>>::into_response(self);
            Responder(DynResponder::new_borrow::<F>(response))
        }
    }

    impl<T: ?Sized + Send + Sync + 'static> IntoResponseClone<StaticRef<T>> for &'static T {
        fn into_clone_responder<F: MockFn<Response = StaticRef<T>>>(self) -> Responder {
            <Self as IntoResponseOnce<StaticRef<T>>>::into_once_responder::<F>(self)
        }
    }

    impl<'u, T: ?Sized + 'static> Output<'u, Self> for StaticRef<T> {
        type Type = &'static T;

        fn from_response(value: <Self as Respond>::Type, _: &ValueChain) -> Self::Type {
            value
        }

        fn try_from_borrowed_response(
            value: &'u <Self as Respond>::Type,
        ) -> Result<Self::Type, SignatureError> {
            Ok(*value)
        }
    }
}

mod mixed_option {
    use super::*;

    type Mix<T> = Mixed<Option<&'static T>>;

    impl<T: ?Sized + 'static> Respond for Mix<T> {
        type Type = Option<BoxBorrow<T>>;
    }

    impl<T0, T> IntoResponseOnce<Mix<T>> for Option<T0>
    where
        T0: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_response(self) -> <Mix<T> as Respond>::Type {
            match self {
                Some(value) => Some(Box::new(value)),
                None => None,
            }
        }

        fn into_once_responder<F: MockFn<Response = Mix<T>>>(self) -> Responder {
            let response = <Self as IntoResponseOnce<Mix<T>>>::into_response(self);
            Responder(DynResponder::new_borrow::<F>(response))
        }
    }

    impl<T0, T> IntoResponseClone<Mix<T>> for Option<T0>
    where
        T0: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_clone_responder<F: MockFn<Response = Mix<T>>>(self) -> Responder {
            let response = <Self as IntoResponseOnce<Mix<T>>>::into_response(self);
            Responder(DynResponder::new_borrow::<F>(response))
        }
    }

    impl<'u, T> Output<'u, Mix<T>> for Mixed<Option<&'u T>>
    where
        T: ?Sized + 'u,
    {
        type Type = Option<&'u T>;

        fn from_response(
            response: <Mix<T> as Respond>::Type,
            value_chain: &'u ValueChain,
        ) -> Self::Type {
            match response {
                Some(value) => Some(value_chain.add(value).as_ref().borrow()),
                None => None,
            }
        }

        fn try_from_borrowed_response(
            response: &'u <Mix<T> as Respond>::Type,
        ) -> Result<Self::Type, SignatureError> {
            Ok(match response {
                Some(value) => Some(value.as_ref().borrow()),
                None => None,
            })
        }
    }
}

mod mixed_result_borrowed_t {
    use super::*;

    type Mix<T, E> = Mixed<Result<&'static T, E>>;

    impl<T: ?Sized + 'static, E: 'static> Respond for Mix<T, E> {
        type Type = Result<BoxBorrow<T>, E>;
    }

    impl<T0, T, E> IntoResponseOnce<Mix<T, E>> for Result<T0, E>
    where
        T0: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
        E: Send + Sync + 'static,
    {
        fn into_response(self) -> <Mix<T, E> as Respond>::Type {
            match self {
                Ok(value) => Ok(Box::new(value)),
                Err(e) => Err(e),
            }
        }

        fn into_once_responder<F: MockFn<Response = Mix<T, E>>>(self) -> Responder {
            match self {
                // In the Ok variant we make a multi-value responder out of it anyway:
                Ok(value) => Responder(DynResponder::new_borrow::<F>(Ok(Box::new(value)))),
                // The Err variant can only be used once:
                Err(error) => Responder(DynResponder::new_cell::<F>(Err(error))),
            }
        }
    }

    impl<T0, T, E> IntoResponseClone<Mix<T, E>> for Result<T0, E>
    where
        T0: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
        E: Clone + Send + Sync + 'static,
    {
        fn into_clone_responder<F: MockFn<Response = Mix<T, E>>>(self) -> Responder {
            match self {
                // There is no `T0: Clone` bound, because it just uses the borrow responder mechanism:
                Ok(value) => Responder(DynResponder::new_borrow::<F>(Ok(Box::new(value)))),
                // We have `E: Clone` because the E is in fact owned...
                Err(error) => Responder(DynResponder::new_clone_factory_cell::<F>(move || {
                    Some(Err(error.clone()))
                })),
            }
        }
    }

    impl<'u, T, E: 'static> Output<'u, Mix<T, E>> for Mixed<Result<&'u T, E>>
    where
        T: ?Sized + 'u,
    {
        type Type = Result<&'u T, E>;

        fn from_response(
            response: <Mix<T, E> as Respond>::Type,
            value_chain: &'u ValueChain,
        ) -> Self::Type {
            match response {
                Ok(value) => Ok(value_chain.add(value).as_ref().borrow()),
                Err(e) => Err(e),
            }
        }

        fn try_from_borrowed_response(
            response: &'u <Mix<T, E> as Respond>::Type,
        ) -> Result<Self::Type, SignatureError> {
            match response {
                Ok(value) => Ok(Ok(value.as_ref().borrow())),
                // No chance of converting the E into owned here:
                Err(_) => Err(SignatureError::NotOwned),
            }
        }
    }
}
