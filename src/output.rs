use crate::{call_pattern::DynResponder, value_chain::ValueChain, MockFn, Responder};
use std::borrow::Borrow;

/// Trait that describes how an output value is temporarily stored by Unimock.
pub trait Output {
    /// The type of the output temporarily stored inside Unimock.
    type Type: 'static;
}

/// Trait for values that can be used as responses at least once.
///
/// This can be implemented by types that do not implement `Clone`.
pub trait IntoOutputOnce<O: Output> {
    // Convert this type into the output type.
    #[doc(hidden)]
    fn into_output(self) -> <O as Output>::Type;

    // Convert this type directly into a responder that can respond (at least) once.
    #[doc(hidden)]
    fn into_once_responder<F: MockFn<Output = O>>(self) -> Responder;
}

/// Trait for `Clone` values which can be converted into a reusable multi-value responder.
pub trait IntoOutputClone<O: Output>: IntoOutputOnce<O> {
    #[doc(hidden)]
    fn into_clone_responder<F: MockFn<Output = O>>(self) -> Responder;
}

/// Trait that describes the output signature of a mocked function.
pub trait OutputSig<'u, O: Output> {
    /// The type of the output compatible with the function signature.
    type Sig;

    #[doc(hidden)]
    fn from_output(value: O::Type, value_chain: &'u ValueChain) -> Self::Sig;

    #[doc(hidden)]
    fn try_borrow_output(value: &'u O::Type) -> Result<Self::Sig, SignatureError>;
}

#[doc(hidden)]
pub enum SignatureError {
    NotOwned,
    NotBorrowed,
}

#[doc(hidden)]
pub struct Owned<T>(std::marker::PhantomData<T>);

// This type describes a function output that is a reference borrowed from `Self`.
#[doc(hidden)]
pub struct Borrowed<T: ?Sized + 'static>(std::marker::PhantomData<T>);

#[doc(hidden)]
pub struct StaticRef<T: ?Sized>(std::marker::PhantomData<T>);

// This type describes a function output that is a mix of owned and borrowed data.
//
// The typical example is `Option<&T>`.
#[doc(hidden)]
pub struct Mixed<T>(std::marker::PhantomData<T>);

type BoxBorrow<T> = Box<dyn Borrow<T> + Send + Sync>;

mod owned {
    use super::*;

    impl<T: 'static> Output for Owned<T> {
        type Type = T;
    }

    impl<I, T: Send + Sync + 'static> IntoOutputOnce<Owned<T>> for I
    where
        I: Into<T>,
    {
        fn into_output(self) -> <Owned<T> as Output>::Type {
            self.into()
        }

        fn into_once_responder<F: MockFn<Output = Owned<T>>>(self) -> Responder {
            let output = <I as IntoOutputOnce<Owned<T>>>::into_output(self);
            Responder(DynResponder::new_owned::<F>(output))
        }
    }

    impl<I, T: Clone + Send + Sync + 'static> IntoOutputClone<Owned<T>> for I
    where
        I: Into<T>,
    {
        fn into_clone_responder<F: MockFn<Output = Owned<T>>>(self) -> Responder {
            let output = <I as IntoOutputOnce<Owned<T>>>::into_output(self);
            Responder(DynResponder::new_owned_clonable::<F>(output))
        }
    }

    impl<'u, T: 'static> OutputSig<'u, Self> for Owned<T> {
        type Sig = T;

        fn from_output(value: <Self as Output>::Type, _: &'u ValueChain) -> Self::Sig {
            value
        }

        fn try_borrow_output(_: &'u <Self as Output>::Type) -> Result<Self::Sig, SignatureError> {
            Err(SignatureError::NotOwned)
        }
    }
}

mod borrowed {
    use super::*;

    impl<T: ?Sized + 'static> Output for Borrowed<T> {
        type Type = Box<dyn Borrow<T> + Send + Sync>;
    }

    impl<I, T> IntoOutputOnce<Borrowed<T>> for I
    where
        I: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_output(self) -> <Borrowed<T> as Output>::Type {
            Box::new(self)
        }

        fn into_once_responder<F: MockFn<Output = Borrowed<T>>>(self) -> Responder {
            let output = <I as IntoOutputOnce<Borrowed<T>>>::into_output(self);
            Responder(DynResponder::new_borrow::<F>(output))
        }
    }

    impl<I, T> IntoOutputClone<Borrowed<T>> for I
    where
        I: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_clone_responder<F: MockFn<Output = Borrowed<T>>>(self) -> Responder {
            <I as IntoOutputOnce<Borrowed<T>>>::into_once_responder::<F>(self)
        }
    }

    impl<'u, T: ?Sized + 'static> OutputSig<'u, Self> for Borrowed<T> {
        type Sig = &'u T;

        fn from_output(
            value: <Borrowed<T> as Output>::Type,
            value_chain: &'u ValueChain,
        ) -> Self::Sig {
            let value_ref = value_chain.add(value);

            value_ref.as_ref().borrow()
        }

        fn try_borrow_output(
            value: &'u <Borrowed<T> as Output>::Type,
        ) -> Result<Self::Sig, SignatureError> {
            Ok(value.as_ref().borrow())
        }
    }
}

mod static_ref {
    use super::*;

    impl<T: ?Sized + 'static> Output for StaticRef<T> {
        type Type = &'static T;
    }

    impl<T: ?Sized + Send + Sync + 'static> IntoOutputOnce<StaticRef<T>> for &'static T {
        fn into_output(self) -> <StaticRef<T> as Output>::Type {
            self
        }

        fn into_once_responder<F: MockFn<Output = StaticRef<T>>>(self) -> Responder {
            let output = <Self as IntoOutputOnce<StaticRef<T>>>::into_output(self);
            Responder(DynResponder::new_borrow::<F>(output))
        }
    }

    impl<T: ?Sized + Send + Sync + 'static> IntoOutputClone<StaticRef<T>> for &'static T {
        fn into_clone_responder<F: MockFn<Output = StaticRef<T>>>(self) -> Responder {
            <Self as IntoOutputOnce<StaticRef<T>>>::into_once_responder::<F>(self)
        }
    }

    impl<'u, T: ?Sized + 'static> OutputSig<'u, Self> for StaticRef<T> {
        type Sig = &'static T;

        fn from_output(value: <Self as Output>::Type, _: &ValueChain) -> Self::Sig {
            value
        }

        fn try_borrow_output(
            value: &'u <Self as Output>::Type,
        ) -> Result<Self::Sig, SignatureError> {
            Ok(*value)
        }
    }
}

mod mixed_option {
    use super::*;

    type Mix<T> = Mixed<Option<&'static T>>;

    impl<T: ?Sized + 'static> Output for Mix<T> {
        type Type = Option<BoxBorrow<T>>;
    }

    impl<T0, T> IntoOutputOnce<Mix<T>> for Option<T0>
    where
        T0: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_output(self) -> <Mix<T> as Output>::Type {
            match self {
                Some(value) => Some(Box::new(value)),
                None => None,
            }
        }

        fn into_once_responder<F: MockFn<Output = Mix<T>>>(self) -> Responder {
            let output = <Self as IntoOutputOnce<Mix<T>>>::into_output(self);
            Responder(DynResponder::new_owned::<F>(output))
        }
    }

    impl<T0, T> IntoOutputClone<Mix<T>> for Option<T0>
    where
        T0: Borrow<T> + Clone + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_clone_responder<F: MockFn<Output = Mix<T>>>(self) -> Responder {
            Responder(DynResponder::new_clone_factory::<F>(move || {
                Some(<Self as IntoOutputOnce<Mix<T>>>::into_output(self.clone()))
            }))
        }
    }

    impl<'u, T> OutputSig<'u, Mix<T>> for Mixed<Option<&'u T>>
    where
        T: ?Sized + 'u,
    {
        type Sig = Option<&'u T>;

        fn from_output(option: <Mix<T> as Output>::Type, value_chain: &'u ValueChain) -> Self::Sig {
            match option {
                Some(value) => Some(value_chain.add(value).as_ref().borrow()),
                None => None,
            }
        }

        fn try_borrow_output(
            option: &'u <Mix<T> as Output>::Type,
        ) -> Result<Self::Sig, SignatureError> {
            Ok(match option {
                Some(value) => Some(value.as_ref().borrow()),
                None => None,
            })
        }
    }
}

mod mixed_result_borrowed_t {
    use super::*;

    type Mix<T, E> = Mixed<Result<&'static T, E>>;

    impl<T: ?Sized + 'static, E: 'static> Output for Mix<T, E> {
        type Type = Result<BoxBorrow<T>, E>;
    }

    impl<T0, T, E> IntoOutputOnce<Mix<T, E>> for Result<T0, E>
    where
        T0: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
        E: Send + Sync + 'static,
    {
        fn into_output(self) -> <Mix<T, E> as Output>::Type {
            match self {
                Ok(value) => Ok(Box::new(value)),
                Err(e) => Err(e),
            }
        }

        fn into_once_responder<F: MockFn<Output = Mix<T, E>>>(self) -> Responder {
            let output = <Self as IntoOutputOnce<Mix<T, E>>>::into_output(self);
            Responder(DynResponder::new_owned::<F>(output))
        }
    }

    impl<T0, T, E> IntoOutputClone<Mix<T, E>> for Result<T0, E>
    where
        T0: Borrow<T> + Clone + Send + Sync + 'static,
        T: ?Sized + 'static,
        E: Clone + Send + Sync + 'static,
    {
        fn into_clone_responder<F: MockFn<Output = Mix<T, E>>>(self) -> Responder {
            Responder(DynResponder::new_clone_factory::<F>(move || {
                Some(<Self as IntoOutputOnce<Mix<T, E>>>::into_output(
                    self.clone(),
                ))
            }))
        }
    }

    impl<'u, T, E: 'static> OutputSig<'u, Mix<T, E>> for Mixed<Result<&'u T, E>>
    where
        T: ?Sized + 'u,
    {
        type Sig = Result<&'u T, E>;

        fn from_output(
            result: <Mix<T, E> as Output>::Type,
            value_chain: &'u ValueChain,
        ) -> Self::Sig {
            match result {
                Ok(value) => Ok(value_chain.add(value).as_ref().borrow()),
                Err(e) => Err(e),
            }
        }

        fn try_borrow_output(
            result: &'u <Mix<T, E> as Output>::Type,
        ) -> Result<Self::Sig, SignatureError> {
            match result {
                Ok(value) => Ok(Ok(value.as_ref().borrow())),
                Err(_) => Err(SignatureError::NotOwned),
            }
        }
    }
}
