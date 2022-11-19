use crate::{
    as_owned::AsOwned, call_pattern::DynResponder, value_chain::ValueChain, MockFn, Responder,
};
use std::borrow::Borrow;

/// Trait that describes how an output value is temporarily stored by Unimock.
pub trait Output {
    /// The type of the output temporarily stored inside Unimock.
    type Type: 'static;
}

/// Trait used to convert values into the desired function output.
pub trait IntoOutput<O: Output> {
    /// Convert this value into the needed type used for function output.
    fn into_output(self) -> <O as Output>::Type;
}

pub trait IntoOutputOnce<O: Output> {
    fn into_output2(self) -> <O as Output>::Type;

    fn into_once_responder<F: MockFn<Output = O>>(self) -> Responder;
}

pub trait IntoOutputClone2<O: Output>: IntoOutputOnce<O> {
    fn into_responder<F: MockFn<Output = O>>(self) -> Responder;
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

#[doc(hidden)]
// This type describes a function output that is a mix of owned and borrowed data.
//
// The typical example is `Option<&T>`.
pub struct Mixed<T>(std::marker::PhantomData<T>);

#[doc(hidden)]
pub struct Complex<T>(std::marker::PhantomData<T>);

type BoxBorrow<T> = Box<dyn Borrow<T> + Send + Sync>;

mod owned {
    use super::*;

    impl<T: 'static> Output for Owned<T> {
        type Type = T;
    }

    impl<I, T: 'static> IntoOutput<Owned<T>> for I
    where
        I: Into<T>,
    {
        fn into_output(self) -> <Owned<T> as Output>::Type {
            self.into()
        }
    }

    impl<I, T: Send + Sync + 'static> IntoOutputOnce<Owned<T>> for I
    where
        I: Into<T>,
    {
        fn into_output2(self) -> <Owned<T> as Output>::Type {
            self.into()
        }

        fn into_once_responder<F: MockFn<Output = Owned<T>>>(self) -> Responder {
            let output = <I as IntoOutputOnce<Owned<T>>>::into_output2(self);
            Responder(DynResponder::new_owned::<F>(output))
        }
    }

    impl<I, T: Clone + Send + Sync + 'static> IntoOutputClone2<Owned<T>> for I
    where
        I: Into<T>,
    {
        fn into_responder<F: MockFn<Output = Owned<T>>>(self) -> Responder {
            let output = <I as IntoOutputOnce<Owned<T>>>::into_output2(self);
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

    impl<I, T> IntoOutput<Borrowed<T>> for I
    where
        I: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_output(self) -> <Borrowed<T> as Output>::Type {
            Box::new(self)
        }
    }

    impl<I, T> IntoOutputOnce<Borrowed<T>> for I
    where
        I: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_output2(self) -> <Borrowed<T> as Output>::Type {
            Box::new(self)
        }

        fn into_once_responder<F: MockFn<Output = Borrowed<T>>>(self) -> Responder {
            let output = <I as IntoOutputOnce<Borrowed<T>>>::into_output2(self);
            Responder(DynResponder::new_borrow::<F>(output))
        }
    }

    impl<I, T> IntoOutputClone2<Borrowed<T>> for I
    where
        I: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_responder<F: MockFn<Output = Borrowed<T>>>(self) -> Responder {
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

    impl<T: ?Sized> IntoOutput<StaticRef<T>> for &'static T {
        fn into_output(self) -> <StaticRef<T> as Output>::Type {
            self
        }
    }

    impl<T: ?Sized + Send + Sync + 'static> IntoOutputOnce<StaticRef<T>> for &'static T {
        fn into_output2(self) -> <StaticRef<T> as Output>::Type {
            self
        }

        fn into_once_responder<F: MockFn<Output = StaticRef<T>>>(self) -> Responder {
            let output = <Self as IntoOutputOnce<StaticRef<T>>>::into_output2(self);
            Responder(DynResponder::new_borrow::<F>(output))
        }
    }

    impl<T: ?Sized + Send + Sync + 'static> IntoOutputClone2<StaticRef<T>> for &'static T {
        fn into_responder<F: MockFn<Output = StaticRef<T>>>(self) -> Responder {
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

mod mixed {
    use super::*;

    impl<T: AsOwned<'static>> Output for Mixed<T> {
        type Type = <T as AsOwned<'static>>::Owned;
    }

    impl<T: AsOwned<'static>> IntoOutput<Mixed<T>> for <T as AsOwned<'static>>::Owned {
        fn into_output(self) -> <Mixed<T> as Output>::Type {
            self
        }
    }

    impl<T: AsOwned<'static>> IntoOutputOnce<Mixed<T>> for <T as AsOwned<'static>>::Owned
    where
        <T as AsOwned<'static>>::Owned: Send + Sync,
    {
        fn into_output2(self) -> <Mixed<T> as Output>::Type {
            self
        }

        fn into_once_responder<F: MockFn<Output = Mixed<T>>>(self) -> Responder {
            let output = <Self as IntoOutputOnce<Mixed<T>>>::into_output2(self);
            Responder(DynResponder::new_owned::<F>(output))
        }
    }

    impl<T: AsOwned<'static>> IntoOutputClone2<Mixed<T>> for <T as AsOwned<'static>>::Owned
    where
        <T as AsOwned<'static>>::Owned: Clone + Send + Sync,
    {
        fn into_responder<F: MockFn<Output = Mixed<T>>>(self) -> Responder {
            let output = <Self as IntoOutputOnce<Mixed<T>>>::into_output2(self);
            Responder(DynResponder::new_owned_clonable::<F>(output))
        }
    }

    impl<'u, T, O> OutputSig<'u, O> for Mixed<T>
    where
        O: Output,
        <O as Output>::Type: Send + Sync,
        T: AsOwned<'u, Owned = O::Type>,
    {
        type Sig = T;

        fn from_output(value: <O as Output>::Type, value_chain: &'u ValueChain) -> Self::Sig {
            let value_ref = value_chain.add(value);
            <T as AsOwned>::from_owned(value_ref)
        }

        fn try_borrow_output(value: &'u <O as Output>::Type) -> Result<Self::Sig, SignatureError> {
            Ok(<T as AsOwned>::from_owned(value))
        }
    }
}

mod complex_option {
    use super::*;

    type This<T> = Complex<Option<&'static T>>;

    impl<T: ?Sized + 'static> Output for This<T> {
        type Type = Option<BoxBorrow<T>>;
    }

    impl<IT, T> IntoOutput<This<T>> for Option<IT>
    where
        IT: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_output(self) -> <This<T> as Output>::Type {
            match self {
                Some(value) => Some(Box::new(value)),
                None => None,
            }
        }
    }

    impl<IT, T> IntoOutputOnce<This<T>> for Option<IT>
    where
        IT: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_output2(self) -> <This<T> as Output>::Type {
            match self {
                Some(value) => Some(Box::new(value)),
                None => None,
            }
        }

        fn into_once_responder<F: MockFn<Output = This<T>>>(self) -> Responder {
            let output = <Self as IntoOutputOnce<This<T>>>::into_output2(self);
            Responder(DynResponder::new_owned::<F>(output))
        }
    }

    impl<IT, T> IntoOutputClone2<This<T>> for Option<IT>
    where
        IT: Borrow<T> + Clone + Send + Sync + 'static,
        T: ?Sized + 'static,
    {
        fn into_responder<F: MockFn<Output = This<T>>>(self) -> Responder {
            Responder(DynResponder::new_clone_factory::<F>(move || {
                let output = <Self as IntoOutputOnce<This<T>>>::into_output2(self.clone());
                Some(output)
            }))
        }
    }

    impl<'u, T> OutputSig<'u, This<T>> for Complex<Option<&'u T>>
    where
        T: ?Sized + 'u,
    {
        type Sig = Option<&'u T>;

        fn from_output(
            option: <This<T> as Output>::Type,
            value_chain: &'u ValueChain,
        ) -> Self::Sig {
            match option {
                Some(value) => Some(value_chain.add(value).as_ref().borrow()),
                None => None,
            }
        }

        fn try_borrow_output(
            option: &'u <This<T> as Output>::Type,
        ) -> Result<Self::Sig, SignatureError> {
            Ok(match option {
                Some(value) => Some(value.as_ref().borrow()),
                None => None,
            })
        }
    }
}

mod complex_result_borrowed_t {
    use super::*;

    type This<T, E> = Complex<Result<&'static T, E>>;

    impl<T: ?Sized + 'static, E: 'static> Output for This<T, E> {
        type Type = Result<BoxBorrow<T>, E>;
    }

    impl<IT, T, E> IntoOutput<This<T, E>> for Result<IT, E>
    where
        IT: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
        E: 'static,
    {
        fn into_output(self) -> <This<T, E> as Output>::Type {
            match self {
                Ok(value) => Ok(Box::new(value)),
                Err(e) => Err(e),
            }
        }
    }

    impl<IT, T, E> IntoOutputOnce<This<T, E>> for Result<IT, E>
    where
        IT: Borrow<T> + Send + Sync + 'static,
        T: ?Sized + 'static,
        E: Send + Sync + 'static,
    {
        fn into_output2(self) -> <This<T, E> as Output>::Type {
            match self {
                Ok(value) => Ok(Box::new(value)),
                Err(e) => Err(e),
            }
        }

        fn into_once_responder<F: MockFn<Output = This<T, E>>>(self) -> Responder {
            let output = <Self as IntoOutputOnce<This<T, E>>>::into_output2(self);
            Responder(DynResponder::new_owned::<F>(output))
        }
    }

    impl<IT, T, E> IntoOutputClone2<This<T, E>> for Result<IT, E>
    where
        IT: Borrow<T> + Clone + Send + Sync + 'static,
        T: ?Sized + 'static,
        E: Clone + Send + Sync + 'static,
    {
        fn into_responder<F: MockFn<Output = This<T, E>>>(self) -> Responder {
            Responder(DynResponder::new_clone_factory::<F>(move || {
                let output = <Self as IntoOutputOnce<This<T, E>>>::into_output2(self.clone());
                Some(output)
            }))
        }
    }

    impl<'u, T, E: 'static> OutputSig<'u, This<T, E>> for Complex<Result<&'u T, E>>
    where
        T: ?Sized + 'u,
    {
        type Sig = Result<&'u T, E>;

        fn from_output(
            result: <This<T, E> as Output>::Type,
            value_chain: &'u ValueChain,
        ) -> Self::Sig {
            match result {
                Ok(value) => Ok(value_chain.add(value).as_ref().borrow()),
                Err(e) => Err(e),
            }
        }

        fn try_borrow_output(
            result: &'u <This<T, E> as Output>::Type,
        ) -> Result<Self::Sig, SignatureError> {
            match result {
                Ok(value) => Ok(Ok(value.as_ref().borrow())),
                Err(_) => Err(SignatureError::NotOwned),
            }
        }
    }
}
