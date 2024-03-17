use core::task::Poll;

use crate::output::*;

type Mix<K> = Deep<Poll<K>>;

impl<K: Kind> Kind for Mix<K> {
    type Return = AsReturn<K>;
    type Respond = AsRespond<K>;
}

impl<K: Return> Return for Mix<K>
where
    <K as Return>::Type: 'static + Send + Sync,
{
    type Type = Poll<<K as Return>::Type>;
}

pub enum AsReturn<K: Kind> {
    Ready(K::Return),
    Pending,
}

impl<K: Kind> GetOutput for AsReturn<K>
where
    Self: 'static,
{
    type Output<'u> =
        Poll<
            <<K as Kind>::Return as GetOutput>::Output<'u>,
        >
        where
            Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        match self {
            Self::Ready(val) => Some(Poll::Ready(val.output()?)),
            Self::Pending => Some(Poll::Pending),
        }
    }
}

pub enum AsRespond<K: Kind> {
    Ready(K::Respond),
    Pending,
}

impl<K: Kind> IntoOutput for AsRespond<K>
where
    Self: 'static,
{
    type Output<'u> =
        Poll<
            <<K as Kind>::Respond as IntoOutput>::Output<'u>,
        >
        where
            Self: 'u;

    fn into_output(self, value_chain: &ValueChain) -> Self::Output<'_> {
        match self {
            Self::Ready(val) => Poll::Ready(val.into_output(value_chain)),
            Self::Pending => Poll::Pending,
        }
    }
}

impl<T, K> IntoReturnOnce<Mix<K>> for Poll<T>
where
    K: Return,
    <K as Return>::Type: 'static + Send + Sync,
    T: IntoReturnOnce<K>,
{
    fn into_return_once(self) -> OutputResult<AsReturn<K>> {
        match self {
            Self::Ready(val) => Ok(AsReturn::Ready(val.into_return_once()?)),
            Self::Pending => Ok(AsReturn::Pending),
        }
    }
}

impl<T, K: Return> IntoReturn<Mix<K>> for Poll<T>
where
    <K as Return>::Type: 'static + Send + Sync,
    T: IntoReturn<K>,
{
    fn into_return(self) -> OutputResult<AsReturn<K>> {
        match self {
            Self::Ready(val) => Ok(AsReturn::Ready(val.into_return()?)),
            Self::Pending => Ok(AsReturn::Pending),
        }
    }
}

impl<T, K: Kind> IntoRespond<Mix<K>> for Poll<T>
where
    T: IntoRespond<K>,
{
    fn into_respond(self) -> OutputResult<AsRespond<K>> {
        match self {
            Self::Ready(val) => Ok(AsRespond::Ready(val.into_respond()?)),
            Self::Pending => Ok(AsRespond::Pending),
        }
    }
}
