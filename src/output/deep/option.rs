use crate::output::*;

type Mix<K> = Deep<Option<K>>;

impl<K: Kind> Kind for Mix<K> {
    type Return = AsReturn<K>;
}

impl<K> Return for Mix<K>
where
    K: Return,
    <K as Return>::Type: 'static + Send + Sync,
{
    type Type = Option<<K as Return>::Type>;
}

pub enum AsReturn<K: Kind> {
    Some(K::Return),
    None,
}

impl<K: Kind> GetOutput for AsReturn<K>
where
    Self: 'static,
{
    type Output<'u> =
        Option<
            <<K as Kind>::Return as GetOutput>::Output<'u>,
        >
        where
            Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        match self {
            Self::Some(val) => Some(Some(val.output()?)),
            Self::None => Some(None),
        }
    }
}

impl<T, K: Return> IntoReturnOnce<Mix<K>> for Option<T>
where
    <K as Return>::Type: 'static + Send + Sync,
    T: IntoReturnOnce<K>,
{
    fn into_return_once(self) -> OutputResult<AsReturn<K>> {
        match self {
            Some(val) => Ok(AsReturn::Some(val.into_return_once()?)),
            None => Ok(AsReturn::None),
        }
    }
}

impl<T, K: Return> IntoReturn<Mix<K>> for Option<T>
where
    <K as Return>::Type: 'static + Send + Sync,
    T: IntoReturn<K>,
{
    fn into_return(self) -> OutputResult<AsReturn<K>> {
        match self {
            Some(val) => Ok(AsReturn::Some(val.into_return()?)),
            None => Ok(AsReturn::None),
        }
    }
}
