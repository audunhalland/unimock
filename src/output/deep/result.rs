use crate::output::*;

type Mix<TK, EK> = Deep<Result<TK, EK>>;

impl<TK, EK> Kind for Mix<TK, EK>
where
    TK: Kind,
    EK: Kind,
{
    type Return = AsReturn<TK, EK>;
    type Respond = AsRespond<TK, EK>;
}

impl<TK, EK> Return for Mix<TK, EK>
where
    TK: Return,
    <TK as Return>::Type: 'static + Send + Sync,
    EK: Return,
    <EK as Return>::Type: 'static + Send + Sync,
{
    type Type = Result<<TK as Return>::Type, <EK as Return>::Type>;
}

pub enum AsReturn<T: Kind, E: Kind> {
    Ok(T::Return),
    Err(E::Return),
}

impl<TK, EK> GetOutput for AsReturn<TK, EK>
where
    TK: Kind,
    EK: Kind,
    Self: 'static,
{
    type Output<'u> =
        Result<
            <<TK as Kind>::Return as GetOutput>::Output<'u>,
            <<EK as Kind>::Return as GetOutput>::Output<'u>,
        >
        where
            Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        match self {
            Self::Ok(val) => Some(Ok(val.output()?)),
            Self::Err(val) => Some(Err(val.output()?)),
        }
    }
}

pub enum AsRespond<TK: Kind, EK: Kind> {
    Ok(TK::Respond),
    Err(EK::Respond),
}

impl<TK, EK> IntoOutput for AsRespond<TK, EK>
where
    TK: Kind,
    EK: Kind,
    Self: 'static,
{
    type Output<'u> =
        Result<
            <<TK as Kind>::Respond as IntoOutput>::Output<'u>,
            <<EK as Kind>::Respond as IntoOutput>::Output<'u>,
        >
        where
            Self: 'u;

    fn into_output(self, value_chain: &ValueChain) -> Self::Output<'_> {
        match self {
            Self::Ok(val) => Ok(val.into_output(value_chain)),
            Self::Err(val) => Err(val.into_output(value_chain)),
        }
    }
}

impl<T, TK, E, EK> IntoReturnOnce<Mix<TK, EK>> for Result<T, E>
where
    TK: Return,
    <TK as Return>::Type: 'static + Send + Sync,
    T: IntoReturnOnce<TK>,
    EK: Return,
    <EK as Return>::Type: 'static + Send + Sync,
    E: IntoReturnOnce<EK>,
{
    fn into_return_once(self) -> OutputResult<AsReturn<TK, EK>> {
        match self {
            Ok(val) => Ok(AsReturn::Ok(val.into_return_once()?)),
            Err(val) => Ok(AsReturn::Err(val.into_return_once()?)),
        }
    }
}

impl<T, TK, E, EK> IntoReturn<Mix<TK, EK>> for Result<T, E>
where
    TK: Return,
    <TK as Return>::Type: 'static + Send + Sync,
    T: IntoReturn<TK>,
    EK: Return,
    <EK as Return>::Type: 'static + Send + Sync,
    E: IntoReturn<EK>,
{
    fn into_return(self) -> OutputResult<AsReturn<TK, EK>> {
        match self {
            Ok(val) => Ok(AsReturn::Ok(val.into_return()?)),
            Err(val) => Ok(AsReturn::Err(val.into_return()?)),
        }
    }
}

impl<T, TK, E, EK> IntoRespond<Mix<TK, EK>> for Result<T, E>
where
    TK: Kind,
    T: IntoRespond<TK>,
    EK: Kind,
    E: IntoRespond<EK>,
{
    fn into_respond(self) -> OutputResult<AsRespond<TK, EK>> {
        match self {
            Ok(val) => Ok(AsRespond::Ok(val.into_respond()?)),
            Err(val) => Ok(AsRespond::Err(val.into_respond()?)),
        }
    }
}
