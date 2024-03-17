use crate::alloc::vec::Vec;
use crate::output::*;

type Mix<K> = Deep<Vec<K>>;

impl<K> Kind for Mix<K>
where
    K: Kind,
{
    type Return = AsReturn<K>;
    type Respond = AsRespond<K>;
}

impl<K> Return for Mix<K>
where
    K: Return,
    <K as Return>::Type: 'static + Send + Sync,
{
    type Type = Vec<<K as Return>::Type>;
}

pub struct AsReturn<K: Kind>(Vec<K::Return>);
pub struct AsRespond<K: Kind>(Vec<K::Respond>);

impl<K> GetOutput for AsReturn<K>
where
    K: Kind,
    Self: 'static,
{
    type Output<'u> = Vec<<<K as Kind>::Return as GetOutput>::Output<'u>>
        where Self: 'u;

    fn output(&self) -> Option<Self::Output<'_>> {
        let mut out = Vec::new();
        for el in self.0.iter() {
            out.push(el.output()?);
        }

        Some(out)
    }
}

impl<K> IntoOutput for AsRespond<K>
where
    K: Kind,
    Self: 'static,
{
    type Output<'u> = Vec<<<K as Kind>::Return as GetOutput>::Output<'u>>
        where Self: 'u;

    fn into_output(self, value_chain: &ValueChain) -> Self::Output<'_> {
        self.0
            .into_iter()
            .map(|el| el.into_output(value_chain))
            .collect()
    }
}

impl<T, K> IntoReturnOnce<Mix<K>> for Vec<T>
where
    K: Return,
    <K as Return>::Type: 'static + Send + Sync,
    T: IntoReturnOnce<K>,
{
    fn into_return_once(self) -> OutputResult<AsReturn<K>> {
        Ok(AsReturn(
            self.into_iter()
                .map(|el| el.into_return_once())
                .collect::<Result<_, _>>()?,
        ))
    }
}

impl<T, K> IntoReturn<Mix<K>> for Vec<T>
where
    K: Return,
    <K as Return>::Type: 'static + Send + Sync,
    T: IntoReturn<K>,
{
    fn into_return(self) -> OutputResult<AsReturn<K>> {
        Ok(AsReturn(
            self.into_iter()
                .map(|el| el.into_return())
                .collect::<Result<_, _>>()?,
        ))
    }
}

impl<T, K> IntoRespond<Mix<K>> for Vec<T>
where
    K: Kind,
    T: IntoRespond<K>,
{
    fn into_respond(self) -> OutputResult<AsRespond<K>> {
        Ok(AsRespond(
            self.into_iter()
                .map(|el| el.into_respond())
                .collect::<Result<_, _>>()?,
        ))
    }
}
