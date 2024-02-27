use crate::alloc::String;
use crate::*;

pub(crate) mod term {
    use crate::alloc::String;

    use crate::{build::dyn_builder::DynCallPatternBuilder, MockFnInfo};

    pub trait Sink {
        fn push(&mut self, info: MockFnInfo, builder: DynCallPatternBuilder) -> Result<(), String>;
    }
}

impl Clause for () {
    fn deconstruct(self, _: &mut dyn term::Sink) -> Result<(), String> {
        Ok(())
    }
}

macro_rules! tuple_nonterminal_impl {
    ([$($t:ident),+], [$($index:tt),+]) => {
        impl<$($t: Clause),+> Clause for ($($t,)+) {
            fn deconstruct(self, sink: &mut dyn term::Sink) -> Result<(), String> {
                $(self.$index.deconstruct(sink)?;)+
                Ok(())
            }
        }
    };
}

tuple_nonterminal_impl! { [T1, T2], [0, 1] }
tuple_nonterminal_impl! { [T1, T2, T3], [0, 1, 2] }
tuple_nonterminal_impl! { [T1, T2, T3, T4], [0, 1, 2, 3] }
tuple_nonterminal_impl! { [T1, T2, T3, T4, T5], [0, 1, 2, 3, 4] }
tuple_nonterminal_impl! { [T1, T2, T3, T4, T5, T6], [0, 1, 2, 3, 4, 5] }
tuple_nonterminal_impl! { [T1, T2, T3, T4, T5, T6, T7], [0, 1, 2, 3, 4, 5, 6] }
tuple_nonterminal_impl! { [T1, T2, T3, T4, T5, T6, T7, T8], [0, 1, 2, 3, 4, 5, 6, 7] }
tuple_nonterminal_impl! { [T1, T2, T3, T4, T5, T6, T7, T8, T9], [0, 1, 2, 3, 4, 5, 6, 7, 8] }
tuple_nonterminal_impl! { [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] }
tuple_nonterminal_impl! { [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] }
tuple_nonterminal_impl! { [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] }
tuple_nonterminal_impl! { [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12] }
tuple_nonterminal_impl! { [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13] }
tuple_nonterminal_impl! { [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14] }
tuple_nonterminal_impl! { [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] }
