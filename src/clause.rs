use crate::build::DynCallPatternBuilder;
use crate::*;

/// Supertrait of Clause that makes that trait sealed
pub trait ClauseSealed: Sized {
    fn deconstruct(self, sink: &mut dyn TerminalSink) -> Result<(), String>;
}

pub trait TerminalSink {
    fn put_terminal(&mut self, terminal: TerminalClause) -> Result<(), String>;
}

/// Public yet hidden terminal clause
#[doc(hidden)]
pub struct TerminalClause {
    pub(crate) dyn_mock_fn: DynMockFn,
    pub(crate) builder: DynCallPatternBuilder,
}

impl ClauseSealed for () {
    fn deconstruct(self, _: &mut dyn TerminalSink) -> Result<(), String> {
        Ok(())
    }
}

macro_rules! tuple_nonterminal_impl {
    ([$($t:ident),+], [$($index:tt),+]) => {
        impl<$($t: ClauseSealed),+> ClauseSealed for ($($t,)+) {
            fn deconstruct(self, sink: &mut dyn TerminalSink) -> Result<(), String> {
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
