use crate::call_pattern::{CallPattern, DynResponder, PatIndex};
use crate::error;
use crate::error::{Lender, MockError, MockResult};
use crate::fn_mocker::{FnMocker, PatternMatchMode};
use crate::macro_api::Evaluation;
use crate::{DynMockFn, SharedState};
use crate::{FallbackMode, MockFn, MockInputs};

use std::borrow::Borrow;

enum Eval<'u> {
    Responder(PatIndex, &'u DynResponder),
    Unmock,
}

/// 'u = unimock instance
pub(crate) struct EvalCtx<'u> {
    mock_fn: DynMockFn,
    shared_state: &'u SharedState,
}

impl<'u> EvalCtx<'u> {
    pub fn new<F: MockFn>(shared_state: &'u SharedState) -> Self {
        Self {
            mock_fn: DynMockFn::new::<F>(),
            shared_state,
        }
    }

    #[allow(clippy::needless_lifetimes)]
    pub fn eval_sized<'i, F: MockFn>(
        self,
        inputs: <F as MockInputs<'i>>::Inputs,
    ) -> MockResult<Evaluation<'i, F::Output, F>>
    where
        F::Output: Sized,
    {
        let input_debugger = &|| F::debug_inputs(&inputs);
        let dyn_ctx = self.into_dyn_ctx(input_debugger);

        match dyn_ctx.eval(&|pattern| pattern.match_inputs::<F>(&inputs))? {
            Eval::Responder(pat_index, responder) => match responder {
                DynResponder::Value(inner) => Ok(Evaluation::Evaluated(
                    *inner.downcast::<F>()?.stored_value.box_clone(),
                )),
                DynResponder::Closure(inner) => {
                    Ok(Evaluation::Evaluated((inner.downcast::<F>()?.func)(inputs)))
                }
                DynResponder::Unmock => Ok(Evaluation::Skipped(inputs)),
                responder => Err(dyn_ctx.sized_error(pat_index, responder)),
            },
            Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
        }
    }

    pub fn eval_unsized_self_borrowed<'i, F: MockFn>(
        self,
        inputs: <F as MockInputs<'i>>::Inputs,
    ) -> MockResult<Evaluation<'i, &'u F::Output, F>> {
        let input_debugger = &|| F::debug_inputs(&inputs);
        let dyn_ctx = self.into_dyn_ctx(input_debugger);

        match dyn_ctx.eval(&|pattern| pattern.match_inputs::<F>(&inputs))? {
            Eval::Responder(pat_index, responder) => match responder {
                DynResponder::Value(inner) => Ok(Evaluation::Evaluated(
                    inner.downcast::<F>()?.stored_value.borrow_stored(),
                )),
                DynResponder::StaticRefClosure(inner) => {
                    Ok(Evaluation::Evaluated((inner.downcast::<F>()?.func)(inputs)))
                }
                DynResponder::Borrowable(inner) => {
                    let borrowable: &dyn Borrow<<F as MockFn>::Output> =
                        inner.downcast::<F>()?.borrowable.as_ref();
                    let borrow = borrowable.borrow();
                    Ok(Evaluation::Evaluated(borrow))
                }
                DynResponder::Unmock => Ok(Evaluation::Skipped(inputs)),
                responder => Err(dyn_ctx.unsized_self_borrowed_error(pat_index, responder)),
            },
            Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
        }
    }

    pub fn eval_unsized_static_ref<'i, F: MockFn>(
        self,
        inputs: <F as MockInputs<'i>>::Inputs,
        lender: Lender,
    ) -> MockResult<Evaluation<'i, &'static F::Output, F>> {
        let input_debugger = &|| F::debug_inputs(&inputs);
        let dyn_ctx = self.into_dyn_ctx(input_debugger);

        match dyn_ctx.eval(&|pattern| pattern.match_inputs::<F>(&inputs))? {
            Eval::Responder(pat_index, responder) => match responder {
                DynResponder::StaticRefClosure(inner) => {
                    Ok(Evaluation::Evaluated((inner.downcast::<F>()?.func)(inputs)))
                }
                DynResponder::Unmock => Ok(Evaluation::Skipped(inputs)),
                responder => Err(dyn_ctx.unsized_static_borrow_error(pat_index, responder, lender)),
            },
            Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
        }
    }

    fn into_dyn_ctx<'s>(self, input_debugger: &'s dyn Fn() -> String) -> DynCtx<'u, 's> {
        DynCtx {
            mock_fn: self.mock_fn,
            shared_state: self.shared_state,
            input_debugger,
        }
    }
}

/// 'u = unimock instance, 's = stack
struct DynCtx<'u, 's> {
    mock_fn: DynMockFn,
    shared_state: &'u SharedState,
    input_debugger: &'s dyn Fn() -> String,
}

impl<'u, 's> DynCtx<'u, 's> {
    #[inline(never)]
    fn sized_error(&self, pat_index: PatIndex, responder: &DynResponder) -> MockError {
        match responder {
            DynResponder::StaticRefClosure(_) | DynResponder::Borrowable(_) => {
                MockError::TypeMismatchExpectedOwnedInsteadOfBorrowed {
                    fn_call: self.fn_call(),
                    pat_index,
                }
            }
            DynResponder::Panic(msg) => MockError::ExplicitPanic {
                fn_call: self.fn_call(),
                pat_index,
                msg: msg.clone(),
            },
            _ => panic!("Responder error not handled"),
        }
    }

    #[inline(never)]
    fn unsized_self_borrowed_error(
        &self,
        pat_index: PatIndex,
        responder: &DynResponder,
    ) -> MockError {
        match responder {
            DynResponder::Closure(_) => MockError::CannotBorrowValueProducedByClosure {
                fn_call: self.fn_call(),
                pat_index,
                lender: Lender::Unimock,
            },
            DynResponder::Panic(msg) => MockError::ExplicitPanic {
                fn_call: self.fn_call(),
                pat_index,
                msg: msg.clone(),
            },
            DynResponder::Unmock
            | DynResponder::Borrowable(_)
            | DynResponder::StaticRefClosure(_)
            | DynResponder::Value(_) => panic!("not an error"),
        }
    }

    #[inline(never)]
    fn unsized_static_borrow_error(
        &self,
        pat_index: PatIndex,
        responder: &DynResponder,
        lender: Lender,
    ) -> MockError {
        match responder {
            DynResponder::Value(_) => MockError::CannotBorrowInvalidLifetime {
                fn_call: self.fn_call(),
                pat_index,
                lender,
            },
            DynResponder::Closure(_) => MockError::CannotBorrowValueProducedByClosure {
                fn_call: self.fn_call(),
                pat_index,
                lender,
            },
            DynResponder::Borrowable(_) => MockError::CannotBorrowInvalidLifetime {
                fn_call: self.fn_call(),
                pat_index,
                lender,
            },
            DynResponder::Panic(msg) => MockError::ExplicitPanic {
                fn_call: self.fn_call(),
                pat_index,
                msg: msg.clone(),
            },
            DynResponder::StaticRefClosure(_) | DynResponder::Unmock => panic!("not an error"),
        }
    }

    #[inline(never)]
    fn eval(
        &self,
        match_inputs: &dyn Fn(&CallPattern) -> MockResult<bool>,
    ) -> MockResult<Eval<'u>> {
        let fn_mocker = match self.shared_state.fn_mockers.get(&self.mock_fn.type_id) {
            None => match self.shared_state.fallback_mode {
                FallbackMode::Error => {
                    return Err(MockError::NoMockImplementation {
                        name: self.mock_fn.name,
                    })
                }
                FallbackMode::Unmock => return Ok(Eval::Unmock),
            },
            Some(fn_mocker) => fn_mocker,
        };

        match self.match_call_pattern(fn_mocker, match_inputs)? {
            Some((pat_index, pattern)) => match pattern.next_responder() {
                Some(responder) => Ok(Eval::Responder(pat_index, responder)),
                None => Err(MockError::NoOutputAvailableForCallPattern {
                    fn_call: self.fn_call(),
                    pat_index,
                }),
            },
            None => match self.shared_state.fallback_mode {
                FallbackMode::Error => Err(MockError::NoMatchingCallPatterns {
                    fn_call: self.fn_call(),
                }),
                FallbackMode::Unmock => Ok(Eval::Unmock),
            },
        }
    }

    fn match_call_pattern(
        &self,
        fn_mocker: &'u FnMocker,
        match_inputs: &dyn Fn(&CallPattern) -> MockResult<bool>,
    ) -> MockResult<Option<(PatIndex, &'u CallPattern)>> {
        match fn_mocker.pattern_match_mode {
            PatternMatchMode::InAnyOrder => fn_mocker
                .call_patterns
                .iter()
                .enumerate()
                .filter_map(|(pat_index, pattern)| match match_inputs(pattern) {
                    Ok(false) => None,
                    Ok(true) => Some(Ok((PatIndex(pat_index), pattern))),
                    Err(err) => Some(Err(err)),
                })
                .next()
                .transpose(),
            PatternMatchMode::InOrder => {
                let ordered_call_index = self
                    .shared_state
                    .next_ordered_call_index
                    .fetch_add(1, std::sync::atomic::Ordering::SeqCst);

                let (pat_index, pattern) = fn_mocker
                    .call_patterns
                    .iter()
                    .enumerate()
                    .find(|(_, pattern)| {
                        pattern.ordered_call_index_range.start <= ordered_call_index
                            && pattern.ordered_call_index_range.end > ordered_call_index
                    })
                    .ok_or_else(|| MockError::CallOrderNotMatchedForMockFn {
                        fn_call: self.fn_call(),
                        actual_call_order: error::CallOrder(ordered_call_index),
                        expected_ranges: fn_mocker
                            .call_patterns
                            .iter()
                            .map(|pattern| std::ops::Range {
                                start: pattern.ordered_call_index_range.start + 1,
                                end: pattern.ordered_call_index_range.end + 1,
                            })
                            .collect(),
                    })?;

                if !match_inputs(pattern)? {
                    return Err(MockError::InputsNotMatchedInCallOrder {
                        fn_call: self.fn_call(),
                        actual_call_order: error::CallOrder(ordered_call_index),
                        pat_index: PatIndex(pat_index),
                    });
                }

                Ok(Some((PatIndex(pat_index), pattern)))
            }
        }
    }

    fn fn_call(&self) -> error::FnCall {
        error::FnCall {
            mock_fn: self.mock_fn.clone(),
            inputs_debug: self.debug_inputs(),
        }
    }

    fn debug_inputs(&self) -> String {
        (self.input_debugger)()
    }
}
