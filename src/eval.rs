use crate::call_pattern::{CallPattern, DynResponder, PatIndex};
use crate::error;
use crate::error::{MockError, MockResult};
use crate::macro_api::Evaluation;
use crate::mock_impl::{MockImpl, PatternMatchMode};
use crate::{DynMockFn, SharedState};
use crate::{FallbackMode, MockFn, MockInputs};

use std::borrow::Borrow;

enum Eval<C> {
    Continue(C),
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

    pub fn eval_sized<'i, F: MockFn + 'static>(
        self,
        inputs: <F as MockInputs<'i>>::Inputs,
    ) -> MockResult<Evaluation<'i, F::Output, F>>
    where
        F::Output: Sized,
    {
        let input_debugger = &|| F::debug_inputs(&inputs);
        let ctx = self.into_dyn_ctx(input_debugger);

        match ctx.eval_responder::<F>(&inputs)? {
            Eval::Continue((pat_index, responder)) => match responder {
                DynResponder::Value(inner) => Ok(Evaluation::Evaluated(
                    *inner.downcast::<F>()?.stored_value.box_clone(),
                )),
                DynResponder::Closure(inner) => {
                    Ok(Evaluation::Evaluated((inner.downcast::<F>()?.func)(inputs)))
                }
                DynResponder::Unmock => Ok(Evaluation::Skipped(inputs)),
                responder => Err(ctx.sized_error(pat_index, responder)),
            },
            Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
        }
    }

    pub fn eval_unsized_self_borrowed<'i, F: MockFn + 'static>(
        self,
        inputs: <F as MockInputs<'i>>::Inputs,
    ) -> MockResult<Evaluation<'i, &'u F::Output, F>> {
        let input_debugger = &|| F::debug_inputs(&inputs);
        let ctx = self.into_dyn_ctx(input_debugger);

        match ctx.eval_responder::<F>(&inputs)? {
            Eval::Continue((pat_index, responder)) => match responder {
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
                responder => Err(ctx.unsized_self_borrowed_error(pat_index, responder)),
            },
            Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
        }
    }

    pub fn eval_unsized_static_ref<'i, F: MockFn + 'static>(
        self,
        inputs: <F as MockInputs<'i>>::Inputs,
    ) -> MockResult<Evaluation<'i, &'static F::Output, F>> {
        let input_debugger = &|| F::debug_inputs(&inputs);
        let ctx = self.into_dyn_ctx(input_debugger);

        match ctx.eval_responder::<F>(&inputs)? {
            Eval::Continue((pat_index, responder)) => match responder {
                DynResponder::StaticRefClosure(inner) => {
                    Ok(Evaluation::Evaluated((inner.downcast::<F>()?.func)(inputs)))
                }
                DynResponder::Unmock => Ok(Evaluation::Skipped(inputs)),
                responder => Err(ctx.unsized_static_ref_error(pat_index, responder)),
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
                    name: self.mock_fn.name,
                    inputs_debug: self.debug_inputs(),
                    pat_index,
                }
            }
            DynResponder::Panic(msg) => MockError::ExplicitPanic {
                name: self.mock_fn.name,
                inputs_debug: self.debug_inputs(),
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
                name: self.mock_fn.name,
                inputs_debug: self.debug_inputs(),
                pat_index,
            },
            DynResponder::Panic(msg) => MockError::ExplicitPanic {
                name: self.mock_fn.name,
                inputs_debug: self.debug_inputs(),
                pat_index,
                msg: msg.clone(),
            },
            _ => panic!("Responder error not handled"),
        }
    }

    #[inline(never)]
    fn unsized_static_ref_error(&self, pat_index: PatIndex, responder: &DynResponder) -> MockError {
        match responder {
            DynResponder::Value(_) => MockError::CannotBorrowValueStatically {
                name: self.mock_fn.name,
                inputs_debug: self.debug_inputs(),
                pat_index,
            },
            DynResponder::Closure(_) => MockError::CannotBorrowValueProducedByClosure {
                name: self.mock_fn.name,
                inputs_debug: self.debug_inputs(),
                pat_index,
            },
            DynResponder::Borrowable(_) => MockError::CannotBorrowValueStatically {
                name: self.mock_fn.name,
                inputs_debug: self.debug_inputs(),
                pat_index,
            },
            DynResponder::Panic(msg) => MockError::ExplicitPanic {
                name: self.mock_fn.name,
                inputs_debug: self.debug_inputs(),
                pat_index,
                msg: msg.clone(),
            },
            _ => panic!("Responder error not handled"),
        }
    }

    fn eval_responder<'i, F: MockFn + 'static>(
        &self,
        inputs: &<F as MockInputs<'i>>::Inputs,
    ) -> MockResult<Eval<(PatIndex, &'u DynResponder)>> {
        match self.eval_mock_op()? {
            Eval::Continue(dyn_mock_impl) => {
                let matched_pattern = match dyn_mock_impl.pattern_match_mode {
                    PatternMatchMode::InAnyOrder => dyn_mock_impl
                        .call_patterns
                        .iter()
                        .enumerate()
                        .filter_map(|(pat_index, pattern)| {
                            match pattern.match_inputs::<F>(inputs) {
                                Ok(false) => None,
                                Ok(true) => Some(Ok((PatIndex(pat_index), pattern))),
                                Err(err) => Some(Err(err)),
                            }
                        })
                        .next()
                        .transpose()?,
                    PatternMatchMode::InOrder => self
                        .try_select_in_order_call_pattern(dyn_mock_impl, &|pattern| {
                            pattern.match_inputs::<F>(inputs)
                        })?,
                };

                self.select_next_responder(matched_pattern)
            }
            Eval::Unmock => Ok(Eval::Unmock),
        }
    }

    #[inline(never)]
    fn select_next_responder(
        &self,
        matched_pattern: Option<(PatIndex, &'u CallPattern)>,
    ) -> MockResult<Eval<(PatIndex, &'u DynResponder)>> {
        match matched_pattern {
            Some((pat_index, pattern)) => match pattern.next_responder() {
                Some(responder) => Ok(Eval::Continue((pat_index, responder))),
                None => Err(MockError::NoOutputAvailableForCallPattern {
                    name: self.mock_fn.name,
                    inputs_debug: self.debug_inputs(),
                    pat_index,
                }),
            },
            None => match self.shared_state.fallback_mode {
                FallbackMode::Error => Err(MockError::NoMatchingCallPatterns {
                    name: self.mock_fn.name,
                    inputs_debug: self.debug_inputs(),
                }),
                FallbackMode::Unmock => Ok(Eval::Unmock),
            },
        }
    }

    #[inline(never)]
    fn eval_mock_op(&self) -> MockResult<Eval<&'u MockImpl>> {
        match self.shared_state.impls.get(&self.mock_fn.type_id) {
            None => match self.shared_state.fallback_mode {
                FallbackMode::Error => Err(MockError::NoMockImplementation {
                    name: self.mock_fn.name,
                }),
                FallbackMode::Unmock => Ok(Eval::Unmock),
            },
            Some(dyn_impl) => Ok(Eval::Continue(dyn_impl)),
        }
    }

    #[inline(never)]
    fn try_select_in_order_call_pattern(
        &self,
        mock_impl: &'u MockImpl,
        match_inputs: &dyn Fn(&CallPattern) -> MockResult<bool>,
    ) -> MockResult<Option<(PatIndex, &'u CallPattern)>> {
        // increase call index here, because stubs should not influence it:
        let global_call_index = self
            .shared_state
            .next_call_index
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        let (pat_index, pattern) = mock_impl
            .call_patterns
            .iter()
            .enumerate()
            .find(|(_, pattern)| {
                pattern.call_index_range.start <= global_call_index
                    && pattern.call_index_range.end > global_call_index
            })
            .ok_or_else(|| MockError::CallOrderNotMatchedForMockFn {
                name: self.mock_fn.name,
                inputs_debug: self.debug_inputs(),
                actual_call_order: error::CallOrder(global_call_index),
                expected_ranges: mock_impl
                    .call_patterns
                    .iter()
                    .map(|pattern| std::ops::Range {
                        start: pattern.call_index_range.start + 1,
                        end: pattern.call_index_range.end + 1,
                    })
                    .collect(),
            })?;

        if !match_inputs(pattern)? {
            return Err(MockError::InputsNotMatchedInCallOrder {
                name: self.mock_fn.name,
                inputs_debug: self.debug_inputs(),
                actual_call_order: error::CallOrder(global_call_index),
                pat_index: PatIndex(pat_index),
            });
        }

        Ok(Some((PatIndex(pat_index), pattern)))
    }

    fn debug_inputs(&self) -> String {
        (self.input_debugger)()
    }
}
