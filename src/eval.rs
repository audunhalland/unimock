use crate::call_pattern::{DynCallPattern, DynResponder, PatIndex};
use crate::error;
use crate::error::{MockError, MockResult};
use crate::macro_api::Evaluation;
use crate::mock_impl::{DynMockImpl, PatternMatchMode};
use crate::{DynMockFn, SharedState};
use crate::{FallbackMode, MockFn, MockInputs};

use std::borrow::Borrow;

enum Eval<C> {
    Continue(C),
    Unmock,
}

enum OutputVariant {
    Sized,
    SelfBorrowed,
    UnsizedStaticRef,
}

pub(crate) fn eval_sized<'i, F: MockFn + 'static>(
    state: &SharedState,
    inputs: <F as MockInputs<'i>>::Inputs,
) -> MockResult<Evaluation<'i, F::Output, F>>
where
    F::Output: Sized,
{
    match eval_responder::<F>(state, &inputs)? {
        Eval::Continue((pat_index, responder)) => match responder {
            DynResponder::Value(inner) => {
                Ok(Evaluation::Evaluated(*inner.downcast::<F>().0.box_clone()))
            }
            DynResponder::Closure(inner) => {
                Ok(Evaluation::Evaluated(inner.downcast::<F>().0(inputs)))
            }
            DynResponder::StaticRefClosure(_) | DynResponder::Borrowable(_) => {
                Err(MockError::TypeMismatchExpectedOwnedInsteadOfBorrowed {
                    name: F::NAME,
                    inputs_debug: F::debug_inputs(&inputs),
                    pat_index,
                })
            }
            DynResponder::Panic(msg) => Err(MockError::ExplicitPanic {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
                msg: msg.clone(),
            }),
            DynResponder::Unmock => Ok(Evaluation::Skipped(inputs)),
        },
        Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
    }
}

pub(crate) fn eval_unsized_self_borrowed<'u, 'i, F: MockFn + 'static>(
    state: &'u SharedState,
    inputs: <F as MockInputs<'i>>::Inputs,
) -> MockResult<Evaluation<'i, &'u F::Output, F>> {
    match eval_responder::<F>(state, &inputs)? {
        Eval::Continue((pat_index, responder)) => match responder {
            DynResponder::Value(inner) => Ok(Evaluation::Evaluated(
                inner.downcast::<F>().0.borrow_stored(),
            )),
            DynResponder::Closure(_) => Err(MockError::CannotBorrowValueProducedByClosure {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
            }),
            DynResponder::StaticRefClosure(inner) => {
                Ok(Evaluation::Evaluated(inner.downcast::<F>().0(inputs)))
            }
            DynResponder::Borrowable(inner) => {
                let borrowable: &dyn Borrow<<F as MockFn>::Output> =
                    inner.downcast::<F>().0.as_ref();
                let borrow = borrowable.borrow();
                Ok(Evaluation::Evaluated(borrow))
            }
            DynResponder::Panic(msg) => Err(MockError::ExplicitPanic {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
                msg: msg.clone(),
            }),
            DynResponder::Unmock => Ok(Evaluation::Skipped(inputs)),
        },
        Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
    }
}

pub(crate) fn eval_unsized_static_ref<'i, F: MockFn + 'static>(
    state: &SharedState,
    inputs: <F as MockInputs<'i>>::Inputs,
) -> MockResult<Evaluation<'i, &'static F::Output, F>> {
    match eval_responder::<F>(state, &inputs)? {
        Eval::Continue((pat_index, responder)) => match responder {
            DynResponder::Value(_) => Err(MockError::CannotBorrowValueStatically {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
            }),
            DynResponder::Closure(_) => Err(MockError::CannotBorrowValueProducedByClosure {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
            }),
            DynResponder::StaticRefClosure(inner) => {
                Ok(Evaluation::Evaluated(inner.downcast::<F>().0(inputs)))
            }
            DynResponder::Borrowable(_) => Err(MockError::CannotBorrowValueStatically {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
            }),
            DynResponder::Panic(msg) => Err(MockError::ExplicitPanic {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
                msg: msg.clone(),
            }),
            DynResponder::Unmock => Ok(Evaluation::Skipped(inputs)),
        },
        Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
    }
}

fn eval_responder<'u, 'i, F: MockFn + 'static>(
    state: &'u SharedState,
    inputs: &<F as MockInputs<'i>>::Inputs,
) -> MockResult<Eval<(PatIndex, &'u DynResponder)>> {
    match eval_mock_op(state, DynMockFn::new::<F>())? {
        Eval::Continue(dyn_mock_impl) => match match_pattern::<F>(state, dyn_mock_impl, inputs)? {
            Some((pat_index, pattern)) => match pattern.next_responder() {
                Some(responder) => Ok(Eval::Continue((pat_index, responder))),
                None => Err(MockError::NoOutputAvailableForCallPattern {
                    name: F::NAME,
                    inputs_debug: F::debug_inputs(inputs),
                    pat_index,
                }),
            },
            None => match state.fallback_mode {
                FallbackMode::Error => Err(MockError::NoMatchingCallPatterns {
                    name: F::NAME,
                    inputs_debug: F::debug_inputs(inputs),
                }),
                FallbackMode::Unmock => Ok(Eval::Unmock),
            },
        },
        Eval::Unmock => Ok(Eval::Unmock),
    }
}

#[inline(never)]
fn eval_mock_op<'u>(
    state: &'u SharedState,
    dyn_mock_fn: DynMockFn,
) -> MockResult<Eval<&'u DynMockImpl>> {
    match state.impls.get(&dyn_mock_fn.type_id) {
        None => match state.fallback_mode {
            FallbackMode::Error => Err(MockError::NoMockImplementation {
                name: dyn_mock_fn.name,
            }),
            FallbackMode::Unmock => Ok(Eval::Unmock),
        },
        Some(dyn_impl) => Ok(Eval::Continue(dyn_impl)),
    }
}

fn match_pattern<'u, 'i, F: MockFn>(
    state: &'u SharedState,
    mock_impl: &'u DynMockImpl,
    inputs: &<F as MockInputs<'i>>::Inputs,
) -> MockResult<Option<(PatIndex, &'u DynCallPattern)>> {
    match mock_impl.pattern_match_mode {
        PatternMatchMode::InOrder => {
            let MatchedInOrderCallPattern {
                pat_index,
                pattern,
                actual_call_order,
            } = try_select_in_order_call_pattern(state, mock_impl, &|| F::debug_inputs(inputs))?;

            if !pattern.match_inputs::<F>(inputs)? {
                return Err(MockError::InputsNotMatchedInCallOrder {
                    name: F::NAME,
                    inputs_debug: F::debug_inputs(inputs),
                    actual_call_order,
                    pat_index,
                });
            }

            Ok(Some((pat_index, pattern)))
        }
        PatternMatchMode::InAnyOrder => {
            for (index, pattern) in mock_impl.call_patterns.iter().enumerate() {
                if pattern.match_inputs::<F>(inputs)? {
                    return Ok(Some((PatIndex(index), pattern)));
                }
            }

            Ok(None)
        }
    }
}

struct MatchedInOrderCallPattern<'u> {
    pat_index: PatIndex,
    pattern: &'u DynCallPattern,
    actual_call_order: error::CallOrder,
}

fn try_select_in_order_call_pattern<'u>(
    state: &'u SharedState,
    mock_impl: &'u DynMockImpl,
    debug_inputs: &dyn Fn() -> String,
) -> MockResult<MatchedInOrderCallPattern<'u>> {
    // increase call index here, because stubs should not influence it:
    let global_call_index = state
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
            name: mock_impl.name,
            inputs_debug: debug_inputs(),
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

    Ok(MatchedInOrderCallPattern {
        pat_index: PatIndex(pat_index),
        pattern,
        actual_call_order: error::CallOrder(global_call_index),
    })
}
