use crate::call_pattern::{DynCallPattern, PatIndex, Responder};
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

pub(crate) fn eval_sized<'i, F: MockFn + 'static>(
    state: &SharedState,
    inputs: <F as MockInputs<'i>>::Inputs,
) -> MockResult<Evaluation<'i, F::Output, F>>
where
    F::Output: Sized,
{
    match eval_responder(state, &inputs)? {
        Eval::Continue((pat_index, responder)) => match responder {
            Responder::Value(stored) => Ok(Evaluation::Evaluated(*stored.box_clone())),
            Responder::Closure(closure) => Ok(Evaluation::Evaluated(closure(inputs))),
            Responder::StaticRefClosure(_) | Responder::Borrowable(_) => {
                Err(MockError::TypeMismatchExpectedOwnedInsteadOfBorrowed {
                    name: F::NAME,
                    inputs_debug: F::debug_inputs(&inputs),
                    pat_index,
                })
            }
            Responder::Panic(msg) => Err(MockError::ExplicitPanic {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
                msg: msg.clone(),
            }),
            Responder::Unmock => Ok(Evaluation::Skipped(inputs)),
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
            Responder::Value(stored) => Ok(Evaluation::Evaluated(stored.borrow_stored())),
            Responder::Closure(_) => Err(MockError::CannotBorrowValueProducedByClosure {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
            }),
            Responder::StaticRefClosure(closure) => Ok(Evaluation::Evaluated(closure(inputs))),
            Responder::Borrowable(borrowable) => {
                let borrowable: &dyn Borrow<<F as MockFn>::Output> = borrowable.as_ref();
                let borrow = borrowable.borrow();
                Ok(Evaluation::Evaluated(borrow))
            }
            Responder::Panic(msg) => Err(MockError::ExplicitPanic {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
                msg: msg.clone(),
            }),
            Responder::Unmock => Ok(Evaluation::Skipped(inputs)),
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
            Responder::Value(_) => Err(MockError::CannotBorrowValueStatically {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
            }),
            Responder::Closure(_) => Err(MockError::CannotBorrowValueProducedByClosure {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
            }),
            Responder::StaticRefClosure(closure) => Ok(Evaluation::Evaluated(closure(inputs))),
            Responder::Borrowable(_) => Err(MockError::CannotBorrowValueStatically {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
            }),
            Responder::Panic(msg) => Err(MockError::ExplicitPanic {
                name: F::NAME,
                inputs_debug: F::debug_inputs(&inputs),
                pat_index,
                msg: msg.clone(),
            }),
            Responder::Unmock => Ok(Evaluation::Skipped(inputs)),
        },
        Eval::Unmock => Ok(Evaluation::Skipped(inputs)),
    }
}

fn eval_responder<'u, 'i, F: MockFn + 'static>(
    state: &'u SharedState,
    inputs: &<F as MockInputs<'i>>::Inputs,
) -> MockResult<Eval<(PatIndex, &'u Responder<F>)>> {
    match eval_mock_op(state, DynMockFn::new::<F>())? {
        Eval::Continue(dyn_mock_impl) => match match_pattern::<F>(state, dyn_mock_impl, inputs)? {
            Some((pat_index, pattern)) => match select_responder_for_call(pattern)? {
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

fn select_responder_for_call<F: MockFn>(pat: &DynCallPattern) -> MockResult<Option<&Responder<F>>> {
    let call_index = pat.call_counter.fetch_add();
    let responders = pat.responders::<F>()?;

    let mut responder = None;

    for call_index_responder in responders.iter() {
        if call_index_responder.response_index > call_index {
            break;
        }

        responder = Some(&call_index_responder.responder)
    }

    Ok(responder)
}
