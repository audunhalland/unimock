use crate::call_pattern::DynCallPattern;
use crate::call_pattern::Responder;
use crate::error;
use crate::error::MockError;
use crate::macro_api::Evaluation;
use crate::mock_impl::{DynMockImpl, PatternMatchMode};
use crate::{FallbackMode, MockFn, MockInputs};

use std::borrow::Borrow;
use std::sync::atomic::AtomicUsize;

enum Eval<C> {
    Continue(C),
    Unmock,
}

pub(crate) fn eval_sized<'i, F: MockFn + 'static>(
    dyn_impl: Option<&DynMockImpl>,
    inputs: <F as MockInputs<'i>>::Inputs,
    call_index: &AtomicUsize,
    fallback_mode: FallbackMode,
) -> Result<Evaluation<'i, F::Output, F>, MockError>
where
    F::Output: Sized,
{
    match eval_responder(dyn_impl, &inputs, call_index, fallback_mode)? {
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
    dyn_impl: Option<&'u DynMockImpl>,
    inputs: <F as MockInputs<'i>>::Inputs,
    call_index: &AtomicUsize,
    fallback_mode: FallbackMode,
) -> Result<Evaluation<'i, &'u F::Output, F>, MockError> {
    match eval_responder::<F>(dyn_impl, &inputs, call_index, fallback_mode)? {
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
    dyn_impl: Option<&DynMockImpl>,
    inputs: <F as MockInputs<'i>>::Inputs,
    call_index: &AtomicUsize,
    fallback_mode: FallbackMode,
) -> Result<Evaluation<'i, &'static F::Output, F>, MockError> {
    match eval_responder::<F>(dyn_impl, &inputs, call_index, fallback_mode)? {
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
    dyn_impl: Option<&'u DynMockImpl>,
    inputs: &<F as MockInputs<'i>>::Inputs,
    call_index: &AtomicUsize,
    fallback_mode: FallbackMode,
) -> Result<Eval<(usize, &'u Responder<F>)>, MockError> {
    match eval_mock_op(dyn_impl, F::NAME, fallback_mode)? {
        Eval::Continue(dyn_mock_impl) => {
            match match_pattern::<F>(dyn_mock_impl, inputs, call_index)? {
                Some((pat_index, pattern)) => match select_responder_for_call(pattern)? {
                    Some(responder) => Ok(Eval::Continue((pat_index, responder))),
                    None => Err(MockError::NoOutputAvailableForCallPattern {
                        name: F::NAME,
                        inputs_debug: F::debug_inputs(inputs),
                        pat_index,
                    }),
                },
                None => match fallback_mode {
                    FallbackMode::Error => Err(MockError::NoMatchingCallPatterns {
                        name: F::NAME,
                        inputs_debug: F::debug_inputs(inputs),
                    }),
                    FallbackMode::Unmock => Ok(Eval::Unmock),
                },
            }
        }
        Eval::Unmock => Ok(Eval::Unmock),
    }
}

#[inline(never)]
fn eval_mock_op<'u>(
    dyn_impl: Option<&'u DynMockImpl>,
    name: &'static str,
    fallback_mode: FallbackMode,
) -> Result<Eval<&'u DynMockImpl>, MockError> {
    match dyn_impl {
        None => match fallback_mode {
            FallbackMode::Error => Err(MockError::NoMockImplementation { name }),
            FallbackMode::Unmock => Ok(Eval::Unmock),
        },
        Some(dyn_impl) => Ok(Eval::Continue(dyn_impl)),
    }
}

fn match_pattern<'u, 'i, F: MockFn>(
    mock_impl: &'u DynMockImpl,
    inputs: &<F as MockInputs<'i>>::Inputs,
    global_call_index: &AtomicUsize,
) -> Result<Option<(usize, &'u DynCallPattern)>, MockError> {
    match mock_impl.pattern_match_mode {
        PatternMatchMode::InOrder => {
            let MatchedInOrderCallPattern {
                pat_index,
                pattern,
                global_call_index,
            } = try_select_in_order_call_pattern(mock_impl, global_call_index, &|| {
                F::debug_inputs(inputs)
            })?;

            if !pattern.match_inputs::<F>(mock_impl.name, inputs)? {
                return Err(MockError::InputsNotMatchedInCallOrder {
                    name: F::NAME,
                    inputs_debug: F::debug_inputs(inputs),
                    actual_call_order: error::CallOrder(global_call_index),
                    pat_index,
                });
            }

            Ok(Some((pat_index, pattern)))
        }
        PatternMatchMode::InAnyOrder => {
            for (index, pattern) in mock_impl.call_patterns.iter().enumerate() {
                if pattern.match_inputs::<F>(F::NAME, inputs)? {
                    return Ok(Some((index, pattern)));
                }
            }

            Ok(None)
        }
    }
}

struct MatchedInOrderCallPattern<'u> {
    pat_index: usize,
    pattern: &'u DynCallPattern,
    global_call_index: usize,
}

fn try_select_in_order_call_pattern<'u>(
    mock_impl: &'u DynMockImpl,
    global_call_index: &AtomicUsize,
    debug_inputs: &dyn Fn() -> String,
) -> Result<MatchedInOrderCallPattern<'u>, MockError> {
    // increase call index here, because stubs should not influence it:
    let global_call_index = global_call_index.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

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
        pat_index,
        pattern,
        global_call_index,
    })
}

fn select_responder_for_call<F: MockFn>(
    pat: &DynCallPattern,
) -> Result<Option<&Responder<F>>, MockError> {
    let call_index = pat.call_counter.fetch_add();
    let match_and_respond = pat.downcast_match_and_respond::<F>(F::NAME)?;

    let mut responder = None;

    for call_index_responder in match_and_respond.responders.iter() {
        if call_index_responder.response_index > call_index {
            break;
        }

        responder = Some(&call_index_responder.responder)
    }

    Ok(responder)
}
