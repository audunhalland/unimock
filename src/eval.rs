use core::sync::atomic::Ordering;

use crate::alloc::{Box, String};
use crate::call_pattern::{CallPattern, PatIndex, PatternError, PatternResult};
use crate::error::{self};
use crate::error::{MockError, MockResult};
use crate::fn_mocker::{FnMocker, PatternMatchMode};
use crate::mismatch::Mismatches;
use crate::private::{Continuation, Eval, MismatchReporter};
use crate::responder::{DowncastResponder, DynResponder};
use crate::state::SharedState;
use crate::{debug, MockFnInfo, Unimock};
use crate::{FallbackMode, MockFn};

enum EvalResult<'u> {
    Responder(EvalResponder<'u>),
    Unmock,
    CallDefaultImpl,
}

struct EvalResponder<'u> {
    fn_mocker: &'u FnMocker,
    pat_index: PatIndex,
    dyn_responder: &'u DynResponder,
}

pub(crate) fn eval<'u, 'i, F: MockFn>(
    unimock: &'u Unimock,
    inputs: F::Inputs<'i>,
) -> MockResult<Eval<'u, 'i, F>> {
    let dyn_ctx = DynCtx {
        info: F::info(),
        shared_state: &unimock.shared_state,
        input_debugger: &|| F::debug_inputs(&inputs),
    };

    match dyn_ctx.eval_dyn(&|pattern, reporter| pattern.match_inputs::<F>(&inputs, reporter))? {
        EvalResult::Responder(eval_responder) => match eval_responder.dyn_responder {
            DynResponder::Return(dyn_return_responder) => {
                match dyn_ctx
                    .downcast_responder::<F, _>(dyn_return_responder, &eval_responder)?
                    .get_output()
                {
                    Some(output) => Ok(Eval::Return(output)),
                    None => Err(MockError::CannotReturnValueMoreThanOnce {
                        fn_call: dyn_ctx.fn_call(),
                        pattern: eval_responder
                            .fn_mocker
                            .debug_pattern(eval_responder.pat_index),
                    }),
                }
            }
            DynResponder::Answer(dyn_responder) => {
                let answerer =
                    dyn_ctx.downcast_responder::<F, _>(dyn_responder, &eval_responder)?;
                Ok(Eval::Continue(
                    Continuation::Answer(answerer.answer_closure.clone()),
                    inputs,
                ))
            }
            DynResponder::Panic(msg) => Err(MockError::ExplicitPanic {
                fn_call: dyn_ctx.fn_call(),
                pattern: eval_responder
                    .fn_mocker
                    .debug_pattern(eval_responder.pat_index),
                msg: msg.clone(),
            }),
            DynResponder::Unmock => Ok(Eval::Continue(Continuation::Unmock, inputs)),
            DynResponder::ApplyDefaultImpl => {
                Ok(Eval::Continue(Continuation::CallDefaultImpl, inputs))
            }
        },
        EvalResult::Unmock => Ok(Eval::Continue(Continuation::Unmock, inputs)),
        EvalResult::CallDefaultImpl => Ok(Eval::Continue(Continuation::CallDefaultImpl, inputs)),
    }
}

/// 'u = unimock instance, 's = stack
struct DynCtx<'u, 's> {
    info: MockFnInfo,
    shared_state: &'u SharedState,
    input_debugger: &'s dyn Fn() -> Box<[Option<String>]>,
}

impl<'u> DynCtx<'u, '_> {
    #[inline(never)]
    fn eval_dyn(
        &self,
        match_inputs: &dyn Fn(&CallPattern, Option<&mut MismatchReporter>) -> PatternResult<bool>,
    ) -> MockResult<EvalResult<'u>> {
        self.check_preconditions()?;

        let fn_mocker = match self.shared_state.fn_mockers.get(&self.info.type_id) {
            None => {
                return if self.info.has_default_impl {
                    Ok(EvalResult::CallDefaultImpl)
                } else if self.info.partial_by_default {
                    Ok(EvalResult::Unmock)
                } else {
                    match self.shared_state.fallback_mode {
                        FallbackMode::Error => Err(MockError::NoMockImplementation {
                            fn_call: self.fn_call(),
                        }),
                        FallbackMode::Unmock => Ok(EvalResult::Unmock),
                    }
                }
            }
            Some(fn_mocker) => fn_mocker,
        };

        match self.match_call_pattern(fn_mocker, match_inputs)? {
            Some((pat_index, pattern)) => match pattern.next_responder() {
                Some(dyn_responder) => Ok(EvalResult::Responder(EvalResponder {
                    fn_mocker,
                    pat_index,
                    dyn_responder,
                })),
                None => Err(MockError::NoOutputAvailableForCallPattern {
                    fn_call: self.fn_call(),
                    pattern: fn_mocker.debug_pattern(pat_index),
                }),
            },
            None => match self.shared_state.fallback_mode {
                FallbackMode::Error => {
                    let mut builder = Mismatches::builder();
                    for (pat_index, call_pattern) in fn_mocker.call_patterns.iter().enumerate() {
                        let mut mismatch_reporter = MismatchReporter::new_enabled();
                        let _ = match_inputs(call_pattern, Some(&mut mismatch_reporter));
                        builder.collect_from_reporter(PatIndex(pat_index), mismatch_reporter);
                    }

                    Err(MockError::NoMatchingCallPatterns {
                        fn_call: self.fn_call(),
                        mismatches: builder.build(),
                    })
                }
                FallbackMode::Unmock => Ok(EvalResult::Unmock),
            },
        }
    }

    fn match_call_pattern(
        &self,
        fn_mocker: &'u FnMocker,
        match_inputs: &dyn Fn(&CallPattern, Option<&mut MismatchReporter>) -> PatternResult<bool>,
    ) -> MockResult<Option<(PatIndex, &'u CallPattern)>> {
        match fn_mocker.pattern_match_mode {
            PatternMatchMode::InAnyOrder => fn_mocker
                .call_patterns
                .iter()
                .enumerate()
                .filter_map(
                    |(pat_index, call_pattern)| match match_inputs(call_pattern, None) {
                        Ok(false) => None,
                        Ok(true) => Some(Ok((PatIndex(pat_index), call_pattern))),
                        Err(err) => Some(Err((PatIndex(pat_index), err))),
                    },
                )
                .next()
                .transpose()
                .map_err(|(pat_index, err)| self.map_pattern_error(err, fn_mocker, pat_index)),
            PatternMatchMode::InOrder => {
                let ordered_call_index = self.shared_state.bump_ordered_call_index();

                let (pat_index, pattern) = fn_mocker
                    .find_call_pattern_for_call_order(ordered_call_index)
                    .ok_or_else(|| MockError::CallOrderNotMatchedForMockFn {
                        fn_call: self.fn_call(),
                        actual_call_order: error::CallOrder(ordered_call_index),
                        expected: self
                            .shared_state
                            .find_ordered_expected_call_pattern_debug(ordered_call_index),
                    })?;

                let mut mismatch_reporter = MismatchReporter::new_enabled();

                if !match_inputs(pattern, Some(&mut mismatch_reporter))
                    .map_err(|err| self.map_pattern_error(err, fn_mocker, pat_index))?
                {
                    let mut builder = Mismatches::builder();
                    builder.collect_from_reporter(pat_index, mismatch_reporter);

                    return Err(MockError::InputsNotMatchedInCallOrder {
                        fn_call: self.fn_call(),
                        actual_call_order: error::CallOrder(ordered_call_index),
                        pattern: fn_mocker.debug_pattern(pat_index),
                        mismatches: builder.build(),
                    });
                }

                Ok(Some((pat_index, pattern)))
            }
        }
    }

    #[inline]
    fn downcast_responder<F: MockFn, D>(
        &self,
        dyn_responder: &'u D,
        eval_responder: &EvalResponder<'u>,
    ) -> MockResult<&'u D::Downcasted>
    where
        D: DowncastResponder<F>,
    {
        dyn_responder.downcast().map_err(|err| {
            self.map_pattern_error(err, eval_responder.fn_mocker, eval_responder.pat_index)
        })
    }

    #[inline(never)]
    fn map_pattern_error(
        &self,
        pattern_error: PatternError,
        fn_mocker: &FnMocker,
        pat_index: PatIndex,
    ) -> MockError {
        match pattern_error {
            PatternError::Downcast => MockError::Downcast {
                fn_call: self.fn_call(),
                pattern: fn_mocker.debug_pattern(pat_index),
            },
            PatternError::NoMatcherFunction => MockError::NoMatcherFunction {
                fn_call: self.fn_call(),
                pattern: fn_mocker.debug_pattern(pat_index),
            },
        }
    }

    fn fn_call(&self) -> debug::FnActualCall {
        debug::FnActualCall {
            info: self.info,
            inputs_debug: self.debug_inputs(),
        }
    }

    fn debug_inputs(&self) -> Box<[Option<String>]> {
        (self.input_debugger)()
    }

    fn check_preconditions(&self) -> MockResult<()> {
        if self
            .shared_state
            .verification_started
            .load(Ordering::Relaxed)
        {
            return Err(MockError::CallToEscapedClone {
                fn_call: self.fn_call(),
            });
        }

        Ok(())
    }
}
