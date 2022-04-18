use crate::*;

use std::any::Any;

/// The outcome of an api application/call on a mock object.
pub enum Outcome<'i, A: Api> {
    /// The mock evaluated the call and produced an output.
    Evaluated(A::Output),
    /// The mock only matched the inputs, and registered that fact.
    /// How to produce the output is left to the caller.
    Matched(A::Inputs<'i>),
}

pub(crate) trait Mock: Any {
    fn as_any(&self) -> &dyn Any;

    fn verify(&self, errors: &mut Vec<String>);
}

pub(crate) struct DynImpl(pub Box<dyn Mock + Send + Sync + 'static>);

pub(crate) fn apply<'i, A: Api + 'static>(
    dyn_impl: Option<&'i DynImpl>,
    inputs: A::Inputs<'i>,
    fallback_mode: FallbackMode,
) -> Result<Outcome<'i, A>, String> {
    match dyn_impl {
        None => match fallback_mode {
            FallbackMode::Error => Err(format!("No mock implementation found for {}", A::NAME)),
            FallbackMode::Fallthrough => Ok(Outcome::Matched(inputs)),
        },
        Some(dyn_impl) => {
            let mock_impl = dyn_impl.0.as_any().downcast_ref::<MockImpl<A>>().unwrap();

            mock_impl
                .has_applications
                .store(true, std::sync::atomic::Ordering::Relaxed);

            if mock_impl.patterns.is_empty() {
                return Err(format!(
                    "{}{}: No registered call patterns",
                    A::NAME,
                    mock_impl.debug_inputs(&inputs)
                ));
            }

            for pattern in mock_impl.patterns.iter() {
                if let Some(arg_matcher) = pattern.arg_matcher.as_ref() {
                    if !arg_matcher(&inputs) {
                        continue;
                    }
                }

                pattern.call_counter.tick();

                return match &pattern.responder {
                    Responder::Closure(closure) => Ok(Outcome::Evaluated(closure(inputs))),
                    Responder::Archetypal => Ok(Outcome::Matched(inputs)),
                    Responder::Error => Err(format!(
                        "{}{}: No output available for matching call pattern #{}",
                        A::NAME,
                        mock_impl.debug_inputs(&inputs),
                        pattern.pat_index,
                    )),
                };
            }

            Err(format!(
                "{}{}: No matching call patterns.",
                A::NAME,
                mock_impl.debug_inputs(&inputs)
            ))
        }
    }
}

#[doc(hidden)]
pub struct MockImpl<A: Api> {
    patterns: Vec<CallPattern<A>>,
    input_debugger: InputDebugger<A>,
    has_applications: AtomicBool,
}

impl<A: Api> MockImpl<A> {
    pub(crate) fn from_each(each: builders::Each<A>) -> Self {
        Self {
            patterns: each.patterns,
            input_debugger: each.input_debugger,
            has_applications: AtomicBool::new(false),
        }
    }

    fn debug_inputs<'i>(&self, inputs: &A::Inputs<'i>) -> String {
        self.input_debugger
            .debug_input_as_tuple(inputs, A::N_INPUTS)
    }
}

impl<A: Api + 'static> Mock for MockImpl<A> {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn verify(&self, errors: &mut Vec<String>) {
        for pattern in self.patterns.iter() {
            pattern
                .call_counter
                .verify(A::NAME, pattern.pat_index, errors);
        }

        if !self
            .has_applications
            .load(std::sync::atomic::Ordering::Relaxed)
        {
            errors.push(format!(
                "Mock for {} was never called. Dead mocks should be removed.",
                A::NAME
            ));
        }
    }
}

pub(crate) struct CallPattern<A: Api> {
    pub pat_index: usize,
    pub arg_matcher: Option<Box<dyn (for<'i> Fn(&A::Inputs<'i>) -> bool) + Send + Sync>>,
    pub call_counter: counter::CallCounter,
    pub responder: Responder<A>,
}

pub(crate) enum Responder<A: Api> {
    Closure(Box<dyn (for<'i> Fn(A::Inputs<'i>) -> A::Output) + Send + Sync>),
    Archetypal,
    Error,
}

pub(crate) struct InputDebugger<A: Api> {
    pub func: Option<Box<dyn (for<'i> Fn(&A::Inputs<'i>) -> String) + Send + Sync>>,
}

impl<A: Api> InputDebugger<A> {
    pub fn new() -> Self {
        Self { func: None }
    }

    pub fn debug_input_as_tuple<'i>(&self, inputs: &A::Inputs<'i>, n_args: u8) -> String {
        if let Some(func) = self.func.as_ref() {
            let debug = func(inputs);
            match n_args {
                1 => format!("({})", debug),
                _ => debug,
            }
        } else {
            anonymous_inputs_debug(n_args)
        }
    }
}

fn anonymous_inputs_debug(n_args: u8) -> String {
    let inner = (0..n_args)
        .into_iter()
        .map(|_| "_")
        .collect::<Vec<_>>()
        .join(", ");

    format!("({})", inner)
}
