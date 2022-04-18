use crate::*;

use std::any::Any;

/// The outcome of an api application/call on a mock object.
pub enum Outcome<'i, F: VirtualFn> {
    /// The mock evaluated the call and produced an output.
    Evaluated(F::Output),
    /// The mock only matched the inputs, and registered that fact.
    /// How to produce the output is left to the caller.
    InvokeOriginal(F::Inputs<'i>),
}

pub(crate) trait Mock: Any {
    fn as_any(&self) -> &dyn Any;

    fn verify(&self, errors: &mut Vec<String>);
}

pub(crate) struct DynImpl(pub Box<dyn Mock + Send + Sync + 'static>);

pub(crate) fn apply<'i, F: VirtualFn + 'static>(
    dyn_impl: Option<&'i DynImpl>,
    inputs: F::Inputs<'i>,
    fallback_mode: FallbackMode,
) -> Result<Outcome<'i, F>, String> {
    match dyn_impl {
        None => match fallback_mode {
            FallbackMode::Error => Err(format!("No mock implementation found for {}", F::NAME)),
            FallbackMode::InvokeOriginal => Ok(Outcome::InvokeOriginal(inputs)),
        },
        Some(dyn_impl) => {
            let mock_impl = dyn_impl.0.as_any().downcast_ref::<MockImpl<F>>().unwrap();

            mock_impl
                .has_applications
                .store(true, std::sync::atomic::Ordering::Relaxed);

            if mock_impl.patterns.is_empty() {
                return Err(format!(
                    "{}{}: No registered call patterns",
                    F::NAME,
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
                    Responder::InvokeOriginal => Ok(Outcome::InvokeOriginal(inputs)),
                    Responder::Error => Err(format!(
                        "{}{}: No output available for matching call pattern #{}",
                        F::NAME,
                        mock_impl.debug_inputs(&inputs),
                        pattern.pat_index,
                    )),
                };
            }

            Err(format!(
                "{}{}: No matching call patterns.",
                F::NAME,
                mock_impl.debug_inputs(&inputs)
            ))
        }
    }
}

pub(crate) struct MockImpl<F: VirtualFn> {
    patterns: Vec<CallPattern<F>>,
    input_debugger: InputDebugger<F>,
    has_applications: AtomicBool,
}

impl<F: VirtualFn> MockImpl<F> {
    pub(crate) fn from_each(each: builders::Each<F>) -> Self {
        Self {
            patterns: each.patterns,
            input_debugger: each.input_debugger,
            has_applications: AtomicBool::new(false),
        }
    }

    fn debug_inputs<'i>(&self, inputs: &F::Inputs<'i>) -> String {
        self.input_debugger
            .debug_input_as_tuple(inputs, F::N_INPUTS)
    }
}

impl<F: VirtualFn + 'static> Mock for MockImpl<F> {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn verify(&self, errors: &mut Vec<String>) {
        for pattern in self.patterns.iter() {
            pattern
                .call_counter
                .verify(F::NAME, pattern.pat_index, errors);
        }

        if !self
            .has_applications
            .load(std::sync::atomic::Ordering::Relaxed)
        {
            errors.push(format!(
                "Mock for {} was never called. Dead mocks should be removed.",
                F::NAME
            ));
        }
    }
}

pub(crate) struct CallPattern<F: VirtualFn> {
    pub pat_index: usize,
    pub arg_matcher: Option<Box<dyn (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync>>,
    pub call_counter: counter::CallCounter,
    pub responder: Responder<F>,
}

pub(crate) enum Responder<F: VirtualFn> {
    Closure(Box<dyn (for<'i> Fn(F::Inputs<'i>) -> F::Output) + Send + Sync>),
    InvokeOriginal,
    Error,
}

pub(crate) struct InputDebugger<F: VirtualFn> {
    pub func: Option<Box<dyn (for<'i> Fn(&F::Inputs<'i>) -> String) + Send + Sync>>,
}

impl<F: VirtualFn> InputDebugger<F> {
    pub fn new() -> Self {
        Self { func: None }
    }

    pub fn debug_input_as_tuple<'i>(&self, inputs: &F::Inputs<'i>, n_args: u8) -> String {
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
