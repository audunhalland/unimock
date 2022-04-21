use crate::error::MockError;
use crate::*;

use std::any::{Any, TypeId};
use std::collections::HashMap;

pub(crate) struct DynImpl(pub Box<dyn TypeErasedMockImpl + Send + Sync + 'static>);

pub(crate) trait TypeErasedMockImpl: Any {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;

    fn assemble_into(&mut self, target: &mut HashMap<TypeId, DynImpl>);

    fn verify(&self, errors: &mut Vec<MockError>);
}

pub(crate) fn apply<'i, F: MockFn + 'static>(
    dyn_impl: Option<&'i DynImpl>,
    inputs: F::Inputs<'i>,
    fallback_mode: FallbackMode,
) -> Result<Outcome<'i, F>, MockError> {
    match dyn_impl {
        None => match fallback_mode {
            FallbackMode::Error => Err(MockError::NoMockImplementation { name: F::NAME }),
            FallbackMode::Unmock => Ok(Outcome::Unmock(inputs)),
        },
        Some(dyn_impl) => {
            let mock_impl = dyn_impl
                .0
                .as_any()
                .downcast_ref::<TypedMockImpl<F>>()
                .ok_or_else(|| MockError::Downcast { name: F::NAME })?;

            mock_impl
                .has_applications
                .store(true, std::sync::atomic::Ordering::Relaxed);

            if mock_impl.patterns.is_empty() {
                return Err(MockError::NoRegisteredCallPatterns {
                    name: F::NAME,
                    inputs_debug: mock_impl.debug_inputs(&inputs),
                });
            }

            for (pat_index, pattern) in mock_impl.patterns.iter().enumerate() {
                if !(*pattern.input_matcher)(&inputs) {
                    continue;
                }

                pattern.call_counter.tick();

                return match &pattern.responder {
                    Responder::Closure(closure) => Ok(Outcome::Evaluated(closure(inputs))),
                    Responder::Unmock => Ok(Outcome::Unmock(inputs)),
                    Responder::Error => Err(MockError::NoOutputAvailableForCallPattern {
                        name: F::NAME,
                        inputs_debug: mock_impl.debug_inputs(&inputs),
                        pat_index,
                    }),
                };
            }

            Err(MockError::NoMatchingCallPatterns {
                name: F::NAME,
                inputs_debug: mock_impl.debug_inputs(&inputs),
            })
        }
    }
}

pub(crate) struct TypedMockImpl<F: MockFn> {
    pub input_debugger: InputDebugger<F>,
    pub patterns: Vec<CallPattern<F>>,
    pub has_applications: AtomicBool,
}

impl<F: MockFn> TypedMockImpl<F> {
    pub fn with_input_debugger(input_debugger: InputDebugger<F>) -> Self {
        Self {
            input_debugger,
            patterns: vec![],
            has_applications: AtomicBool::new(false),
        }
    }

    fn debug_inputs<'i>(&self, inputs: &F::Inputs<'i>) -> String {
        self.input_debugger
            .debug_input_as_tuple(inputs, F::N_INPUTS)
    }
}

impl<F: MockFn + 'static> TypeErasedMockImpl for TypedMockImpl<F> {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn assemble_into(&mut self, target: &mut HashMap<TypeId, DynImpl>) {
        let stored_dyn = target.entry(TypeId::of::<F>()).or_insert_with(|| {
            DynImpl(Box::new(Self::with_input_debugger(
                InputDebugger::new_nodebug(),
            )))
        });
        let stored_typed = stored_dyn
            .0
            .as_any_mut()
            .downcast_mut::<TypedMockImpl<F>>()
            .unwrap();

        stored_typed.patterns.append(&mut self.patterns);
        stored_typed
            .input_debugger
            .steal_debug_if_necessary(&mut self.input_debugger);
    }

    fn verify(&self, errors: &mut Vec<MockError>) {
        for (pat_index, pattern) in self.patterns.iter().enumerate() {
            pattern.call_counter.verify(F::NAME, pat_index, errors);
        }

        if !self
            .has_applications
            .load(std::sync::atomic::Ordering::Relaxed)
        {
            errors.push(MockError::MockNeverCalled { name: F::NAME });
        }
    }
}

pub(crate) struct CallPattern<F: MockFn> {
    pub input_matcher: Box<dyn (for<'i> Fn(&F::Inputs<'i>) -> bool) + Send + Sync>,
    pub call_counter: counter::CallCounter,
    pub responder: Responder<F>,
}

pub(crate) enum Responder<F: MockFn> {
    Closure(Box<dyn (for<'i> Fn(F::Inputs<'i>) -> F::Output) + Send + Sync>),
    Unmock,
    Error,
}

pub(crate) struct InputDebugger<F: MockFn> {
    pub debug_func: Option<Box<dyn (for<'i> Fn(&F::Inputs<'i>) -> String) + Send + Sync>>,
}

impl<F: MockFn> InputDebugger<F> {
    pub fn new_nodebug() -> Self {
        Self { debug_func: None }
    }

    pub fn new_debug() -> Self
    where
        for<'i> F::Inputs<'i>: std::fmt::Debug,
    {
        Self {
            debug_func: Some(Box::new(|args| format!("{:?}", args))),
        }
    }

    pub fn steal_debug_if_necessary(&mut self, other: &mut InputDebugger<F>) {
        if self.debug_func.is_none() {
            self.debug_func = other.debug_func.take()
        }
    }

    pub fn debug_input_as_tuple<'i>(&self, inputs: &F::Inputs<'i>, n_args: u8) -> String {
        if let Some(func) = self.debug_func.as_ref() {
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
