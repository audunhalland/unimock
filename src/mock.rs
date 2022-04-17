use crate::*;

pub enum ApplyResult<'i, A: Api> {
    Evaluated(A::Output),
    Fallthrough(A::Inputs<'i>),
}

pub(crate) struct DynImpl(pub Box<dyn std::any::Any + Send + Sync + 'static>);

pub(crate) fn apply<'i, A: Api + 'static>(
    dyn_impl: Option<&'i DynImpl>,
    inputs: A::Inputs<'i>,
    fallback_mode: FallbackMode,
) -> ApplyResult<'i, A> {
    match dyn_impl {
        None => match fallback_mode {
            FallbackMode::Panic => panic!("No mock implementation found for {}", A::NAME),
            FallbackMode::Fallthrough => ApplyResult::Fallthrough(inputs),
        },
        Some(dyn_impl) => {
            let mock_impl = dyn_impl.0.downcast_ref::<MockImpl<A>>().unwrap();

            mock_impl
                .call_counter
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

            if mock_impl.patterns.is_empty() {
                return mock_impl.report_error(|| {
                    panic!(
                        "{}{}: No registered call patterns",
                        A::NAME,
                        mock_impl.debug_inputs(&inputs)
                    );
                });
            }

            for pattern in mock_impl.patterns.iter() {
                if let Some(arg_matcher) = pattern.arg_matcher.as_ref() {
                    if !arg_matcher(&inputs) {
                        continue;
                    }
                }

                pattern.call_counter.tick();

                match &pattern.responder {
                    Responder::Closure(closure) => return ApplyResult::Evaluated(closure(inputs)),
                    Responder::Fallthrough => return ApplyResult::Fallthrough(inputs),
                    Responder::Panic => {
                        panic!(
                            "{}{}: No output available for matching call pattern #{}",
                            A::NAME,
                            mock_impl.debug_inputs(&inputs),
                            pattern.pat_index,
                        );
                    }
                }
            }

            return mock_impl.report_error(|| {
                panic!(
                    "{}{}: No registered call patterns",
                    A::NAME,
                    mock_impl.debug_inputs(&inputs)
                );
            });
        }
    }
}

#[doc(hidden)]
pub struct MockImpl<A: Api> {
    patterns: Vec<CallPattern<A>>,
    input_debugger: InputDebugger<A>,
    call_counter: AtomicUsize,
    error_counter: AtomicUsize,
}

impl<A: Api> MockImpl<A> {
    pub(crate) fn from_each(each: builders::Each<A>) -> Self {
        Self {
            patterns: each.patterns,
            input_debugger: each.input_debugger,
            call_counter: AtomicUsize::new(0),
            error_counter: AtomicUsize::new(0),
        }
    }

    pub(crate) fn empty_with_forwarding() -> Self {
        Self {
            patterns: vec![],
            input_debugger: InputDebugger { func: None },
            call_counter: AtomicUsize::new(0),
            error_counter: AtomicUsize::new(0),
        }
    }

    fn report_error<'i, F>(&self, panic_producer: F) -> ApplyResult<'i, A>
    where
        F: FnOnce() -> ApplyResult<'i, A>,
    {
        self.error_counter
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        panic_producer()
    }

    fn debug_inputs<'i>(&self, inputs: &A::Inputs<'i>) -> String {
        self.input_debugger
            .debug_input_as_tuple(inputs, A::N_INPUTS)
    }
}

impl<A: Api> Drop for MockImpl<A> {
    fn drop(&mut self) {
        let call_count = self.call_counter.load(std::sync::atomic::Ordering::Relaxed);
        if call_count == 0 {
            panic!(
                "Mock for {} was never called. Dead mocks should be removed.",
                A::NAME
            );
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
    Fallthrough,
    Panic,
}

impl<A: Api> Drop for CallPattern<A> {
    fn drop(&mut self) {
        self.call_counter.verify(A::NAME, self.pat_index);
    }
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
