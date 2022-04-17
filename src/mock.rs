use crate::*;

#[doc(hidden)]
pub enum Impl<'s, A: Api + 'static> {
    Mock(&'s MockImpl<A>),
    CallOriginal,
}

impl<'s, A: Api + 'static> Impl<'s, A> {
    pub(crate) fn from_storage(storage: &'s DynImpl) -> Self {
        match storage {
            DynImpl::CallOriginal => Self::CallOriginal,
            DynImpl::Mock(any) => {
                let mock_impl = any.downcast_ref::<MockImpl<A>>().unwrap();
                Self::Mock(&mock_impl)
            }
        }
    }

    pub(crate) fn from_fallback(fallback_mode: &FallbackMode) -> Self {
        match fallback_mode {
            FallbackMode::CallOriginal => Self::CallOriginal,
            FallbackMode::Panic => panic!("No mock implementation found for {}", A::NAME),
        }
    }
}

pub(crate) enum DynImpl {
    Mock(Box<dyn std::any::Any + Send + Sync + 'static>),
    CallOriginal,
}

#[doc(hidden)]
pub struct MockImpl<A: Api> {
    patterns: Vec<CallPattern<A>>,
    input_debugger: InputDebugger<A>,
    call_counter: AtomicUsize,
}

impl<A: Api> MockImpl<A> {
    pub(crate) fn from_each(each: builders::Each<A>) -> Self {
        Self {
            patterns: each.patterns,
            input_debugger: each.input_debugger,
            call_counter: AtomicUsize::new(0),
        }
    }

    pub fn invoke<'i>(&'i self, inputs: A::Inputs<'i>) -> A::Output {
        self.call_counter
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        if self.patterns.is_empty() {
            panic!(
                "{}{}: No registered call patterns",
                A::NAME,
                self.debug_inputs(&inputs)
            );
        }

        for pattern in self.patterns.iter() {
            if let Some(arg_matcher) = pattern.arg_matcher.as_ref() {
                if !arg_matcher(&inputs) {
                    continue;
                }
            }

            pattern.call_counter.tick();

            if let Some(output_factory) = pattern.output_factory.as_ref() {
                return output_factory(inputs);
            } else {
                panic!(
                    "{}{}: No output available for matching call pattern #{}",
                    A::NAME,
                    self.debug_inputs(&inputs),
                    pattern.pat_index,
                );
            }
        }

        panic!(
            "{}{}: No matching call patterns.",
            A::NAME,
            self.debug_inputs(&inputs)
        );
    }

    fn debug_inputs<'i>(&self, inputs: &A::Inputs<'i>) -> String {
        self.input_debugger.debug(inputs, A::N_INPUTS)
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
    pub output_factory: Option<Box<dyn (for<'i> Fn(A::Inputs<'i>) -> A::Output) + Send + Sync>>,
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

    pub fn debug<'i>(&self, inputs: &A::Inputs<'i>, n_args: u8) -> String {
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
