use crate::private::lib::Box;

pub(crate) trait Cell<T: ?Sized + 'static>: Send + Sync {
    fn try_take(&self) -> Option<Box<T>>;
}

pub(crate) struct CloneCell<T>(pub T);

impl<T: Clone + Send + Sync + 'static> Cell<T> for CloneCell<T> {
    fn try_take(&self) -> Option<Box<T>> {
        Some(Box::new(self.0.clone()))
    }
}

pub(crate) struct FactoryCell<T> {
    factory: Box<dyn Fn() -> Option<T> + Send + Sync + 'static>,
}

impl<T> FactoryCell<T> {
    pub fn new(factory: impl Fn() -> Option<T> + Send + Sync + 'static) -> Self {
        Self {
            factory: Box::new(factory),
        }
    }
}

impl<T: Send + Sync + 'static> Cell<T> for FactoryCell<T> {
    fn try_take(&self) -> Option<Box<T>> {
        (*self.factory)().map(|value| Box::new(value))
    }
}
