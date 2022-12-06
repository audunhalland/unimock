use once_cell::sync::OnceCell;

pub(crate) trait Cell<T: ?Sized + 'static>: Send + Sync {
    fn try_take(&self) -> Option<Box<T>>;

    fn borrow(&self) -> &T;
}

pub(crate) struct CloneCell<T>(pub T);

impl<T: Clone + Send + Sync + 'static> Cell<T> for CloneCell<T> {
    fn try_take(&self) -> Option<Box<T>> {
        Some(Box::new(self.0.clone()))
    }

    fn borrow(&self) -> &T {
        &self.0
    }
}

pub(crate) struct FactoryCell<T> {
    factory: Box<dyn Fn() -> Option<T> + Send + Sync + 'static>,
    borrowed_value: OnceCell<T>,
}

impl<T> FactoryCell<T> {
    pub fn new(factory: impl Fn() -> Option<T> + Send + Sync + 'static) -> Self {
        Self {
            factory: Box::new(factory),
            borrowed_value: OnceCell::new(),
        }
    }
}

impl<T: Send + Sync + 'static> Cell<T> for FactoryCell<T> {
    fn try_take(&self) -> Option<Box<T>> {
        (*self.factory)().map(|value| Box::new(value))
    }

    fn borrow(&self) -> &T {
        if let Some(value) = self.borrowed_value.get() {
            return value;
        }

        {
            if let Some(value) = (*self.factory)() {
                if self.borrowed_value.set(value).is_err() {
                    panic!("Tried to set borrowed value twice");
                }
            }
        }

        self.borrowed_value
            .get()
            .expect("Tried to borrow a value that has already been taken")
    }
}
