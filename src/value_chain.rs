use std::any::Any;

use lazycell::AtomicLazyCell;

/// A collection that can store values, but never remove them until dropped.
///
/// This allows retaining shared references to inserted values.
#[derive(Default)]
pub struct ValueChain {
    root_node: AtomicLazyCell<Node>,
}

impl ValueChain {
    pub fn add<T: Any + Send + Sync>(&self, value: T) -> &T {
        let node = self.push_node(Node::new(value));

        node.value.as_ref().downcast_ref::<T>().unwrap()
    }

    fn push_node(&self, new_node: Node) -> &Node {
        if let Err(mut new_node) = self.root_node.fill(new_node) {
            let mut parent_node = self.root_node.borrow().unwrap();

            while let Err(node) = parent_node.next.fill(new_node) {
                parent_node = parent_node.next.borrow().unwrap();
                new_node = node;
            }

            parent_node.next.borrow().unwrap()
        } else {
            self.root_node.borrow().unwrap()
        }
    }
}

struct Node {
    value: Box<dyn Any + Send + Sync>,
    next: Box<AtomicLazyCell<Node>>,
}

impl Node {
    pub fn new<T: Any + Send + Sync>(value: T) -> Self {
        Self {
            value: Box::new(value),
            next: Default::default(),
        }
    }
}

#[test]
fn it_works() {
    let value_chain = ValueChain::default();
    let first = value_chain.add(1);
    let second = value_chain.add("");
    let third = value_chain.add(42.0);

    assert_eq!(&1, first);
    assert_eq!(&"", second);
    assert_eq!(&42.0, third);
}
