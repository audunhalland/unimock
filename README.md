# unimock


`unimock` is a library for defining _mock implementations_ of traits.

Mocking, in a broad sense, is a way to control API behaviour during test execution.

The _uni_ in unimock indicates one-ness: All mockable traits are implemented by a single type, [Unimock].
This design allows for a great flexibility in coding style, as will be demonstrated further down.

The first code example is the smallest possible use of unimock:

```rust
use unimock::*;

#[unimock]
trait Foo {}

fn takes_foo(foo: impl Foo) {}

takes_foo(mock(None));
```

1. `trait Foo` is declared with a [`#[unimock]`](unimock) annotation which makes its behaviour mockable.
2. `fn takes_foo` accepts some type that implements the trait. This function adheres to zero-cost _Inversion of Control/Dependency Inversion_.
3. A mock instantiation by calling [`mock(None)`](mock()), which returns a [Unimock] value which is passed into `takes_foo`.

The [mock()] function takes an argument, in this case the value `None`. The argument is _what behaviour are we mocking_, in this case [None] at all!
`Foo` contains no methods, so there is no behaviour to mock.

## Methods and behaviour mocking

In order to be somewhat useful, the traits we abstract over should contain some methods. In a unit test for some function, we'd like
to mock the behaviour of that function's dependencies (expressed as trait bounds).

[mock()] accepts a collection of [Clause]s. Clauses carry the full recipe on how Unimock will behave once instantiated.

Given some trait,

```rust
#[unimock]
trait Foo {
    fn foo(&self) -> i32;
}
```

we would like to tell unimock what `Foo::foo`'s behaviour will be, i.e. what it will return. In order to do that, we first need to refer to the method.
In Rust, trait methods aren't reified entities, they are not types nor values, so they cannot be referred to in code.
Therefore, the unimock macro creates a surrogate type to represent it. By default, this type will be called

`Foo__foo`.

This type will implement [MockFn], which is the entrypoint for creating clauses:

```rust
#[unimock]
trait Foo {
    fn foo(&self) -> i32;
}

fn test_me(foo: impl Foo) -> i32 {
    foo.foo()
}

let clause = Foo__foo::each_call(matching!()).returns(1337).in_any_order();

assert_eq!(1337, test_me(mock(Some(clause))));
```

[Clause] construction is a type-state machine that in this example goes through 3 steps:

1. `Foo__foo::each_call(matching!())`: Define a _call pattern_. Each call to `Foo::foo` that matches the empty argument list (i.e. always matching, since the method is paremeter-less)
2. `.returs(1337)`: Each matching call will return the value `1337`
3. `.in_any_order()`: this directive describes how the resulting Clause behaves in relation to other clauses in the behaviour description, and returns it. In this example there is only one clause.

#### Call patterns (matching inputs)

It is common to want to control how a function will respond in relation to what input is given to it!
Inputs are matched by a function that receives the inputs as a tuple, and returns whether it matched as a [bool].
A specific [MockFn] together with an input matcher is referred to as a _call pattern_ from now on.

The [matching] macro provides syntax sugar for argument matching. It has a syntax inspired by the [std::matches] macro.

Inputs being matched is a condition that needs to be fulfilled in order for the rest of the call pattern to be evaluated.

#### Specifying outputs
Specifying outputs can be done in several ways. The simplest one is `returns(something)`. Different ways of specifying outputs are
found in [build::Match].

## Combining clauses
[mock()] accepts as argument anything that can be converted to a clause iterator, so that you can specify more that one kind of behaviour!
An iterator has a specific order of items, and sometimes the order of clauses matters too. It will depend on the type of clause.

Other mocking libraries often have distinctions between several kinds of "test doubles". Terminology varies. Unimock uses this terminology:

* _Mock_: A test double where every valid interaction must be declared up front.
* _Spy_: A test double which behaves as release code, unless behaviour is overridden.
* _Stub_: Defined behaviour for a single function, where the order of calls does not matter.

Now that terminology is in place for unimock, let's look at various ways to combine clauses.

```rust
#[unimock]
trait Foo {
    fn foo(&self, arg: i32) -> i32;
}

#[unimock]
trait Bar {
    fn bar(&self, arg: i32) -> i32;
}

fn test_me(deps: &(impl Foo + Bar), arg: i32) -> i32 {
    deps.bar(deps.foo(arg))
}

assert_eq!(
    42,
    test_me(
        &mock([
            Foo__foo::each_call(matching!(_))
                .answers(|arg| arg * 3)
                .in_any_order(),
            Bar__bar::each_call(matching!((arg) if *arg > 20))
                .answers(|arg| arg * 2)
                .in_any_order(),
        ]),
        7
    )
);

// alternatively, define _stubs_ for each method.
// This is a nice way to group methods by introducing a closure scope:
assert_eq!(
    42,
    test_me(
        &mock([
            Foo__foo::stub(|each| {
                each.call(matching!(1337)).returns(1024);
                each.call(matching!(_)).answers(|arg| arg * 3);
            }),
            Bar__bar::stub(|each| {
                each.call(matching!((arg) if *arg > 20)).answers(|arg| arg * 2);
            }),
        ]),
        7
    )
);
```

In both these examples, the order in which the clauses are specified do not matter, _except for input matching_.
In order for unimock to find the correct response, call patterns will be matched in the sequence they were defined.

## Interaction verifications

Unimock has one built-in verification that is always enabled:

_Every [MockFn] that is introduced in some clause, *must* be called at least once._

If this requirement is not met, Unimock will panic inside its Drop implementation. The reason
is to help avoiding "bit rot" accumulating over time inside test code. When refactoring release
code, tests should always follow along and not be overly generic.

Every unimock verification happens automatically in [Unimock::drop].

#### Optional call count expectations in call patterns
To make a call count expectation for a specific call pattern, look at [build::QuantifyResponse], which
has methods like `once()`, `n_times(n)` and `at_least_times(n)`.

With exact quantification in place, we can produce output sequences by chaining output definitions:

```no_compile
each.call(matching!(_)).returns(1).n_times(2).then().returns(2);
```

The output sequence will be `[1, 1, 2, 2, 2, ..]`. A call pattern like this is _expected_ to be called at least 3 times.
2 times because of the first exact output sequence, then at least one time because of the `.then()` combinator.

#### Verifying exact sequence of calls
Exact call sequences may be expressed using _strictly ordered clauses_. Use [MockFn::next_call] to define a call pattern, and
[build::QuantifiedResponse::in_order] to make it into a clause.

```no_compile
mock([
    Foo_foo::next_call(matching!(3)).returns(5).once().in_order(),
    Bar_bar::next_call(matching!(8)).returns(7).n_times(2).in_order(),
]);
```

Order-sensitive clauses and order-insensitive clauses (like `::stub`) do not interfere with each other.
However, these kinds of clauses cannot be combined _for the same MockFn_ in a single Unimock value.

## Application architecture

Writing larger, testable applications with unimock requires some degree of architectural discipline. We already know
how to specify dependencies using trait bounds. But would this scale in practice when several layers are involved?
One of the main features of unimock is that all traits are implemented by [Unimock]. This means that trait bounds
can be composed, and we can use _one value_ that implements all our dependencies:

```rust
fn some_function(deps: &(impl A + B + C), arg: i32) {
    // ..
}
```

In a way, this function resembles a `self`-receiving function. The `deps` argument is how the function
abstracts over its dependencies. Let's keep this call convention and let it scale a bit by introducing two layers:

```rust
use std::any::Any;

trait A {
    fn a(&self, arg: i32) -> i32;
}

trait B {
    fn b(&self, arg: i32) -> i32;
}

fn a(deps: &impl B, arg: i32) -> i32 {
    deps.b(arg) + 1
}

fn b(deps: &impl Any, arg: i32) -> i32 {
    arg + 1
}
```

The dependency from `fn a` to `fn b` is completely abstracted away, and in test mode
the `deps: &impl X` gets substituted with `deps: &Unimock`. Unimock is only concerned with the
testing side of the picture. To wire all of this together to a full-fledged runtime solution
without too much boilerplate, reach for the _[entrait pattern](https://docs.rs/entrait)_.

#### Combining release code and mocks: Spying
Unimock can be used to create arbitrarily deep integration tests, mocking away layers only indirectly used.
For that to work, unimock needs to know how to call the "real" implementation of traits.

See the documentation of [Unmock] and [spy] to see how this works.

Although this can be implemented with unimock directly, it works best with a higher-level
macro like [entrait](https://docs.rs/entrait).

## Misc
Unimock works best with high-level abstractions over function calls. It does not work that well
with generic traits or traits with associated types.

License: MIT
