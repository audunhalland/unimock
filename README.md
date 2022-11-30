# unimock

[<img alt="crates.io" src="https://img.shields.io/crates/v/unimock.svg?style=for-the-badge&logo=rust" height="20">](https://crates.io/crates/unimock)
[<img alt="docs.rs" src="https://img.shields.io/docsrs/unimock?style=for-the-badge&logo=docs.rs" height="20">](https://docs.rs/unimock)
[<img alt="CI" src="https://img.shields.io/github/workflow/status/audunhalland/unimock/Rust/main?style=for-the-badge&logo=github" height="20">](https://github.com/audunhalland/unimock/actions?query=branch%3Amain)

<!-- cargo-rdme start -->


`unimock` is a library for defining _mock implementations_ of traits.

Mocking, in a broad sense, is a way to control API behaviour during test execution.

The _uni_ in unimock indicates one-ness: All mockable traits are implemented by a single type, Unimock.
This design allows for a great flexibility in coding style, as will be demonstrated further down.

The first code example is the smallest possible use of unimock:

```rust
use unimock::*;

#[unimock]
trait Foo {}

fn takes_foo(foo: impl Foo) {}

takes_foo(Unimock::new(()));
```

1. `trait Foo` is declared with the `#[unimock]` attribute which makes its behaviour mockable.
2. `fn takes_foo` accepts some type that implements the trait. This function adheres to zero-cost _Inversion of Control/Dependency Inversion_.
3. A mock instantiation by calling `Unimock::new(())`, which crates a `Unimock` value which is passed into `takes_foo`.

The `new` function takes an argument called `setup` (implementing `Clause`), in this case the unit value `()`.
The setup argument is _what behaviour is being mocked_, in this case nothing at all.
`Foo` contains no methods, so there is no behaviour to mock.

### Methods and behaviour mocking

In order to be somewhat useful, the traits we abstract over should contain some methods.
In a unit test for some function, we'd like to mock the behaviour of that function's dependencies (expressed as trait bounds).

Given some trait,

```rust
#[unimock]
trait Foo {
    fn foo(&self) -> i32;
}
```

we would like to tell unimock what `Foo::foo`'s behaviour will be, i.e. what it will return.
In order to do that, we first need to refer to the method.
In Rust, trait methods aren't reified entities, they are not types nor values, so they cannot be referred to in code.
We need to tell unimock to expose a separate mocking API.
This API will be created in form of a new module, [which is named](#selecting-a-name-for-the-mock-api) by passing e.g. `api=TraitMock` to the unimock macro invocation.

Each of the trait's original methods will get exported as mock config entrypoints through this module: For example `TraitMock::method`.
`method` is a type that will implement `MockFn`, which is the entrypoint for creating a `Clause`:

```rust
#[unimock(api=FooMock)]
trait Foo {
    fn foo(&self) -> i32;
}

fn test_me(foo: impl Foo) -> i32 {
    foo.foo()
}

let clause = FooMock::foo.each_call(matching!()).returns(1337);

assert_eq!(1337, test_me(Unimock::new(clause)));
```

Clause construction is a type-state machine that in this example goes through two steps:

1. `FooMock::foo.each_call(matching!())`: Define a _call pattern_.
   Each call to `Foo::foo` that matches the empty argument list (i.e. always matching, since the method is parameter-less).
2. `.returns(1337)`: Each matching call will return the value `1337`.
   In this example there is only one clause.

#### Call patterns (matching inputs)

It is common to want to control how a function will respond in relation to what input is given to it!
Inputs are matched by a function that receives the inputs as a tuple, and returns whether it matched as a `bool`.
A specific `MockFn` together with an input matcher is referred to as a _call pattern_ from now on.

The `matching!` macro provides syntax sugar for argument matching.
It has a syntax inspired by the [`std::matches`](https://doc.rust-lang.org/std/macro.matches.html) macro.

Inputs being matched is a condition that needs to be fulfilled in order for the rest of the call pattern to be evaluated.

#### Specifying outputs (responses)
Specifying outputs can be done in several ways. The simplest one is `returns(some_value)`.
Different ways of specifying outputs are found in `build::DefineResponse`.

There are different constraints acting on return values based on how the clause gets initialized:

* `some_call` is tailored for calls that will happen once. Return values have no [Clone] constraint.
* `each_call` is tailored for calls that are expected to happen more than once, thus requiring [Clone] on return values.
* `next_call` is used for [verifying exact call sequences](#verifying-exact-sequence-of-calls), otherwise works similar to `some_call`.



### Combining setup clauses
`Unimock::new()` accepts as argument anything that implements [Clause].
Basic setup clauses can be combined into composite clauses by using _tuples_:

```rust
#[unimock(api=FooMock)]
trait Foo {
    fn foo(&self, arg: i32) -> i32;
}

#[unimock(api=BarMock)]
trait Bar {
    fn bar(&self, arg: i32) -> i32;
}

fn test_me(deps: &(impl Foo + Bar), arg: i32) -> i32 {
    deps.bar(deps.foo(arg))
}

assert_eq!(
    42,
    test_me(
        &Unimock::new((
            FooMock::foo
                .some_call(matching!(_))
                .answers(|arg| arg * 3),
            BarMock::bar
                .some_call(matching!((arg) if *arg > 20))
                .answers(|arg| arg * 2),
        )),
        7
    )
);

// alternatively, define _stubs_ for each method.
// This is a nice way to group methods by introducing a closure scope:
assert_eq!(
    42,
    test_me(
        &Unimock::new((
            FooMock::foo.stub(|each| {
                each.call(matching!(1337)).returns(1024);
                each.call(matching!(_)).answers(|arg| arg * 3);
            }),
            BarMock::bar.stub(|each| {
                each.call(matching!((arg) if *arg > 20)).answers(|arg| arg * 2);
            }),
        )),
        7
    )
);
```

In both these examples, the order in which the clauses are specified do not matter, _except for input matching_.
In order for unimock to find the correct response, call patterns will be matched in the sequence they were defined.

### Interaction verifications
Unimock performs interaction verifications using a declarative approach.
Expected interactions are configured at construction time, using [Clause]s.
Rust makes it possible to automatically verify things because of RAII and the [drop] method, which Unimock implements.
When a Unimock instance goes out of scope, Rust automatically runs its verification rules.

One verification is always enabled in unimock:

_Each `MockFn` mentioned in some setup clause must be interacted with at least once._

If this requirement is not met, Unimock will panic inside its Drop implementation.
The reason is to help avoiding "bit rot" accumulating over time inside test code.
When refactoring release code, tests should always follow along and not be overly generic.

In general, clauses do not only encode what behaviour is _allowed_ to happen, but also that this behaviour necessarily _must happen_.

#### Optional call count expectations in call patterns
To make a call count expectation for a specific call pattern,
   look at [`Quantify`](build::Quantify) or [`QuantifyReturnValue`](build::QuantifyReturnValue), which have methods like
   [`once()`](build::Quantify::once),
   [`n_times(n)`](build::Quantify::n_times) and
   [`at_least_times(n)`](build::Quantify::at_least_times).

With exact quantification in place, _output sequence_ verifications can be constructed by chaining combinators:

```rust
each.call(matching!(_)).returns(1).n_times(2).then().returns(2);
```

The output sequence will be `[1, 1, 2, 2, 2, ..]`.
A call pattern like this _must_ be called at least 3 times.
2 times because of the first exact output sequence, then at least one time because of the [`.then()`](build::QuantifiedResponse::then) combinator.

#### Verifying exact sequence of calls
Exact call sequences may be expressed using _strictly ordered clauses_.
Use [`next_call`](MockFn::next_call) to define this kind of call pattern.

```rust
Unimock::new((
    FooMock::foo.next_call(matching!(3)).returns(5),
    BarMock::bar.next_call(matching!(8)).returns(7).n_times(2),
));
```

All clauses constructed by `next_call` are expected to be evaluated in the exact sequence they appear in the clause tuple.

Order-sensitive clauses and order-insensitive clauses (like [`some_call`](MockFn::some_call)) do not interfere with each other.
However, these kinds of clauses cannot be combined _for the same MockFn_ in a single Unimock value.

### Application architecture

Writing larger, testable applications with unimock requires some degree of architectural discipline.
We already know how to specify dependencies using trait bounds.
But would this scale in practice when several layers are involved?
One of the main features of unimock is that all traits are implemented by `Unimock`.
This means that trait bounds can be composed, and we can use _one value_ that implements all our dependencies:

```rust
fn some_function(deps: &(impl A + B + C), arg: i32) {
    // ..
}
```

In a way, this function resembles a `self`-receiving function.
The `deps` argument is how the function abstracts over its dependencies.
Let's keep this call convention and let it scale a bit by introducing two layers:

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

The dependency from `fn a` to `fn b` is completely abstracted away, and in test mode the `deps: &impl X` gets substituted with `deps: &Unimock`.
But Unimock is only concerned with the _testing_ side of the picture.
The previous code snippet is at the extreme end of the loosely-coupled scale: _No coupling at all!_
It shows that unimock is merely a piece in a larger picture.
To wire all of this together into a full-fledged runtime solution, without too much boilerplate, reach for the _[entrait pattern](https://docs.rs/entrait)_.

#### Gated mock implementation
If the trait definition, the uses of the trait bound and the tests all live within the same crate, it's possible to _gate_ the macro invocation:

```rust
#[cfg_attr(test, unimock(api = FooMock))]
trait Foo {}
```

#### Combining release code and mocks: Partial mocks
Unimock can be used to create arbitrarily deep integration tests, mocking away layers only indirectly used.
For that to work, unimock needs to know how to call the "real" implementation of traits.

See the documentation of `new_partial` to see how this works.

Although this can be implemented with unimock directly, it works best with a higher-level macro like [`entrait`](https://docs.rs/entrait).

#### Misc

##### What kinds of things can be mocked with unimock?
* Traits with any number of methods
* Traits with generic parameters, although these cannot be lifetime constrained (i.e. need to satisfy `T: 'static`).
* Methods with any self receiver (`self`, `&self`, `&mut self` or arbitrary (e.g. `self: Rc<Self>`)).
* Methods that take reference inputs.
* Methods returning references to self.
* Methods returning references to arguments.
* Methods returning a type containing lifetime parameters. For a mocked return they will have to be `'static`.
* Async methods when the trait is annotated with `#[async_trait]`.
* Methods that return a future that is an associated type. Requires nightly.

##### What kinds of traits or methods cannot be mocked?
* Traits with associated types. Unimock would have to select a type at random, which does not make a lot of sense.
* Static methods, i.e. no `self` receiver. Static methods with a _default body_ are accepted though, but not mockable.
* Methods receiving `&mut` arguments other than `&mut self`.
    It _might_ work, but is currently unsupported due to stricter lifetime constraints that is harder to express via generics.

#### Selecting a name for the mock `api`
Due to [macro hygiene](https://en.wikipedia.org/wiki/Hygienic_macro),
    unimock tries to avoid autogenerating any new identifiers that might accidentally create undesired namespace collisions.
To avoid user confusion through conjuring up new identifier names out of thin air, the name of the mocking API therefore has to be user-supplied.
Although the user is free to choose any name, unimock suggests following a naming convention.

The entity being mocked is a trait, but the mocking API is a module.
This introduces a conflict in naming convention style, since traits use CamelCase but modules use snake_case.

_The suggested naming convention is using the name of the trait (e.g. `Trait`) postfixed with `Mock`: The resulting module should be called `TraitMock`._

This will make it easier to discover the API, as it shares a common prefix with the name of the trait.



### Project goals
##### Use only safe Rust
Unimock respects the memory safety and soundness provided by Rust.
Sometimes this fact can lead to less than optimal ergonomics.

For example, in order to use `.returns(value)`, the value must (generally) implement `Clone`, `Send`, `Sync` and `'static`.
If it's not all of those things, the slightly longer `.answers(|_| value)` can be used instead.

##### Keep the amount of generated code to a minimum
The unimock API is mainly built around generics and traits, instead of being macro-generated.
Any mocking library will likely always require _some_ degree of introspective metaprogramming (like macros),
  but doing too much of that is likely to become more confusing to users, as well as taking longer to compile.
The `#[unimock]` macro does the minimal things to fill out a few simple trait impls, and that's it. There are no
complex functions or structs that need to be generated.

There is a downside to this approach, though.
Rust generics aren't infinitely flexible,
  so sometimes it's possible to misconfigure a mock in a way that the type system is unable to catch up front,
  resulting in runtime (or rather, test-time) failures.

All things considered, this tradedoff seems sound, because this is only testing, after all.

##### Use nice, readable APIs
Unimock's mocking API has been designed to read like natural english sentences.

This was a fun design challenge, but it arguably also has some real value.
It is assumed that code is quicker (and perhaps more fun) to read and write when it resembles real language.

<!-- cargo-rdme end -->
