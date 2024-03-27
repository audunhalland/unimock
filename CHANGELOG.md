# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
### Added
- Basic support for mutable outputs ([#49](https://github.com/audunhalland/unimock/pull/49))

## [0.6.2] - 2024-03-27
### Fixed
- Don't trigger `manual_async_fn` lint when mocking `-> impl Future` methods.

## [0.6.1] - 2024-03-27
### Fixed
- Unmocking using `async fn` when the trait fn has an `-> impl Future` signature.

## [0.6.0] - 2024-03-25
### Changed
- Unimock now supports very flexible argument mutation, instead of one hard-coded parameter.
  To achieve this, the `answers` API had to be redesigned with a new signature based on a `dyn Fn`.
  This dynamic function type has one fixed signature per `MockFn`, so its return type isn't generic as it used to be in `0.5.x`.
  All generated borrows have to be done explicitly through `Unimock::make_ref` for this to work. ([#43](https://github.com/audunhalland/unimock/pull/43), [#47](https://github.com/audunhalland/unimock/pull/47))
  - The function passed to `answers` must be a `&static` Fn, _or_ it can be an `Arc` closure that can be registered by calling `answers_arc`.
  - The parameters passed to this function are the same as passed to the mocked trait method, including `self`.
- Output trait hierarchy (which allows safely mocking borrowed return values) rewritten to be more flexible and future-proof than previously ([#46](https://github.com/audunhalland/unimock/pull/46))
- `default_implementation` renamed to `applies_default_impl`.
- `unmocked` renamed to `applies_unmocked`.
### Added
- Mocks for `tokio-1` and `futures-io-0-3` async read/write traits ([#45](https://github.com/audunhalland/unimock/pull/45))
### Fixed
- Fix `matching!` against references to number literals ([#42](https://github.com/audunhalland/unimock/pull/42))
- Borrows from function arguments can now be made without leaking memory ([#47](https://github.com/audunhalland/unimock/pull/47))
### Removed
- The `mutates` builder APIs. These are now handled using `answers`.

## [0.5.8] - 2024-01-15
### Fixed
- Mutation when generics are involved. ([#38](https://github.com/audunhalland/unimock/pull/38))

## [0.5.7] - 2023-11-16
### Added
- (static) Associated constants support with `#[unimock(const FOO: Type = value;)]`.

## [0.5.6] - 2023-11-15
### Fixed
- Proper `no_std` support by disabling default features in `once_cell`. ([#35](https://github.com/audunhalland/unimock/pull/35))

## [0.5.5] - 2023-10-23
### Added
- Support for async fn through RPITIT, i.e. `fn -> impl Future<Output = _>`. ([#34](https://github.com/audunhalland/unimock/pull/34))
### Fixed
- `Pin<&mut Self>` receivers in combination with the default impl delegator with the help of [polonius-the-crab](https://github.com/danielhenrymantilla/polonius-the-crab.rs)! ([#30](https://github.com/audunhalland/unimock/pull/30))
- `&mut Self` receivers that returns references to self, within default body (same fix as above).

## [0.5.4] - 2023-10-03
### Added
- Mixed return type support for `core::task::Poll<T>` where `T` is already mixed.
### Fixed
- Default methods combined with arbitrary Self types like `Rc<Self>`/`Arc<Self>`/`Pin<&mut Self>`.
- Argument debugging for `&mut T` args where `T` does not implement `Debug`.
- Compile error for `&mut T<'_>` arguments that are not mock-mutable.
- Missing `.await` syntax when using default async methods.
- Fully qualified method call syntax in default method glue, to avoid ambiguity.

## [0.5.3] - 2023-07-07
### Fixed
- Mock of lifetime generic trait
### Added
- Missing MIT license file

## [0.5.2] - 2023-04-30
### Fixed
- Documentation inaccuracies

## [0.5.1] - 2023-04-12
### Fixed
- Remove outdated dependency `doc-cfg`.

## [0.5.0] - 2023-04-12
### Added
- Support for mutating one `&mut` parameter with `.mutates`.
- Support for falling back to default implementations of methods.
- (static) Associated type support with `#[unimock(type T = Foo;)]`.
- `no_std` support, by opting out of the `"std"` feature.
- `spin-lock` feature for `no_std` users.
- `mock-core` feature for mocking implementations of traits in `core`.
- `mock-std` feature for mocking implementations of traits in `std`.
- Unimock now implements `UnwindSafe` and `RefUnwindSafe` by manual implementation.
### Changed
- `macro_api` renamed to `private` and excluded from docs.
- `MockFn::NAME` replaced by `fn info() -> MockFnInfo`.
- macro keyword `emulate` renamed to `mirror`.
### Fixed
- Generics involving `Self`.

## [0.4.12] - 2023-03-29
### Fixed
- Unnecessary `&self` reference in macro-generated code if self was already a reference.

## [0.4.11] - 2023-03-29
### Fixed
- Code generation: Don't parenthesize inputs when there is only one argument.
- Require dependency versions closer to latest releases.
- `unimock_macros`: Added `README`.
- Most hyperlinks in README.md.
### Added
- Trait emulation, only intended for internal (and future) use.

## [0.4.10] - 2023-03-20
### Changed
- Upgrade to `syn` 2.0.

## [0.4.9] - 2023-01-07
### Added
- Support for generic trait methods.

## [0.4.8] - 2022-12-30
### Fixed
- cfg attrs on trait methods are now honoured.

## [0.4.7] - 2022-12-21
### Added
- Panic when the original unimock is destroyed in another thread than the one that created it.

## [0.4.6] - 2022-12-14
### Changed
- Make inexact quantification (at_least_times) of strictly ordered clauses (next_call) into a compile error instead of a runtime error.
### Added
- `Vec<&T>` output values.
- Tuple output values with any combination of borrows of up to 4-element tuples.
### Fixed
- `matching!` of str-like arguments in or-pattern (`"a" | "A"`).
- Remove `T: Clone` bound on `-> Option<&T>` multi-return functions.
- Remove `T: Clone` bound on `-> Result<&T, E>` multi-return functions. There is still a `E: Clone` bound.

## [0.4.5] - 2022-12-07
### Changed
- Now depends on `once_cell` instead of `lazycell` internally.
### Fixed
- Theoretical stack overflow when dropping unimock (in case of many borrowed return values).
- A bug in code generation for `impl Future` lifetime bounds.

## [0.4.4] - 2022-11-30
### Changed
- The unimock macro now generates `Output<'u> = Self::Response;` in MockFn if the Response and Output types are the same type. This is a backwards compatible change.
- Order of keywords in Cargo.toml.
### Added
- Note in the crate documentation about `&mut` arguments and tricky lifetimes.

## [0.4.3] - 2022-11-28
### Changed
- Cargo package description
### Added
- More cargo keyword variants
### Fixed
- A bug where an input lifetime was rewritten to a self lifetime in the case of borrowed output

## [0.4.2] - 2022-11-28
### Added
- Documentation note about `#[cfg_attr(test, unimock)]`.
### Fixed
- Compile error when `matching!` a literal string pattern against a type that implements both `AsRef<str>` and `Debug`.

## [0.4.1] - 2022-11-24
### Added
- New default feature: `pretty-print`
### Fixed
- Hygiene issue when mocking a function with an argument called `eval` which can be unmocked.
- Using `eq!` or `ne!` as part of disjuctive input tuple in `matching!` macro.
- Diagnostics on mismatched call patterns. Now uses a debug print solution based on `pretty_assertions`.

## [0.4.0] - 2022-11-20
### Changed
- `#[unimock]` is now more hygienic and does not autogenerate a mock api identifier by default.
    Instead, the user-facing mock API is opt-in by specifying `api=my_ident`.
- `Unimock::new(setup)` replaces `mock(clause)`.
- `Unimock::new_partial(setup)` replaces `spy(clause)`.
- `Clause` is now a trait instead of a type.
- Clause composition is now done using tuples instead of `impl IntoIterator`, because clauses now have non-uniform types.
- The generated `MockFn` structs are now put in a module called `TraitMock` where `Trait` is the name of the trait.
  The name of the generated structs are the same as the names of the methods.
- Renamed the `unmocked=[..]` attribute option to `unmock_with=[..]`.
- Improvements to function output modelling.
- Mock builder API simplification. There is no longer any `returns_ref` or `returns_static`, just `returns`.
- Unimock is now implemented with Generic Associated Types, and requires Rust minimum 1.65.
### Added
- New mock entry point `some_call` which does not require return value to implement `Clone`. `next_call` now also uses this API.
- `api=[..]` attribute option for opting out of generating an encapsulating module.
- Support for self references in `Option` and `Result` return values (e.g. `-> Option<&str>`)
- Better mismatch debugging with improved `matching!` API under the hood.
- Support for `Eq`-based matching in the `matching!` macro.
### Fixed
- Avoid spitting out `#[allow(non_camel_case_types)]` when the user supplied the MockFn type names.
### Removed
- `mod = *` attribute option at trait level.
- The `.in_any_order()` and `.in_order()` builder methods. Those types instead just implement `Clause`.
- The `Unmock` trait.

## [0.3.14] - 2022-08-01
### Added
- Support for unpacked modules with `mod=*` and support uniform MockFn aliases with `as=alias`
### Changed
- Refactor code according to clippy lints.

## [0.3.13] - 2022-07-17
### Fixed
- Problem/regression regarding trailing comma in inputs destructuring.

## [0.3.12] - 2022-07-16
### Fixed
- Using generics when the MockFn is put in a separate module.
- Generic unmockable functions.
### Changed
- Some more internal refactoring to keep more functions non-generic.

## [0.3.11] - 2022-07-16
### Added
- Support for mocking generic traits. In order to work, the generic arguments have to be `'static`.
### Changed
- Moved eval methods from Unimock into macro_api. Technically breaking, but these APIs were never supposed to be used directly from applications.
  It will be clearer that these are internal implementation details when part of macro_api rather than public methods on Unimock.
### Fixed
- Now supports skipping methods without a self receiver, but with a default body

## [0.3.10] - 2022-07-15
### Added
- Support for return types with lifetime parameters. These are turned into `'static` for unimock.

## [0.3.9] - 2022-07-14
### Added
- Support for return references borrowed from arguments instead of self.

## [0.3.8] - 2022-07-13
### Fixed
- Output `async_trait` attribute on the same form that it was parsed.

## [0.3.7] - 2022-07-13
### Added
- `prefix` parameter to the unimock macro, in case the crate is re-exported through another crate.

## [0.3.6] - 2022-07-11
### Changed
- Lots of internal refactoring to keep more things non-generic, saves code size.

## [0.3.5] - 2022-06-30
### Added
- Add cargo keyword, cargo categories.

## [0.3.4] - 2022-06-30
### Changed
- Simplify various lifetime signatures, remove unneeded outlives bounds, remove elidable lifetimes.
### Added
- More documentation, project goals

## [0.3.3] - 2022-06-27
### Added
- Support for methods returning associated futures, through return-position-impl-trait. When used, requires `generic_associated_types` and `type_alias_impl_trait`.
### Removed
- `mockall` dev dependency

## [0.3.2] - 2022-06-26
### Changed
- Fix missing README file on crates.io

## [0.3.1] - 2022-06-26
### Changed
- Pretty formatting for fn signatures in generated MockFn documentation.

## [0.3.0] - 2022-06-22
### Added
- self receiver in MockFn call pattern constructors. This works much better with rustfmt,
  which tends to move a dot-method call to the next line, indented. This means that each
  new mock will start naturally in the clause array, and all subsequent setup methods will be
  indented, so it's much easier to see where each mock starts and ends.
- Documentation attributes for generated MockFns
- Ability to unmock with a specific argument list: `unmocked=[foo(a, b)]`
### Removed
- Breaking: Ability to make call patterns with `T::method`, as these methods now take a self parameter.

## [0.2.1] - 2022-06-12
### Removed
- `RefUnwindSafe` bound on stored data, to allow things like `UnsafeCell` in return types.

## [0.2.0] - 2022-06-03
### Changed
- Change MockFn to not require generic associated types, by introducing new trait MockInputs
- Improvements to panic messages and related tests

## [0.2.0-beta.0] - 2022-05-15
### Added
- Completely new API for specifying trait mocks, greatly reducing amount of generated code
- Generic Associated Types dependency
- Unmocking/spying feature

### Removed
- `mockall` dependency

## [0.1.0] - 2022-03-13
### Added
- The `unimock` attribute macro.
- The `Unimock` type.
