# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
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
