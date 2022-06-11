# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
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
