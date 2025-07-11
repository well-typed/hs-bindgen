# Developer Documentation

This folder contains documentation specifically for developers working
on `hs-bindgen`. The documentation here focuses on development workflows,
environment setup, and building examples rather than end-user usage. As such,
this is intended for contributors and maintainers of `hs-bindgen`.

For user-facing documentation about using `hs-bindgen` in your projects,
please refer to the [main documentation](../manual).

## Contents

- [Development Environment](dev-environment.md): Instructions for setting up your
  development environment on Linux, NixOS, macOS, and Windows, including
  platform-specific quirks and requirements.
- [Building the Manual](building-manual.md): Detailed instructions for building
  and running the manual on all supported platforms (Linux, macOS, and
  Windows).
- [Project-structure.md](project-structure.md): Overview of the project
  structure and codebase organization to help new developers understand
  the repository layout.
- [Tests](testing.md): Guide to running tests, test structure, and
  testing best practices for the project.
- [Troubleshooting](troubleshooting.md): Collection of troubleshooting
  recipes.

## Quick Start

1. Follow the environment setup guide for your platform in
   [dev-environment.md](dev-environment.md)
2. Build and run the manual following the instructions in
   [building-manual.md](building-manual.md)
3. If you encounter issues, check the platform-specific troubleshooting
   sections in those files

## Useful discussion threads

- [Clang vs GCC backend](https://github.com/well-typed/hs-bindgen/issues/847)
- [Binding generation examples](https://github.com/well-typed/hs-bindgen/issues/91)
