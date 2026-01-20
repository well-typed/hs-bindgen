# Constructor import issue

This example demonstrates that we solve the constructor import issue. See issue
[#1282](https://github.com/well-typed/hs-bindgen/issues/1282).

Running the Haskell executable is not so interesting. This example is aimed
primarily at showing that the generated bindings compile. The bindings are
generated in such a way to elicit the constructor import issue. If the bindings
compile, then the constructor import issue is fixed. This example also serves as
a regression test, preventing the bug from being re-introduced in the future.

## Prerequisites

No additional installation requirements

## Running the Example

```bash
./generate-and-run.sh
```
