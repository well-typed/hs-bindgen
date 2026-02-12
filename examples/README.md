# Examples

This directory contains runnable example projects using `hs-bindgen`. These
examples include bindings and toy programs for the following C libraries:

* `bundled-c`
* `c-minisat`
* `libpcap`

## Running examples

### Requirements

The examples are only tested (in CI) on Ubuntu. They might work on other
distributions such as Windows or MacOS as well, but we offer no guarantees.

The following software should be installed before trying to build and run one of
the examples:

* A version of GHC that is compatible with `hs-bindgen`, such as `9.6.x`
* A version of Cabal that is compatible with `hs-bindgen`, such as `3.16.x`
* A version of LLVM (and Clang) that is compatible with `hs-bindgen`, such as `15`

There are a number of other "basic" packages that have to be installed on the
system as well, such as `git` and a `gcc` toolchain. Most systems will have
these installed already.

## Building and running

Most example projects still require other software to be installed. These
prerequisites are described in the README file of each example project. With the
requirements met, building and running an example should be as simple as
`cd`-ing into the example's directory and running the `generate-and-run.sh`
script.

To run the `c-minisat` example:

```bash
cd c-minisat
./generate-and-run.sh
```

## Adding new examples

New examples should be put into their own directory under the
`REPOSITORY_ROOT/examples` directory. The author of the new example is in
principle free to structure the new example project as they wish, but it is
advisable to follow the structure of existing example projects, in part to make
it easy to integrate the new example with our CI.

### CI integration

Let's say we we want to create an example project for the `libfoo` C library,
and also build and run it in CI. Integration is rather straightforward as long
as the following requirements are met:

* Creat a directory at `REPOSITORY_ROOT/examples/libfoo`
* Create a Haskell package at
   `REPOSITORY_ROOT/examples/libfoo/hs-project/libfoo.cabal`
  * The package should include an executable component `libfoo-bin`
  * The package should have its own project file at
    `REPOSITORY_ROOT/examples/libfoo/hs-project/cabal.project`, which should
    contain at least:

    ```cabal
    import: ../../../cabal.project.base
    packages: .
              ../../../c-expr-runtime
              ../../../hs-bindgen-runtime
    ```

    If other `hs-bindgen` packages are required, add them to `packages`.
* Add a script at `REPOSITORY_ROOT/examples/libfoo/generate-and-run.sh`
* The script should install the `libfoo` C package (locally)
* The script should run `hs-bindgen-cli` on `libfoo`'s header files and put
    the generated modules into the Haskell project
* The script should make sure that the Haskell package can find the installed
    `libfoo` package. For locally installed packages, this probably means
    setting `LD_LIBRARY_PATH` and updating the
    `REPOSITORY_ROOT/examples/libfoo/hs-project/cabal.project.local` file so
    that it includes:

    ```cabal
    package libfoo
      extra-include-dirs:
        -- insert absolute path to installation directory for header files here
      extra-lib-dirs:
        -- insert absolute path to installation directory for dll files here
    ```

    If a `cabal.project.local` file already exists, then the file should be
    updated to include the lines above. Otherwise, it should create the file
    with the lines above.
* The script should run the Haskell executable
* Add a composite action by creating a new file at
   `REPOSITORY_ROOT/.github/actions/examples/libfoo.action.yml`
  * The composite action should install example-specific prerequisites, such as
    system packages
  * The composite action should run the
    `REPOSITORY_ROOT/examples/libfoo/generate-and-run.sh` script
* Update the workflow file at `REPOSITORY_ROOT/.github/workflows/examples.yml`
  * Add `'libfoo'` to the `example` array of the workflow matrix
  * Towards the end of the file, add a step that calls the `libfoo` composite action:

    ```yml
    - name: 🧪 Build and run libfoo example
      if: ${{ matrix.example == 'libfoo' }}
      uses: ./.github/actions/examples/libfoo
    ```

Now create a PR with these changs and (hopefully) you will observe that a new
job is run that tests the new example project.
