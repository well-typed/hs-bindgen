# Testing Guide

This document covers tests and how to run them for the `hs-bindgen` project.

## Test Structure

### Golden Tests

## Running Tests

### Fast Mode

The `test-hs-bindgen` test suite supports a `--fast` (`-f`) flag that skips slow
tests for quicker development iteration:

```bash
cabal run test-hs-bindgen -- --fast
```

This skips:
- **TH fixture compilation tests** (~177 tests) — these run `cabal build` on
  generated TH modules and are the most expensive test group.
- **Unsafe golden tests** (~168 tests) — if `.Safe` passes, `.Unsafe` almost
  certainly will too, since they differ only in the foreign import safety
  annotation.

Full mode (the default) runs all tests and is unchanged.

### Debug Mode

Use `--debug` (`-v`) to print all trace messages during golden test execution:

```bash
cabal run test-hs-bindgen -- --debug
```

### Golden Test Updates

When tests fail due to expected output changes, update them with:

```bash
cabal run -- test-hs-bindgen --accept
```

## Test Development

### Adding New Tests

## Continuous Integration

### Test Failures

A very useful thing to debug failing CI is to add:

```yaml
- name: Setup tmate session
  uses: mxschmitt/action-tmate@v3
```

To the CI workflow. This will give an address one can ssh to and debug things.
