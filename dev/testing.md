# Testing Guide

This document covers tests and how to run them for the `hs-bindgen` project.

## Test Structure

### Golden Tests

## Running Tests

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

```
- name: Setup tmate session
  uses: mxschmitt/action-tmate@v3
```

To the CI workflow. This will give an address one can ssh to and debug things.
