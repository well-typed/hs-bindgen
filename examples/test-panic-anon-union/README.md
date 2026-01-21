# Test: Anonymous Union Panic with External Binding Specs

This is a minimal reproducible test case for a panic that occurs when generating Haskell bindings for C headers containing anonymous unions, when using external binding specs.

## The Bug

When processing C headers with external binding specs that reference anonymous unions, hs-bindgen panics with:

```
PANIC!: the impossible happened
Unexpected multiple locations for anon decl DeclId {name = DeclName {text = "outer_u", kind = NameKindTagged TagKindUnion}, isAnon = True}: []
```

Location: `hs-bindgen/src-internal/HsBindgen/Frontend/LocationInfo.hs:79:16`

## Root Cause

The invariant at `LocationInfo.hs:77-84` expects anonymous declarations to have **exactly one** location. However, when an anonymous union is defined in one header and referenced via an external binding spec from another header, the location lookup returns **zero locations** (`[]`), triggering the panic.

## Test Case

The `test.h` file contains the minimal C pattern that triggers this bug:

1. **Struct with anonymous union**: `struct outer` contains a field `u` of anonymous union type
2. **Indirect reference**: `struct inner` uses `struct outer` as a field type
3. **External binding spec**: When processing with `--external-binding-spec`, the code looks up the anonymous union by name but finds no locations

## Reproduction

### Step 1: Generate binding spec for the anonymous union

```bash
cabal run hs-bindgen-cli -- preprocess \
    --hs-output-dir /tmp/test-output \
    --create-output-dirs \
    --module Test.Outer \
    --parse-all \
    --select-from-main-headers \
    --enable-program-slicing \
    --gen-binding-spec /tmp/binding-specs/outer.yaml \
    examples/test-panic-anon-union/test.h
```

### Step 2: Process using the external binding spec (triggers panic)

```bash
cabal run hs-bindgen-cli -- preprocess \
    --hs-output-dir /tmp/test-output \
    --create-output-dirs \
    --module Test.Inner \
    --parse-all \
    --select-from-main-headers \
    --enable-program-slicing \
    --external-binding-spec /tmp/binding-specs/outer.yaml \
    examples/test-panic-anon-union/test.h
```

### Quick Test

Or simply run the provided script:

```bash
./reproduce.sh
```

## Expected Behavior

The binding generation should succeed, properly handling anonymous unions referenced via external binding specs.

## Actual Behavior

Panic occurs due to empty location list for anonymous declaration.

## Real-World Example

This pattern was discovered when processing RPM headers:
- `rpm/rpmsw.h` defines `struct rpmsw_s` with an anonymous union field
- `rpm/rpmio.h` uses `struct rpmsw_s` as a field type in other structs
- Processing `rpmio.h` with external binding specs for `rpmsw.h` triggers the same panic

## Files

- `test.h` - Minimal C header that reproduces the issue
- `reproduce.sh` - Script to run both commands and demonstrate the panic
