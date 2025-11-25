# QR Code Generator Haskell Bindings

This example demonstrates generating Haskell bindings for
[QR-Code-generator](https://github.com/nayuki/QR-Code-generator), a C library
for generating QR codes.

## Running the Example

```bash
./generate-and-run.sh
```

This script will:

1. Build the QR Code generator C library
2. Generate Haskell bindings using `hs-bindgen`
3. Create `cabal.project.local` with the necessary configuration
4. Build and run the example program

## Quirks and Workarounds

### 1. Using Raw Wrappers Instead of Safe Bindings

The example code uses `_wrapper` functions (e.g., `qrcodegen_getSize_wrapper`,
`qrcodegen_getModule_wrapper`) instead of the "safe" versions exported by the
`Generated.Safe` module.

The safe bindings expect `IncompleteArray Word8` arguments:

```haskell
qrcodegen_getSize :: IncompleteArray Word8 -> IO CInt
```

However the array is not read-only and using `IncompleteArray` and the example
allocates QR code buffers using `allocaArray`, which produces raw `Ptr Word8`
values.

```haskell
qrcodegen_getSize_wrapper :: Ptr Word8 -> IO CInt
```

### 2. Linking Against the C Library

The `.cabal` file includes `extra-libraries: qrcodegen`, which tells the linker
to link against `libqrcodegen.so`. The library name should match the base name
of the binary file (without the `lib` prefix and `.so` extension).
