# Functions

## Introduction

TODO

## Function pointers

TODO

## Userland CAPI

TODO

### By-value `struct` arguments or return values

TODO

### Static inline functions

A common idiom in C is the use of `static inline` functions, such as

```c
static inline int mod_10(int x) { return x % 10; }
```

Such functions are declared in a header file; they are `inline` to encourage the
C compiler to inline them at use sites, and they are `static` because otherwise
every C file that would `#include` this header would contain a copy of this
function, resulting in duplicate symbols.

From the perspective of `hs-bindgen`, these look like ordinary functions,
and we will generate

```hs
foreign import ccall safe "<userland CAPI wrapper>"
  mod_10 :: CInt -> IO CInt
```
