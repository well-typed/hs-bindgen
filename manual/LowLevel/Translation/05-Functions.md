# Functions

## Introduction

This chapter discusses how `hs-bindgen` generates bindings for C functions.

## Safe vs unsafe foreign imports

When importing a C function, GHC allows us to choose between two calling
conventions: `safe` and `unsafe`. The distinction is important:

* **Safe** foreign imports may call back into Haskell code. They are more
  expensive.
* **Unsafe** foreign imports may not call back into Haskell. They are faster
  because they avoid this overhead, but using `unsafe` incorrectly can lead to
  undefined behavior.

To give users control over this choice, `hs-bindgen` generates two separate
modules for function bindings:

* `ModuleName.Safe` - contains all function imports using the `safe` calling
  convention
* `ModuleName.Unsafe` - contains all function imports using the `unsafe`
  calling convention

Both modules export identical APIs, differing only in their calling convention.
Users can import from whichever module best suits their needs.

## Function addresses

### Function pointers

In theory every C function is a candidate for being passed to other functions as
a function pointer. For example, consider the following two (contrived)
functions in a header file, with appropriate definitions in a body file that we
do not show here:

```c
extern int square(int);
extern int apply(int (*f)(int), int x); // applies f to x
```

We might use these functions as follows:

```c
int main() {
  printf("%d\n", apply(&square, 4)); // prints 16
}
```

To be able to pass C function pointers to other C functions through the Haskell
FFI, `hs-bindgen` generates for each C function a binding to the address of that
C function.

```hs
foreign import {-# details elided #-} square
  :: FC.CInt -> IO FC.CInt

-- This is a wrapper around an internal foreign import. Details are elided.
square :: F.FunPtr (FC.CInt -> IO FC.CInt)

foreign import {-# details elided #-} apply
  :: F.FunPtr (FC.CInt -> IO FC.CInt) -> FC.CInt -> IO FC.CInt

-- This is a wrapper around an internal foreign import. Details are elided.
apply :: F.FunPtr ((F.FunPtr (FC.CInt -> IO FC.CInt)) -> FC.CInt -> IO FC.CInt)
```

A binding to the address of a C function is generated using a stub, just like we
do for bindings to global variables. See the [Globals][globals] section of the
manual for more information about stubs.

Note that we've also generated `apply`, even though we did not need it in
this case. `hs-bindgen` generates address stubs for *all* function declarations.

We might use these bindings as follows:

```hs
main = do
  y <- apply square 4
  print y -- prints 16
```

[globals]:./Globals.md#Guidelines-for-binding-generation

### Implicit function to pointer conversion

In C, functions are not "first-class citizens", but *pointers to functions* can
be passed around freely. Typically, C code is explicit about the fact that it
deals with function pointers, but there are some cases where implicit conversion
happens, and we should take that into account for binding generation.

One such case is when function parameters have function types. Quoted from the
["Function declarations" section][creference:fun-decl] of the C reference
website:

> * any parameter of function type is adjusted to the corresponding pointer type
>
>   ```c
>   int f(char g(double)); // declares int f(char (*g)(double))
>   int h(int(void)); // declares int h(int (*)(void))
>   ```

Another such case is when function names are used in expressions. Quoted from
the ["Implicit conversions" section][creference:fun-decl] of the C reference
website:

> Any function designator expression, when used in any context other than ...
> undergoes a conversion to the non-lvalue pointer to the function designated by
> the expression.
>
> ```c
> int f(int);
> int (*p)(int) = f; // conversion to &f
> (***p)(1); // repeated dereference to f and conversion back to &f
> ```

Technically, only the latter case is called an "implicit conversion" by the C
reference website. Still, the former case is very similar in that a function
type is "converted" to the corresponding pointer type. Still, we now know that
it safe to use function pointer values in place of function values (the latter
is converted to the former), and it is safe to use function pointer types in
place of function types (the latter is convert/adjusted to the former).

Now for some examples. In the example below, the `f` argument of
`apply1_pointer` is a pointer argument, so no adjustments are needed. The `f`
argument of `apply1_nopointer` is a function type argument (we look through
typedefs), so we adjust its type to a pointer-to-function type. In the end, both
C functions get Haskell bindings with the exact same type.

```c
typedef int int2int(int);
extern int apply1_pointer_arg (int2int *, int);
extern int apply1_nopointer_arg (int2int, int);
```

```hs
newtype Int2int = Int2int { un_Int2int :: CInt -> IO CInt }
foreign import {-# details elided #-} apply1_pointer_arg
  :: FunPtr Int2int -> CInt -> IO CInt
foreign import {-# details elided #-} apply1_nopointer_arg
  :: FunPtr Int2int -> CInt -> IO CInt
```

Similarly, the address stubs `apply1_pointer_arg` and `apply1_nopointer_arg` that
we generate (see the ["Function pointers" section](#function-pointers)) would
get the exact same type as the other.

Note that parameters of function type can occur almost anywhere where types
normally can occur, with some minor restrictions. For example: variables,
function results, union fields and struct fields are not allowed to have a
function type. However, they can still have a *pointer-to-*function type. And
the pointed-to function type can have parameters of function type. So, for
variables, function results, union fields and struct fields, any parameter of
function type (we look through typedefs) is recursively adjusted to the
corresponding pointer type.

```c
extern int (* const apply1_nopointer_res (void)) (int2int, int);

extern int (* const apply1_nopointer_var) (int2int, int);

struct Apply1Struct {
  int (* const apply1_nopointer_struct_field)(int2int, int);
};
extern const struct Apply1Struct apply1_struct;

union Apply1Union {
  int (* const apply1_nopointer_union_field)(int2int, int);
};
extern const union Apply1Union apply1_union;
```

```hs
foreign import {-# details elided #-} apply1_nopointer_res
  :: IO (FunPtr (FunPtr Int2int -> CInt -> IO CInt))

apply1_nopointer_var :: FunPtr (
     FunPtr Int2int
  -> CInt
  -> IO CInt
  )

data Apply1Struct = Apply1Struct
  { apply1Struct_apply1_nopointer_struct_field ::
      FunPtr (
           FunPtr Int2int
        -> CInt
        -> IO CInt
        )
  }
apply1_struct :: Apply1Struct

newtype Apply1Union = Apply1Union
  { un_Apply1Union :: ByteArray
  }
get_apply1Union_apply1_nopointer_union_field ::
     Apply1Union
  -> FunPtr (
         FunPtr Int2int
      -> CInt
      -> IO CInt
      )
set_apply1Union_apply1_nopointer_union_field ::
     FunPtr (
           FunPtr Int2int
        -> CInt
        -> IO CInt
        )
  -> Apply1Union
apply1_union :: Apply1Union
```

[creference:fun-decl]: https://en.cppreference.com/w/c/language/function_declaration.html#Explanation

## Conversion between Haskell functions and C functions

Beyond generating type definitions for function pointers and handling implicit
conversions, `hs-bindgen` generates the additional FFI imports needed to
convert between Haskell functions and C function pointers in both directions.

### Auxiliary `_Deref` types

For each typedef function pointer type in the C API, `hs-bindgen` generates
two related types. Given:

```c
typedef void (*ProgressUpdate)(int percentComplete);
```

We generate:

```hs
newtype ProgressUpdate_Deref = ProgressUpdate_Deref
  { un_ProgressUpdate_Deref :: CInt -> IO ()
  }

newtype ProgressUpdate = ProgressUpdate
  { un_ProgressUpdate :: FunPtr ProgressUpdate_Deref
  }
```

The `_Deref` auxiliary type represents the Haskell function signature, while
the main type wraps the `FunPtr` to that signature. This separation mirrors
how C distinguishes between a function pointer and the function it points to.

### Wrapper and dynamic imports

For function pointer types that are actually used by the C API, `hs-bindgen`
generates both `"wrapper"` and `"dynamic"` foreign import stubs. These provide
bidirectional conversion between Haskell functions and C function pointers:

```hs
-- Create a C-callable function pointer from a Haskell function
foreign import ccall "wrapper" toProgressUpdate_Deref ::
     ProgressUpdate_Deref
  -> IO (FunPtr ProgressUpdate_Deref)

-- Convert a C function pointer back to a Haskell function
foreign import ccall "dynamic" fromProgressUpdate_Deref ::
     FunPtr ProgressUpdate_Deref
  -> ProgressUpdate_Deref
```

These stubs are abstracted over two type classes in order to offer a better
API to the end user. The following instances are also generated:

```hs
instance ToFunPtr ProgressUpdate_Deref where
  toFunPtr = toProgressUpdate_Deref

instance FromFunPtr ProgressUpdate_Deref where
  fromFunPtr = fromProgressUpdate_Deref
```

A function pointer will have a `ToFunPtr` and `FromFunPtr` instance if at
least one of its arguments contains at least one domain specific type. This
check is done recursively so higher order functions will be inspected
correctly.

For the purpose of instance generation, **domain-specific types** are types
defined in the generated bindings for the specific C library being bound,
such as:
- Structs and their fields
- Enums and typedefs
- Function pointer wrapper types

Conversely, **non-domain-specific types** are standard FFI types from GHC's
base libraries, such as `CInt`, `CDouble`, `Ptr a`, `IO ()`, etc.

For example:
- `ProgressUpdate_Deref` with type `CInt -> IO ()` **will** get instances
  because `ProgressUpdate_Deref` itself is domain-specific.
- A hypothetical function pointer type `CInt -> IO CInt` **will not** get
  instances because both `CInt` and `IO CInt` are non-domain-specific.

This distinction is important to avoid orphan instances and to prevent
generating multiple instances for the same type signature when binding
different C libraries.

#### Wrapping Haskell functions

To pass a Haskell function as a callback to C, use `toFunPtr` or the
`withToFunPtr` bracket combinator:

```hs
import HsBindgen.Runtime.FunPtr (withToFunPtr)

myCallback :: ProgressUpdate_Deref
myCallback = ProgressUpdate_Deref $ \progress ->
  putStrLn $ "Progress: " ++ show progress ++ "%"

-- Preferred: automatic cleanup with withToFunPtr
withToFunPtr myCallback $ \funPtr -> do
  onProgressChanged (ProgressUpdate funPtr)

-- Or manually manage the function pointer lifetime with bracket
bracket
  (toFunPtr myCallback)
  (freeHaskellFunPtr . un_ProgressUpdate)
  (\funPtr -> onProgressChanged (ProgressUpdate funPtr))
```

#### Unwrapping function pointers

To call a function pointer returned from C, use `fromFunPtr`:

```hs
do
  validatorFunPtr <- getValidator
  -- validatorFunPtr :: DataValidator

  -- Extract the FunPtr and convert to Haskell function
  let validator = fromFunPtr (un_DataValidator validatorFunPtr)
  result <- un_DataValidator_Deref validator 42
```

### Example: struct with function pointer fields

Function pointers frequently appear as struct fields for registering handlers:

```c
struct MeasurementHandler {
  void (*onReceived)(struct Measurement *data);
  int (*validate)(struct Measurement *data);
  void (*onError)(int errorCode);
};

void registerHandler(struct MeasurementHandler *handler);
```

In Haskell we can make use of the `ToFunPtr` to construct the
`MeasurementHandler` record.

```hs
alloca $ \handlerPtr -> do
  onReceivedPtr <- toFunPtr $ OnReceived_Deref $ \dataPtr -> do
    measurement <- peek dataPtr
    print measurement

  validatePtr <- toFunPtr $ Validate_Deref $ \dataPtr -> do
    -- validation logic
    return 1

  onErrorPtr <- toFunPtr $ OnError_Deref $ \errorCode ->
    putStrLn $ "Error: " ++ show errorCode

  poke handlerPtr $ MeasurementHandler
    { measurementHandler_onReceived = onReceivedPtr
    , measurementHandler_validate = validatePtr
    , measurementHandler_onError = onErrorPtr
    }

  registerHandler handlerPtr
```

## Userland CAPI

GHC's foreign function interface has limitations on which C functions can be
imported directly. For example, GHC cannot import functions that take or return
structs by value. To work around these limitations, `hs-bindgen` generates C
wrapper functions that can be imported by GHC, and then generates Haskell
bindings to these wrappers instead of to the original C functions.

This approach is similar to GHC's `capi` calling convention, which also
generates C wrappers to handle features that the FFI cannot express directly.
However, by generating these wrappers ourselves at the userland level, we can
extend the set of supported function signatures beyond what GHC's `capi`
provides. For instance, we can handle by-value struct arguments and return
values, which `capi` does not support.

The generated wrappers use hash-based names to prevent potential name
collisions. For example, a C function `print_point` might have a wrapper named
`hs_bindgen_test_example_a1b2c3d4e5f6g7h8`:

```c
// Generated C wrapper
void hs_bindgen_test_example_a1b2c3d4e5f6g7h8 ( struct point * arg1 ) {
  print_point ( arg1 );
}
```

```hs
-- Generated Haskell import
foreign import ccall "hs_bindgen_test_example_a1b2c3d4e5f6g7h8"
  print_point :: Ptr Point → IO ()
```

This hash-based naming ensures that even if multiple functions have similar
names or signatures, their wrappers will have unique names, avoiding linker
errors from symbol collisions.

This userland CAPI approach is used for all function imports in `hs-bindgen`,
not just those with features GHC cannot handle directly. This provides a
uniform interface and makes it straightforward to add support for additional
C features in the future.

### By-value `struct` arguments or return values

The GHC FFI does not support passing structs by value to or from C functions.
For example, consider:

```c
struct point byval ( struct point p );
```

This function cannot be imported directly. Instead, `hs-bindgen` generates a C
wrapper that accepts and returns structs by pointer, performing the necessary
conversions:

```c
void hs_bindgen_example_9a8b7c6d5e4f3210 ( struct point * arg
                                         , struct point * res ) {
  * res = byval (* arg );
}
```

This wrapper is then imported in Haskell:

```hs
foreign import ccall safe "hs_bindgen_example_9a8b7c6d5e4f3210"
    byval_wrapper :: Ptr Point → Ptr Point → IO ()
```

Finally, we generate a Haskell wrapper function that recovers the original
by-value semantics using `with`, `alloca`, and `peek`:

```hs
byval :: Point → IO Point
byval p =
  with p $ \ arg →
  alloca $ \ res → do
    byval_wrapper arg res
    peek res
```

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
