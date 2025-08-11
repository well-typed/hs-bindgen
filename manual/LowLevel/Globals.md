# Global variables

Consider

```c
struct globalConfig {
  int numThreads;
  int numWorkers;
};

extern struct globalConfig globalConfig;

void printGlobalConfig();
```

This results in

```hs
foreign import {-# details elided #-}
  globalConfig_ptr :: Ptr GlobalConfig
```

along with the usual definitions for structs and functions:

```hs
data GlobalConfig = GlobalConfig {
      globalConfig_numThreads :: CInt
    , globalConfig_numWorkers :: CInt
    }
  deriving stock (Eq, Show)

instance Storable GlobalConfig where (..)

foreign import (..) printGlobalConfig :: IO ()
```

We can use these bindings as follows:

```hs
do config <- peek globalConfig_ptr
   print config      -- Print it Haskell side
   poke globalConfig_ptr $ config{globalConfig_numThreads = 3}
   printGlobalConfig -- Print it C side
```

## Non-extern globals

Some headers define globals without declaring them to be `extern`, for example

```c
int nonExternGlobalInt = 8;
```

The code `hs-bindgen` produces for this is the same as for extern globals.

```hs
foreign import {-# details elided #-}
  nonExternGlobalInt_ptr :: Ptr CInt
```

However, such headers need to be treated with caution: if they are included more
than once, the resulting binary will have duplicate symbols, resulting in linker
errors ("multiple definition of `nonExternGlobalInt`").

## Unsupported: thread-local variables

We currently do not generate bindings for global variables that are marked
`thread_local` ([#828](https://github.com/well-typed/hs-bindgen/issues/828)):

```c
thread_local extern int threadLocal;
```

Taking a pointer of such a variable may not be safe, and we should generate a
getter and setter instead. Such "global thread-local variables" should be
exceedingly rare.

## Invalid examples

Since `extern` merely declares the existence of a global variable, there must be
an actual definition of it in some C file. Consequently, anonymous declarations
inside `extern`s are unusable (as the corresponding definition of the global in
the C file would be unable to give it a value of the _same_ struct). Therefore

```c
extern struct {
  int numThreads;
  int numWorkers;
} globalConfig;
```

will result in a warning, and not produce any bindings.

## Constants

Global variables can also represenent global *constants* if they use
`const`-qualified types.

```c
extern const int globalConstant;
```

The code `hs-bindgen` produces for this is the same as for non-constant global variables,

```hs
foreign import {-# details elided #-}
  globalConstant_ptr :: Ptr CInt
```

but `hs-bindgen` also produces a utility function that produces the value of the
constant directly, rather than a pointer to the value.

```hs
{-# NOINLINE globalConstant #-}
globalConstant :: CInt
globalConstant = {-# details elided #-}
```

We've also added a `NOINLINE` pragma so that the `globalConstant` is evaluated
only once.

## Guidelines for binding generation

The approach to generating foreign imports for global variables is as follows:

* The foreign import should return a pointer to the location in memory where the
  global is stored, rather than the value of the global itself, so that we can
  modify the value at that location.

* But first, we generate a stub C function that returns the address of the
  global variable. This stub is necessary to prevent linker errors on Windows.
  For more information about the error, see [issue #898][issue-898] and [PR
  #927][pr-927].

* Then, we create a foreign import of that stub C function.

* If the type of the global variable is `const`-qualified, then it is actually a
  global *constant*. If there is a `Storable` instance in scope for the variable
  type, we generate a *pure* Haskell function that returns the value of the
  global constant, using a combination of `unsafePerformIO` and `peek` on the
  variable pointer. This is safe, since the value of the global constant should
  not change.

[issue-898]:https://github.com/well-typed/hs-bindgen/issues/898
[pr-927]:https://github.com/well-typed/hs-bindgen/pull/927

We include examples of generated bindings for a variety of types below.

### Simple value: `int`

Global:
```c
int x;
```

Stub:
```c
// The x_ptr_c indirection is just here to give the pointer a name that we
// can refer to in the memory layout. It is not included in the stub that we
// actually generate
__attribute__ ((const)) int* get_x_ptr(void) {
    int *x_ptr_c = &x;
    return x_ptr_c;
}
```

Import:
```hs
foreign import ccall safe "get_x_ptr" x_ptr :: Ptr CInt
```

Memory layout:

| type                     | name          | address | value   |
| ------------------------ | ------------- | ------- | ------- |
| int                      | x             | 1000    | 17      |
|                          |               | ...     |         |
| int*                     | x_ptr_c       | 2000    | 1000    |
|                          |               | ...     |         |
| Ptr CInt                 | x_ptr         | 3000    | 1000    |

Constant:
```hs
{-# NOINLINE x #-}
-- If the type of the global were @const int x@, we would also generate the following
x :: CInt
x = unsafePerformIO $ peek x_ptr
```

### Pointer value: `int*`

There is some ambiguity here: does `int*` point to a single `int`, or a sequence
of `int`s? We can not detect this, so we treat it as if it points to a single
`int` and generate a binding accordingly. From Haskell, one can still use the
generated binding to the pointer as if it were pointing to a sequence of `int`s.
Of course, this is only safe if the user knows that it is pointing to a sequence
of `int`s.

Global:
```c
int* x;
```

Stub:
```c
// The x_ptr_c indirection is just here to give the pointer a name that we
// can refer to in the memory layout. It is not included in the stub that we
// actually generate
int** get_x() {
  int** x_ptr_c = &x;
  return x_ptr_c;
}
```

Import:
```hs
foreign import ccall safe "get_x_ptr" x_ptr :: Ptr (Ptr CInt)

-- The code below is not included in the generated bindings.
-- It should be included by a user of the bindings if they want
-- to use array utilities provided by @hs-bindgen-runtime@.

x_constant_array_ptr :: IO (Ptr (ConstantArray 3 CInt))
x_constant_array_ptr = ConstantArray.isConstantArray (Proxy @3) <$> peek x_ptr

x_incomplete_array_ptr :: IO (Ptr (IncompleteArray CInt))
x_incomplete_array_ptr = IncompleteArray.isIncompleteArray <$> peek x_ptr
```

Memory layout:

| type                     | name          | address | value   |
| ------------------------ | ------------- | ------- | ------- |
| int                      |               | 1000    | 1       |
|                          |               | 1004    | 2       |
|                          |               | 1008    | 3       |
|                          |               | ...     |         |
| int*                     | x             | 2000    | 1000    |
|                          |               | ...     |         |
| int**                    | x_ptr_c       | 3000    | 2000    |
|                          |               | ...     |         |
| Ptr (Ptr CInt)           | x_ptr         | 4000    | 2000    |

Constant:
```hs
{-# NOINLINE x #-}
-- If the type of the global were @int * const x@, we would also generate the following.
-- Note that we do this for const-pointer-to-int, not for pointer-to-const-int. The "outer"
-- type should be const-qualified, which is the pointer in this case, not the int.
x :: Ptr CInt
x = unsafePerformIO $ peek x_ptr
```

### Arrays of known size: `int[3]`

We have a subtle choice here of what to generate a binding for. The options are:

* Generate a binding to the pointer to the first element of the array. This
  emphasises individual elements.

* Generate a binding to the pointer to the whole of the array. This emphasises
  the array as a whole.

For uniformity, we use the latter option.

Note that the *value* of the pointer is the same regardless of which approach we
pick. A pointer to the first element of the array points to the start of the
array, and a pointer to the array as a whole *also* points to the start of the
array. The difference is only in the type of the pointer. As such, a user of the
generated bindings can safely cast the pointer to the whole array to a pointer
to the first element of the array.

Global:
```c
typedef int triplet[3];
triplet x;
```

Stub:
```c
// The x_ptr_c indirection is just here to give the pointer a name that we
// can refer to in the memory layout. It is not included in the stub that we
// actually generate
__attribute__ ((const)) triplet *get_x_ptr(void) {
  x_ptr_c = &x;
  return x_ptr_c;
}
```

Import:
```hs
newtype Triplet = Triplet (ConstantArray 3 CInt)
foreign import ccall safe "get_x_ptr" x_ptr :: Ptr Triplet

-- The code below is not included in the generated bindings.
-- It should be included by a user of the bindings if they want
-- to use the array pointer as an array element pointer instead.

x_elem_ptr :: Ptr CInt
x_elem_ptr = snd $ ConstantArray.isFirstElem x_ptr
```

Memory layout:

| type                       | name          | address | value   |
| -------------------------- | ------------- | ------- | ------- |
| int[3]                     | x             | 1000    | 1       |
|                            |               | 1004    | 2       |
|                            |               | 1008    | 3       |
|                            |               | ...     |         |
| (*int)[3]                  | x_ptr_c       | 2000    | 1000    |
|                            |               | ...     |         |
| Ptr (ConstantArray 3 CInt) | x_ptr         | 3000    | 1000    |

Constant:
```hs
{-# NOINLINE x #-}
-- If the type of the global were @const triplet x@, we would also generate the following
x :: Triplet
x = unsafePerformIO $ peek x_ptr
```

# Arrays of unknown size: `int[]`

The approach is here is exactly the same as for arrays of known size. The
difference is only in the types: we use `IncompleteArray` instead of
`ConstantArray`.

Global:
```c
typedef int list[];
list x;
```

Stub:
```c
// The x_ptr_c indirection is just here to give the pointer a name that we
// can refer to in the memory layout. It is not included in the stub that we
// actually generate
__attribute__ ((const)) list *get_x_ptr(void) {
  x_ptr_c = &x;
  return x_ptr_c;
}
```

Import:
```hs
newtype List = List (IncompleteArray CInt)
foreign import ccall safe "get_x_ptr" x_ptr :: Ptr List

-- The code below is not included in the generated bindings.
-- It should be included by a user of the bindings if they want
-- to use the array pointer as an array element pointer instead.

x_elem_ptr :: Ptr CInt
x_elem_ptr = IncompleteArray.isFirstElem x_ptr
```

Memory layout:

| type                       | name          | address | value   |
| -------------------------- | ------------- | ------- | ------- |
| int[]                      | x             | 1000    | 1       |
|                            |               | 1004    | 2       |
|                            |               | 1008    | 3       |
|                            |               | ...     |         |
| (*int)[]                   | x_ptr_c       | 2000    | 1000    |
|                            |               | ...     |         |
| Ptr (IncompleteArray CInt) | x_ptr         | 3000    | 1000    |

Constant:
```hs
{-# NOINLINE x #-}
-- If the type of the global were @const list x@, we would /not/ generate the following.
-- It would fail to compile because 'IncompleteArray' does not have a 'Storable' instance,
-- and therefore neither does the 'List' newtype.
x :: List
x = unsafePerformIO $ peek x_ptr -- ERROR
```
