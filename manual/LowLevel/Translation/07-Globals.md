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

`hs-bindgen` translates the C code above roughly into

```haskell
foreign import {-# details elided #-}
  hs_bindgen_e5d5a0f4ce4b2b08 :: IO (Ptr GlobalConfig)

{-# NOINLINE globalConfig #-}
globalConfig :: Ptr GlobalConfig
globalConfig = unsafePerformIO hs_bindgen_e5d5a0f4ce4b2b08
```

where `hs_bindgen_*` is a randomly generated name. The `NOINLINE` pragma is
there to ensure that the foreign import is evaluated only once. The usual
definitions for `struct`s and functions are also generated, including:

```haskell
data GlobalConfig = GlobalConfig {
      globalConfig_numThreads :: CInt
    , globalConfig_numWorkers :: CInt
    }
  deriving stock (Eq, Show)

instance Storable GlobalConfig where {-# details elided #-}
```

We can use these bindings as follows:

```haskell
do config <- peek Globals.globalConfig
   print config
   poke Globals.globalConfig $
     config{Globals.globalConfig_numThreads = 3}
   config' <- peek Globals.globalConfig
   print config'
```

## Non-extern globals

Some headers define globals directly, instead of declaring them to be `extern`
and putting the definition in a `.c` file. For example:

```c
int nonExternGlobalInt = 8;
```

The code `hs-bindgen` produces for this is the same as for extern globals.

```haskell
foreign import {-# details elided #-}
  hs_bindgen_f034badbc299f27b :: IO (Ptr CInt)

{-# NOINLINE nonExternGlobalInt #-}
nonExternGlobalInt :: Ptr CInt
nonExternGlobalInt = unsafePerformIO hs_bindgen_f034badbc299f27b
```

In fact, the storage class specifier does not influence the bindings we
generate. For example, if a `static` specifier had been used, the resulting
Haskell bindings would have been the same.

We can use these bindings as follows:

```haskell
do print =<< peek Globals.nonExternGlobalInt
```

However, such headers need to be treated with caution: if they are included more
than once, the resulting binary will have duplicate symbols, resulting in linker
errors ("multiple definition of `nonExternGlobalInt`"). `hs-bindgen` diagnoses
such cases and warns the user, but still generates the code. To prevent such
warnings (and linker errors), an `extern` or `static` storage class specifier
should be used. In the former case, the definition, e.g., `= 8`, should also be
moved outside of the header file.

## Unsupported: thread-local variables

We currently do not generate bindings for global variables that are marked
`thread_local` ([#828](https://github.com/well-typed/hs-bindgen/issues/828)):

```c
thread_local extern int threadLocal;
```

Taking a pointer of such a variable may not be safe, and we should generate a
getter and setter instead. "Global thread-local variables" are rare.

## Anonymous declarations

For most `extern` variables, the definition of the variable lives outside of the
header file in a `.c` file. Consequently, anonymous declarations inside `extern`s
are unusable (as the corresponding definition of the global in the C file would
be unable to give it a value of the _same_ struct). Therefore

```c
extern struct {
  int x;
  int y;
} unusableAnon;
```

will result in a warning, and not produce any bindings.

## Constants

Global variables can also represent global *constants* if they use
`const`-qualified types. `hs-bindgen` can diagnose when types are
`const`-qualified even in the presence of `typedefs`. Global constants are not
affected by storage-class specifiers -- the generated Haskell bindings are the
same regardless.

```c
extern const int globalConstant;
typedef const int ConstInt;
static ConstInt anotherGlobalConstant = 123;
```

`hs-bindgen` uses `ConstPtr` instead of `Ptr` for these non-constant global
variables. Moreover, if a constant points to a value with a `Storable` instance,
we generate additional utility functions that return the value of the constants
directly:

```haskell
foreign import {-# details elided #-}
  hs_bindgen_c83c3e4e014bf39c :: IO (ConstPtr CInt)

{-# NOINLINE hs_bindgen_4b7fabc21bc52057 #-}
hs_bindgen_4b7fabc21bc52057 :: ConstPtr CInt
hs_bindgen_4b7fabc21bc52057= unsafePerformIO hs_bindgen_c83c3e4e014bf39c

{-# NOINLINE globalConstant #-}
globalConstant :: CInt
globalConstant = {-# details elided #-}

newtype ConstInt = ConstInt { un_ConstInt :: CInt }

foreign import {-# details elided #-}
  hs_bindgen_2d6b9a52b97910a9 :: IO (ConstPtr ConstInt)

{-# NOINLINE hs_bindgen_fad79735fe298c1f #-}
hs_bindgen_fad79735fe298c1f :: ConstPtr ConstInt
hs_bindgen_fad79735fe298c1f = unsafePerformIO hs_bindgen_2d6b9a52b97910a9

{-# NOINLINE anotherGlobalConstant #-}
anotherGlobalConstant :: ConstInt
anotherGlobalConstant = {-# details elided #-}
```

Global constants can not be mutated in C unless unsafe casts are used, hence it
is safe to bind the pointed-to-value. The mangled name of the `ConstPtr`
encourages usage of the values directly binding the pointed-to-value and
indicates the `ConstPtr` being internal. However, nothing prevents a user from
`poke`-ing to a `ConstPtr` in Haskell land, but it being a `ConstPtr` should be
a hint to the user not to do this.

We've also add `NOINLINE` pragmas so that `globalConstant` and
`anotherGlobalConstant` are evaluated only once.

We can then use the generated bindings as follows:

```haskell
do print Globals.globalConstant
   print Globals.anotherGlobalConstant
```

### Constant examples

For the following examples

```c
//! An array of known size of const ints
extern const int constArray1 [4];
//! An array of unknown size of const insts
extern const int constArray2 [];

struct tuple { int x; const int y; };
//! A constant tuple
extern const struct tuple constTuple;
//! A non-constant tuple with a constant member
extern struct tuple nonConstTuple;

//! An int
extern int Int;
//! A const int
extern const int constInt;
//! A pointer to int
extern int * ptrToInt;
//! A pointer to const int
extern const int * ptrToConstInt;
//! A const pointer to int
extern int * const constPtrToInt;
//! A const pointer to const int
extern const int * const constPtrToConstInt;
```

`hs-bindgen` will roughly generate the following Haskell code, where most
details except for type signatures are elided:

```haskell
data Tuple = Tuple
  { tuple_x :: CInt
  , tuple_y :: CInt
  }
  deriving stock (Eq, Show)

instance Storable Tuple where {-# details elided #-}

foreign import ccall unsafe "hs_bindgen_a804e6470cde45c2" hs_bindgen_a804e6470cde45c2 ::
     IO (ConstPtr ((ConstantArray 4) CInt))
hs_bindgen_6acdd0466d14ab3f :: ConstPtr ((ConstantArray 4) CInt)
constArray1 :: (ConstantArray 4) CInt

foreign import ccall unsafe "hs_bindgen_3cd4fc49a6bb5840" hs_bindgen_3cd4fc49a6bb5840 ::
     IO (ConstPtr (IncompleteArray CInt))
constArray2 :: ConstPtr (IncompleteArray CInt)

foreign import ccall unsafe "hs_bindgen_8c3024ef7f2b0594" hs_bindgen_8c3024ef7f2b0594 ::
     IO (ConstPtr Tuple)
hs_bindgen_c9a3292a299edcd8 :: ConstPtr Tuple
constTuple :: Tuple

foreign import ccall unsafe "hs_bindgen_e1200a75ed20a2d2" hs_bindgen_e1200a75ed20a2d2 ::
     IO (Ptr Tuple)
nonConstTuple :: Ptr Tuple

foreign import ccall unsafe "hs_bindgen_1d4f0442a6f47a9a" hs_bindgen_1d4f0442a6f47a9a ::
     IO (Ptr CInt)
int :: Ptr CInt

foreign import ccall unsafe "hs_bindgen_188ef9ca039f4abc" hs_bindgen_188ef9ca039f4abc ::
     IO (ConstPtr CInt)
hs_bindgen_12ed4d294c0aeb5f :: ConstPtr CInt
constInt :: CInt

foreign import ccall unsafe "hs_bindgen_1fb9e392279def5a" hs_bindgen_1fb9e392279def5a ::
     IO (Ptr (Ptr CInt))
ptrToInt :: Ptr (Ptr CInt)

foreign import ccall unsafe "hs_bindgen_4003d50d5f510514" hs_bindgen_4003d50d5f510514 ::
     IO (Ptr (ConstPtr CInt))
ptrToConstInt :: Ptr (ConstPtr CInt)

foreign import ccall unsafe "hs_bindgen_c3df48685426f621" hs_bindgen_c3df48685426f621 ::
     IO (ConstPtr (Ptr CInt))
hs_bindgen_ce37f0f07507c808 :: ConstPtr (Ptr CInt)
constPtrToInt :: Ptr CInt

foreign import ccall unsafe "hs_bindgen_7a4dc03eb19059c3" hs_bindgen_7a4dc03eb19059c3 ::
     IO (ConstPtr (ConstPtr CInt))
hs_bindgen_4800a04940b1c189 :: ConstPtr (ConstPtr CInt)
constPtrToConstInt :: ConstPtr CInt
```

And we can use these bindings like so:

```haskell
do print Globals.constArray1
   print =<< IA.peekArray 5 Globals.constArray2.unConstPtr
   print Globals.constTuple
   print =<< F.peek Globals.nonConstTuple
   print =<< F.peek Globals.int
   print Globals.constInt
   print =<< F.peek Globals.ptrToInt
   print =<< F.peek Globals.ptrToConstInt
   print Globals.constPtrToInt
   print Globals.constPtrToConstInt
```

## Guidelines for binding generation

The approach to generating foreign imports for global variables is as follows:

* The foreign import should return a pointer to the location in memory where the
  global is stored, rather than the value of the global itself, so that we can
  modify the value at that location.

* But first, we generate a stub C function that returns the address of the
  global variable. This stub is necessary to prevent linker errors on Windows.
  For more information about the error, see [issue #898][issue-898] and [PR
  #927][pr-927]. The stub is given a unique, mangled name to prevent duplicate
  symbols.

* Then, we create a foreign import of that stub C function, returning a pointer
  in `IO`. This foreign import is marked unsafe, since there is no possibility
  of callbacks into Haskell code. The name of the foreign import is the same as
  the mangled name of the stub C function. The import is meant to be internal,
  so we do not have to generate a descriptive name, and the mangled name
  prevents name clashes with other Haskell identifiers.

* Then, we create a pure Haskell function that safely unsafely performs the `IO`
  and returns the pointer.

* If the type of the global variable is `const`-qualified, then it is actually a
  global *constant*. We use `ConstPtr` rather than `Ptr`. Then, if there is a
  `Storable` instance in scope for the variable type, we generate a *pure*
  Haskell function that returns the value of the global constant, using a
  combination of `unsafePerformIO` and `peek` on the variable pointer. This is
  safe, since the value of the global constant should not change. In this last
  case, we also mangle the name of the variable pointer, to indicate it is
  internal and encourage the use of the *pure* value.

[issue-898]:https://github.com/well-typed/hs-bindgen/issues/898
[pr-927]:https://github.com/well-typed/hs-bindgen/pull/927

We include (simplified) examples of generated bindings for a variety of types
below.

### Simple value: `int`

Global:

```c
extern int a;
```

Stub:

```c
/* get_a */ __attribute__ ((const)) int* fe8f4js8(void) { return &a; }
```

Import:

```haskell
foreign import ccall unsafe "fe8f4js8" fe8f4js8 :: IO (Ptr CInt)

{-# NOINLINE a #-}
a :: Ptr CInt
a = unsafePerformIO fe8f4js8
```

Memory layout:
| type              | C name | Haskell name | address | value |
|-------------------|--------|--------------|---------|-------|
| `int`             | a      |              | 1000    | 17    |
|                   |        |              | ...     |       |
| `int* ; Ptr CInt` |        | a            | 2000    | 1000  |

Constant:
```haskell
-- If the type of the global had been const-qualified instead, like @const int
-- a2@, we instead generate the following
{-# NOINLINE a2 #-}
hs_bindgen_3c1f548cbbd9c65b :: ConstPtr CInt
a2 :: CInt
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
extern int * b;
```

Stub:

```c
/* get_b */ int** ae8fae8() { return &b; }
```

Import:

```haskell
foreign import ccall unsafe "ae8fae8" ae8fae8 :: IO (Ptr (Ptr CInt))

{-# NOINLINE b #-}
b :: Ptr (Ptr CInt)
b = unsafePerformIO ae8fae8

-- The code below is not included in the generated bindings.
-- It should be included by a user of the bindings if they want
-- to use array utilities provided by @hs-bindgen-runtime@.

b_constant_array :: IO (Ptr (ConstantArray 3 CInt))
b_constant_array = toConstantArrayPtr (Proxy @3) <$> peek b

b_incomplete_array :: IO (Ptr (IncompleteArray CInt))
b_incomplete_array = toIncompleteArrayPtr <$> peek b
```

Memory layout:

| type                    | C name | Haskell name | address | value |
|-------------------------|--------|--------------|---------|-------|
| `int`                   |        |              | 1000    | 1     |
|                         |        |              | 1004    | 2     |
|                         |        |              | 1008    | 3     |
|                         |        |              | ...     |       |
| `int*`                  | b      |              | 2000    | 1000  |
|                         |        |              | ...     |       |
| `int**; Ptr (Ptr CInt)` |        | b            | 3000    | 2000  |

Constant:

```haskell
-- If the type of the global had been const-qualified instead, like @const int
-- * b2@, we would also generate the following. Note that we do this for
-- const-pointer-to-int, not for pointer-to-const-int. The "outer" type should
-- be const-qualified, which is the pointer in this case, not the int.
{-# NOINLINE b2 #-}
b2 :: Ptr CInt
```

### Arrays of known size: `int[3]`

We have a subtle choice here of what to generate a binding for. The options are:

* Generate a binding to the pointer to the first element of the array. This
  emphasises individual elements.

* Generate a binding to the pointer to the whole of the array. This emphasises
  the array as a whole.

For uniformity, we use the latter option.

Note that the _value_ of the pointer is the same regardless of which approach we
pick. A pointer to the first element of the array points to the start of the
array, and a pointer to the array as a whole _also_ points to the start of the
array. The difference is only in the type of the pointer. As such, a user of the
generated bindings can safely cast the pointer to the whole array to a pointer
to the first element of the array.

Global:

```c
typedef int triplet[3];
extern triplet c;
```

Stub:

```c
/* get_c */ __attribute__ ((const)) triplet *f94u3030(void) { return &c; }
```

Import:

```haskell
newtype Triplet = Triplet (ConstantArray 3 CInt)
foreign import ccall unsafe "f94u3030" f94u3030 :: IO (Ptr Triplet)

{-# NOINLINE c #-}
c :: Ptr Triplet
c = unsafePerformIO f94u3030

-- The code below is not included in the generated bindings.
-- It should be included by a user of the bindings if they want
-- to use the array pointer as an array element pointer instead.

c_elem :: Ptr CInt
c_elem = snd $ toFirstElemPtr c
```

Memory layout:

| type                                     | C name | Haskell name | address | value |
|------------------------------------------|--------|--------------|---------|-------|
| `int[3]`                                 | c      |              | 1000    | 1     |
|                                          |        |              | 1004    | 2     |
|                                          |        |              | 1008    | 3     |
|                                          |        |              | ...     |       |
| `(*int)[3] ; Ptr (ConstantArray 3 CInt)` |        | c            | 2000    | 1000  |

Constant:

```haskell
-- If the type of the global had been const-qualified instead, like @const
-- triplet c2@, we would also generate the following
{-# NOINLINE c2 #-}
c2 :: Triplet
```

# Arrays of unknown size: `int[]`

The approach is here is exactly the same as for arrays of known size. The
difference is only in the types: we use `IncompleteArray` instead of
`ConstantArray`.

Global:

```c
typedef int list[];
extern list d;
```

Stub:

```c
/* get_d */ __attribute__ ((const)) list *poeyrb8a(void) { return &d; }
```

Import:

```haskell
newtype List = List (IncompleteArray CInt)
foreign import ccall unsafe "poeyrb8a" poeyrb8a :: IO (Ptr List)

{-# NOINLINE d #-}
d :: Ptr List
d = unsafePerformIO poeyrb8a

-- The code below is not included in the generated bindings.
-- It should be included by a user of the bindings if they want
-- to use the array pointer as an array element pointer instead.

d_elem :: Ptr CInt
d_elem = toFirstElemPtr d
```

Memory layout:

| type                                    | C name | Haskell name | address | value |
|-----------------------------------------|--------|--------------|---------|-------|
| `int[]`                                 | d      |              | 1000    | 1     |
|                                         |        |              | 1004    | 2     |
|                                         |        |              | 1008    | 3     |
|                                         |        |              | ...     |       |
| `(*int)[] ; Ptr (IncompleteArray CInt)` |        | d            | 2000    | 1000  |

Constant:

```haskell
-- If the type of the global had been const-qualified instead, like @const list
-- d2@, we would /not/ generate the following. It would fail to compile because
-- 'IncompleteArray' does not have a 'Storable' instance, and therefore neither
-- does the 'List' newtype.
{-# NOINLINE 2 #-}
d2 :: List -- ERROR
```
