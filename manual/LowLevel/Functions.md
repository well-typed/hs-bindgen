# Functions

## Introduction

TODO

## Function pointers

TODO: introduction to function pointers

1. For every C function, generate an additional binding for the address of that C function.

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
square_ptr :: F.FunPtr (FC.CInt -> IO FC.CInt)

foreign import {-# details elided #-} apply
  :: F.FunPtr (FC.CInt -> IO FC.CInt) -> FC.CInt -> IO FC.CInt

-- This is a wrapper around an internal foreign import. Details are elided.
apply_ptr :: F.FunPtr ((F.FunPtr (FC.CInt -> IO FC.CInt)) -> FC.CInt -> IO FC.CInt)
```

A binding to the address of a C function is generated using a stub, just like we
do for bindings to global variables. See the [Globals][globals] section of the
manual for more information about stubs.

Note that we've also generated `apply_ptr`, even though we did not need it in
this case. `hs-bindgen` generates address stubs for *all* function declarations.

We might use these bindings as follows:

```hs
main = do
  y <- apply square_ptr 4
  print y -- prints 16
```

[globals]:./Globals.md#Guidelines-for-binding-generation

## Implicit function to pointer conversion

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
argument of `apply1_nopointer` is a function type argument, so we adjust its
type to a pointer-to-function type. In the end, both C functions get Haskell
bindings with the exact same type.

```c
extern int apply1_pointer_arg (int (*f)(int), int x);
extern int apply1_nopointer_arg (int f (int), int x);
```
```hs
foreign import {-# details elided #-} apply1_pointer_arg
  :: FunPtr (CInt -> IO CInt) -> CInt -> IO CInt
foreign import {-# details elided #-} apply1_nopointer_arg
  :: FunPtr (CInt -> IO CInt) -> CInt -> IO CInt
```

Similarly, the address stubs `apply1_pointer_arg_ptr` and `apply1_nopointer_arg_ptr` that
we generate (see the ["Function pointers" section](#function-pointers)) would
get the exact same type as the other.

Note that parameters of function type can occur almost anywhere where types
normally can occur, with some minor restrictions. For example: variables,
function results, union fields and struct fields are not allowed to have a
function type. However, they can still have a *pointer-to-*function type. And
the pointed-to function type can have parameters of function type. So, for
variables, function results, union fields and struct fields, any parameter of
function type is recursively adjusted to the corresponding pointer type.

```c
extern int (* const apply1_nopointer_res (void)) (int (int), int);
.
extern int (* const apply1_nopointer_var) (int (int), int);

struct Apply1Struct {
  int (* const apply1_nopointer_struct_field)(int (int), int);
};
extern const struct Apply1Struct apply1_struct;

union Apply1Union {
  int (* const apply1_nopointer_union_field)(int (int), int);
};
extern const union Apply1Union apply1_union;
```

```hs
apply1_nopointer_var :: FunPtr (
     (FunPtr (CInt -> IO CInt))
  -> CInt
  -> IO CInt
  )

data Apply1Struct = Apply1Struct
  { apply1Struct_apply1_nopointer_struct_field ::
      FunPtr (
           (FunPtr (CInt -> IO CInt))
        -> CInt
        -> IO CInt
        )
  }
apply1_struct :: Apply1Struct

newtype Apply1Union = Apply1Union
  { un_Apply1Union :: Data.Array.Byte.ByteArray
  }
get_apply1Union_apply1_nopointer_union_field ::
     Apply1Union
  -> FunPtr (
         (FunPtr (CInt -> IO CInt))
      -> CInt
      -> IO CInt
      )
set_apply1Union_apply1_nopointer_union_field ::
     FunPtr (
           (FunPtr (CInt -> IO CInt))
        -> CInt
        -> IO CInt
        )
  -> Apply1Union
apply1_union :: Apply1Union
```

[creference:fun-decl]: https://en.cppreference.com/w/c/language/function_declaration.html#Explanation
[creference:fun-ptr-conv]: https://en.cppreference.com/w/c/language/conversion.html#Function_to_pointer_conversion

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
