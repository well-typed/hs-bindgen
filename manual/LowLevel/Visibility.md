# Visibility

The visibility of a linker symbol, e.g., a function or variable name, determines
whether or not a linker symbol is visible outside of the shared object that it
is defined in. `hs-bindgen` takes visibility into account when generating
bindings for global variables and functions.

## Types of visibility

There are [4 types of visibility][fun-attributes]: **default**, **hidden**,
**internal**, and **protected**. We leave out descriptions for **internal** and
**protected** because they are rarely used in practice, and for our purposes of
binding generation we can treat them as **hidden** (they lead to linker errors
all the same).

* **Default:** The associated linker symbol is visible outside the shared object
  that it is defined in. In other words: the symbol is available to be linked
  against from outside the shared object that it is defined in.

  Despite the name, **default** always means public.

* **Hidden:**  The associated linker symbol is *not* visible outside the shared
  object that it is defined in. In other words: the symbol is *not* available to
  be linked against from outside the shared object that it is defined in.

[fun-attributes]: https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-visibility-function-attribute
[gcc-options]: https://gcc.gnu.org/onlinedocs/gcc/Code-Gen-Options.html#index-fvisibility
[clang-options]: https://clang.llvm.org/docs/ClangCommandLineReference.html#cmdoption-clang-fvisibility

### How is visibility determined

[Visibility attributes][fun-attributes] in C code affect visibility, but so do
[GCC options][gcc-options] and [Clang options][clang-options]. As such, it is
not sufficient to look at the syntax of the C code to determine visibility.
Currently we obtain the visibility of functions and variables (see the
`getCursorVisibility` function).

TODO: we currently use `clang` for parsing in `hs-bindgen` without an
`-fvisilibity` option. Should we? What if a shared library is compiled and
linked using `-fvisibility=hidden`, but we use clang in `hs-bindgen` with
`-fvisibility=default`? It sounds like that would be wrong.

## Guidelines for how visibility affects binding generation

The guideline for generating bindings to global variable declarations and
functions declarations in header files is to *only* do this if the visibility is
public. It is very likely that the generated Haskell binding would otherwise
fail to compile and link. Also, it's arguably a bug in the C shared library if a
header file contains declarations with non-public visibility.

### Why only public visibility?

Let's say we have a header file `A.h` that declares a function `foo` with
hidden visiblity:

```c
void __attribute__ ((visibility ("hidden"))) foo (void);
```

Let's also assume `A.c` includes a definition for this `foo` function. We can
compile this into a shared library `libA.so` like so:

```
❯ gcc -Wall -o A.o -c -fPIC A.c
❯ gcc -Wall --shared -o libA.so A.o
```

Now we create a separate file `B.c` that uses the `foo` function from
`libA.so`.

```c
#include "A.h"
void bar (void) { foo(); }
```

If we try to compile the following code and link it against `libA.so`, we
would get linker errors.

```
❯ gcc -Wall -o B.o -c -fPIC B.c
❯ gcc -Wall --shared -o libB.so B.o libA.so
/usr/bin/ld: B.o: in function `bar':
B.c:(.text+0x9): undefined reference to `foo'
/usr/bin/ld: libB.so: hidden symbol `foo' isn't defined
/usr/bin/ld: final link failed: bad value
collect2: error: ld returned 1 exit status
```

Even if a definition for `foo` is included in `A.c`, this definition is not
visible outside outside of the shared object `libA.so`, so `libB.so` can not use
it! Since we included the header `A.h` in `B.c`, `B.c` will roughly look like
this after preprocessing:

```c
void __attribute__ ((visibility ("hidden"))) foo (void); // included from A.h
void bar (void) { foo(); }
```

The only way to make this code compile and link is to define `foo` ourselves,
but that seems silly. Similar errors would occur if `foo` had protected or internal
visibility. Hence, we treat declarations with protected or internal visibility
as if it had hidden visibility.

To conclude, it does not make much sense at all to put non-public functions in
header files, because header files should typically expose the public interface.
Putting a hidden declaration in a header file of a shared library basically
says: "I'm exposing a function name from my library but I'm not letting you use
the definition".

#### What about global variables?

If we change the example from the previous section slightly and instead put a
non-public, extern global variable `foo` in `A.h` with a definition (let's say
`int foo = 17;`) in `A.c`.

```c
int __attribute__ ((visibility ("hidden"))) foo;
```

And we refer to `foo` from `B.c`:

```c
#include "A.h"
int bar (void) { return foo; }
```

Then no linker errors occur, but the behaviour of the program is weird. `bar`
would return `0` instead of `17`. The value of `foo` is still not exposed from
`A`, so `B` instead defaults its own local version of `foo` to the value `0`.

For uniformity and to prevent surprises, we take the same approach for global
variables as for functions, even though the non-public global variable case
successfully compiles and links.

### Static linking

TODO: if we were to create a /static/ library out of `A`, then the visibiltity
doesn't really lead to errors when `A` and `B` are linked together, because `A`
and `B` are then in the /same shared object/. Rather, the visibility nows
determine which symbols are exposed from the shared object that includes both
`A` and `B`. We currently have not experiment much with statically linking
Haskell bindings with static C libraries (i.e., archives), but when we do we
might have to revisit the guidelines outlined in this document.
