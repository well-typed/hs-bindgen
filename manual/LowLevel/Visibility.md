# Visibility

In the scope of this document, by linker symbols (or symbolf or short) we mean
the names of function and global variable declarations.

The *linkage* of a symbol determines whether or not a symbol is visible outside
the translation unit it is defined in during linking. In other words: the symbol
is available to be linked against from outside the translation unit that it is
defined in.

The *visibility* of a symbol determines whether or not a symbol is visible
outside of the shared object that it is defined in during linking. In other
words: the symbol is available to be linked against from outside the shared
object that it is defined in.

`hs-bindgen` takes linkage and visibility into account when generating bindings
for global variables and functions. Linker errors are likely for some
combinations of linkage and visibility and so we forego generating bindings in
those cases, eventhough it is likely a bug in the C library and thus should be
fixed in the C library, not in `hs-bindgen`.

## Types of linkage

There are [3 types of linkage][linkage]: **external**,
**internal**, or **none**.

* **External**: the symbol is visibile outside the translation unit it's defined in.

* **Internal**: the symbol is *not* visibile outside the translation unit it's
  defined in.

* **None**: only applies to local objecs, such as local variables and function
  parameters.

Since we only generate bindings for top-level declarations, we will only
consider **external** and **internal** linkage from now on.

**External** and **internal** linkage can be implemented using the `extern` and
`static` keywords respectively, but there are other factors that determine the
linkage of declarations.

[linkage]: https://en.cppreference.com/w/cpp/language/storage_duration.html#Linkage

## Types of visibility

There are [4 types of visibility][visibility]: **default**, **hidden**,
**internal**, and **protected**. We leave out descriptions for **internal** and
**protected** because they are rarely used in practice, and for our purposes of
binding generation we can treat them as **hidden** (they lead to linker errors
in all the same way). We will call **hidden**, **internal** and **protected**:
**non-public** visibility.

* **Default:** The symbol is visible outside the shared object that it is
  defined in. Despite the name, **default** always means public.

* **Hidden:**  The symbol is *not* visible outside the shared object that it is
  defined in.

[Visibility attributes][visibility] in C code affect visibility, but so do [GCC
options][gcc-options] and [Clang options][clang-options]. As such, it is
technically not sufficient to look at the syntax of the C code to determine
visibility. Currently we obtain the visibility of functions and variables (see
the `getCursorVisibility` function) purely syntactically regardless.

TODO: we currently use `clang` for parsing in `hs-bindgen` without an
`-fvisilibity` option. Should we? What if a shared library is compiled and
linked using `-fvisibility=hidden`, but we use clang in `hs-bindgen` with
`-fvisibility=default`? It sounds like that would be wrong.

[visibility]: https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-visibility-function-attribute
[gcc-options]: https://gcc.gnu.org/onlinedocs/gcc/Code-Gen-Options.html#index-fvisibility
[clang-options]: https://clang.llvm.org/docs/ClangCommandLineReference.html#cmdoption-clang-fvisibility

## Guidelines for how visibility affects binding generation

The guideline for generating bindings to global variable declarations and
functions declarations in header files is to do this *unless* a declaration has
external linkage and non-public visibility. It is very likely that the generated
Haskell binding would otherwise fail to compile and link. For example, the
following would lead to a linker error:

```c
void __attribute__ ((visibility ("hidden"))) foo (void);
```

Note however, that this heck is conservative, and may rule out declarations that
do not lead to linker errors. We take this for granted. For example, the
following does not immediately lead to linker errors if a binding is generated
for it:

```c
int __attribute__ ((visibility ("hidden"))) i = 7;
```

### Example

NOTE: the example in this section can be readily adapted to global variables
instead of functions. Swap out `foo` for an `extern` variable, and similar
linker errors are observed

Let's say we have a header file `A.h` that declares a function `foo` with hidden
visiblity (and external linkage by default):

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

### Static linking

TODO: if we were to create a /static/ library out of `A`, then the visibiltity
doesn't really lead to errors when `A` and `B` are linked together, because `A`
and `B` are then in the /same shared object/. Rather, the visibility nows
determine which symbols are exposed from the shared object that includes both
`A` and `B`. We currently have not experiment much with statically linking
Haskell bindings with static C libraries (i.e., archives), but when we do we
might have to revisit the guidelines outlined in this document.
