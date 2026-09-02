# Macros

A C [macro][manual:terminology-macro] is a [macro
name][manual:terminology-macro-name] associated with a [replacement
list][manual:terminology-replacement-list]. This chapter describes how
`hs-bindgen` translates macros to Haskell bindings. The terms used throughout
are defined in the [terminology][manual:terminology] chapter, and the examples
come from [`macro.h`][header:macro.h].

## Macros in C

A [macro definition][manual:terminology-macro-definition] is a `#define`
preprocessing directive. The [C standard][c-standard] gives its syntax as
follows

```text
object-like macro:
    # define identifier replacement-list new-line

function-like macro:
    # define identifier lparen identifier-list_opt ) replacement-list new-line
    # define identifier lparen ... ) replacement-list new-line
    # define identifier lparen identifier-list , ... ) replacement-list new-line

replacement-list:
    pp-tokens_opt
pp-tokens:
    preprocessing-token
    pp-tokens preprocessing-token
lparen:
    a ( character not immediately preceded by white space
identifier-list:
    identifier
    identifier-list , identifier
new-line:
    the new-line character
```

The preprocessor replaces each macro
[invocation][manual:terminology-macro-invocation] by the corresponding
replacement list, a process termed [macro
expansion][manual:terminology-macro-expansion].
[Object-like][manual:terminology-object-like-macro] macros are invoked by their
name alone; [function-like][manual:terminology-function-like-macro] macros are
invoked by the same syntax as a function call (i.e., only when the macro name is
immediately followed by the argument list enclosed by `(` and `)`).

A replacement list is a sequence of *preprocessing tokens* that need not form
any well-formed C construct. That is what makes macros powerful and, for a
binding generator such as `hs-bindgen`, awkward. Three further properties
augment the problem:

* Macros chains. The result of an expansion is
  [rescanned][manual:terminology-rescanning] for further macro names, so `A` can
  expand to `B`, which expands to `C`.
* Macro definitions have a lifetime rather than a lexical scope. A macro
  definition lasts from its `#define` until a matching `#undef`, or until the
  end of the translation unit.
* A `#define` may appear between any two declarations, although the `#` itself
  must start a line.

## Macros in `hs-bindgen`

`hs-bindgen` uses `libclang` to parse C headers. By default, `libclang` expands
all macros, and so we could opt for `hs-bindgen` being completely unaware of the
existence of macros. However, macros often carry semantic information. For
example, when translating a C header including

```c
#define EPSILON 0.1
```

it is useful to obtain a binding to `EPSILON`. To this end, `hs-bindgen` tries
hard to handle macros systematically. Similar to a compiler, it parses,
resolves, and typechecks macros before translating them to Haskell bindings.

Since the expansion of macros has no syntactic constraints, generation of
Haskell bindings to macros is best-effort. `hs-bindgen` supports common schemes
used in macros, but may fail on a specific macro you need. Let us know!

## Typical macros and their translations

The kind of binding `hs-bindgen` generates depends on what the replacement list
parses as: an expression, a type, or neither.

### Macro values

Object-like macros whose replacement list is a C expression are [macro
values][manual:terminology-macro-value], and are translated to Haskell value
bindings:

```c
#define FIELD_OFFSET 4
#define EPSILON      0.1
```

```haskell
fIELD_OFFSET :: CInt
fIELD_OFFSET = (4 :: CInt)

ePSILON :: CDouble
ePSILON = (0.1 :: CDouble)
```

The Haskell type is not given in the C code; it follows from typechecking the
replacement list under the C typing rules. Character and string literals are
macro values, [see below][t:literals].

### Macro types

Object-like macros whose replacement list is a C type are [macro
types][manual:terminology-macro-type], and are translated to Haskell newtypes:

```c
#define YEAR int
```

```haskell
newtype YEAR = YEAR
  { unwrapYEAR :: CInt
  }
  -- Many derived instances here.
```

We use `newtype` instead of a type synonym because the macro carries semantic
information (see [typedefs][manual:low-level/introduction-typedefs]).

Declarations that use the macro type refer to the newtype, not to its
underlying type:

```c
YEAR getYear(date *d);
```

```haskell
getYear :: Ptr Date -> IO YEAR
```

<details>
The fact that the return type of `getYear` refers to the macro type is lost by
`libclang` which reports it as `int`. `hs-bindgen` has to recover the reference
by [reparsing][t:reparsing] the declaration of `getYear`.
</details>

### Function-like macros

Function-like macros are translated to Haskell functions:

```c
#define PTR_TO_FIELD(ptr) ptr + 4
```

```haskell
pTR_TO_FIELD :: forall a. C.Add a CInt => a -> C.AddRes a CInt
pTR_TO_FIELD = \ptr -> ptr C.+ (4 :: CInt)
```

Macro parameters carry no type annotations, so `hs-bindgen` infers the most
general type. The class `Add` and the type family `AddRes` come from
[`c-expr-runtime`][hackage:c-expr-runtime], which mirrors the C typing rules at
the Haskell type level. Instantiating `a` to `Ptr x` yields `Ptr x`;
instantiating it to `CLong` yields `CLong`.

### Macros that are not translated

Not every macro has a Haskell counterpart. A macro whose replacement list is
neither an expression nor a type, such as

```c
#define AND &&
```

or one that expands to statements rather than to a value, such as

```c
#define ASSERT(n) if(!(n)){                                                 \
    printf(__FILE__ "@%d: `" #n "` - Failed\n", __LINE__);                  \
    return(-1);}
```

is not translated and no binding is generated. `hs-bindgen` emits a trace
message and carries on; see [tracing][manual:tracing].

### Character and string literals
[t:literals]: #character-and-string-literals

Object-like macros that expand to a single character or string literal are
[macro values][manual:terminology-macro-value], and are translated to Haskell
value bindings.

#### Character literals

A [character constant][creference:character-constant] such as

```c
#define LETTER 'a'
```

is translated to a [`CChar`][hackage:base:CChar] holding the value of the
character:

```haskell
lETTER :: CChar
lETTER = 97
```

In C a character constant has type `int`; its value fits in a single byte.
`hs-bindgen` uses `CChar` to represent characters, so they can directly be used
with C functions. For example,

```c
#define CHINESE 'c'

#define JAPANESE 'j'

// Greet in Chinese ('c') or Japanese ('j').
void greet(char lang);
```

```haskell
cHINESE :: CChar
cHINESE = 99

greet :: CChar -> IO ()
greet = ...

main :: IO ()
main = greet cHINESE
```

Escape sequences are understood and reduced to their byte value. For example,
`'\a'` (the ASCII BEL control character) becomes the byte 7:

```haskell
bELL :: CChar
bELL = 7
```

Wide character literals (prefixed with `L`, `u`, `U`, or `u8`) and characters
whose value does not fit in a single byte are rejected. The original C literal
is preserved in the generated Haddock comment.

#### String literals

A [string literal][creference:string-literal] such as

```c
#define GREETING "hello"
```

is translated to a [`ByteString`][hackage:bytestring:ByteString]:

```haskell
gREETING :: ByteString
gREETING = BS.pack [0x68, 0x65, 0x6C, 0x6C, 0x6F]
```

The byte-string holds the *execution-encoding bytes* of the literal, assuming a
UTF-8 execution character set _excluding the terminating `null`_. Apart from the
terminating `null`, the representation is *bit-for-bit accurate*: the bytes are
exactly what a C compiler would embed in the object file. This is why we use
`ByteString` rather than `String`. Then, the generated binding can be passed
directly to a C function that expects that byte sequence.

Two consequences are worth highlighting:

* The byte-string contains *no implicit terminating `null`*; it is exactly the
  content of the literal. Embedded nulls (e.g. from `"abc\0def"`) are preserved
  verbatim, and multi-byte characters are stored as their individual UTF-8
  bytes. For example, "hello" in Japanese is five characters but fifteen UTF-8
  bytes:

    ```c
    #define GREETING_JP "こんにちは"
    ```

    ```haskell
    gREETING_JP :: ByteString
    gREETING_JP =
      BS.pack [ 0xE3, 0x81, 0x93, 0xE3, 0x82, 0x93, 0xE3, 0x81, 0xAB
              , 0xE3, 0x81, 0xA1, 0xE3, 0x81, 0xAF ]
    ```

* Because the binding is an ordinary (pure) value rather than an `IO` action
  (unlike a [global variable][manual:translation/globals]), it can be inspected
  and rendered in pure code:

    ```haskell
    BS8.unpack gREETING  -- "hello", computed purely, no IO required
    ```

#### Passing a string literal to C

Since the byte-string carries no terminating `null`, handing it to a C function
that expects a `null`-terminated string requires
[`Data.ByteString.useAsCString`][hackage:bytestring:useAsCString], which copies
the bytes, appends the `null`, and gives us a pointer to pass on. Given

```c
int greeting_length(char* str);
```

we can write

```haskell
len <- BS.useAsCString gREETING greeting_length
print len  -- 5
```

If you do not require the string to be terminated by `null`, use
`Data.ByteString.useAsCStringLen`, which copies the bytes, but does not append
the `null`.

## The path of a macro through `hs-bindgen`

### Macro languages

The way how `hs-bindgen` parses, typechecks and translates [replacement
lists][manual:terminology-replacement-list] is not fixed. Instead, a pluggable
[macro language][manual:terminology-macro-language] is used. At the moment,
`hs-bindgen` comes with the following macro languages:

* `CExpr` is the default. `CExpr` understands C expressions and C type
  expressions sorting them into [macro values][manual:terminology-macro-value]
  and [macro types][manual:terminology-macro-type], respectively. The
  implementation of `CExpr` is available in a separate repository containing the
  macro language ([`c-expr-dsl`][hackage:c-expr-dsl]), and a corresponding
  runtime ([`c-expr-runtime`][hackage:c-expr-runtime]).
* `Empty` recognises no macro at all, so the generated bindings directly use the
  macro [expansions][manual:terminology-macro-expansion] from `libclang`. No
  bindings to macros are generated.
* `Raw` treats every macro as a macro value whose translation is its token list.

`hs-bindgen-cli` always uses `CExpr`. The other two are reachable from the
Template Haskell backend, by using `withHsBindgenMacroLang` in place of
`withHsBindgen`; they exist mainly for testing and for diagnosing macro
handling.

### Reparsing declarations with macro expansions
[t:reparsing]: #reparsing-declarations-with-macro-expansions

By default `libclang` expands macros before `hs-bindgen` sees a declaration.
Given

```c
#define YEAR int

YEAR getYear(date *d);
```

`libclang` reports the return type of `getYear` as `int`. The information that a
macro was involved at all is lost. `hs-bindgen` therefore
[reparses][manual:terminology-reparsing] those declarations that contain macro
expansions, inspecting the original tokens, so that the generated binding can
refer to the macro type `YEAR` rather than to `int`.

Macros are typechecked before any declaration with macro expansions is reparsed:
the [macro type][manual:terminology-macro-type] has to exist before a
declaration can refer to it.



<!-- sources and references -->

[c-standard]: https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf
[creference:character-constant]: https://en.cppreference.com/w/c/language/character_constant.html
[creference:string-literal]: https://en.cppreference.com/w/c/language/string_literal.html
[hackage:base:CChar]: https://hackage.haskell.org/package/base/docs/Foreign-C-Types.html#t:CChar
[hackage:bytestring:ByteString]: https://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html#t:ByteString
[hackage:bytestring:useAsCString]: https://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html#v:useAsCString
[hackage:c-expr-dsl]: https://hackage.haskell.org/package/c-expr-dsl
[hackage:c-expr-runtime]: https://hackage.haskell.org/package/c-expr-runtime
[header:macro.h]: ../../c/macro.h
[manual:low-level/introduction-typedefs]: ../introduction.md#typedefs
[manual:terminology]: ../../terminology.md
[manual:terminology-function-like-macro]: ../../terminology.md#function-like-macro
[manual:terminology-macro]: ../../terminology.md#macro
[manual:terminology-macro-definition]: ../../terminology.md#macro-definition
[manual:terminology-macro-expansion]: ../../terminology.md#macro-expansion
[manual:terminology-macro-invocation]: ../../terminology.md#macro-invocation
[manual:terminology-macro-language]: ../../terminology.md#macro-language
[manual:terminology-macro-name]: ../../terminology.md#macro-name
[manual:terminology-macro-type]: ../../terminology.md#macro-type
[manual:terminology-macro-value]: ../../terminology.md#macro-value
[manual:terminology-object-like-macro]: ../../terminology.md#object-like-macro
[manual:terminology-parsable-macro]: ../../terminology.md#parsable-macro
[manual:terminology-reparsing]: ../../terminology.md#reparsing
[manual:terminology-replacement-list]: ../../terminology.md#replacement-list
[manual:terminology-rescanning]: ../../terminology.md#rescanning
[manual:tracing]: ../usage/tracing.md
[manual:translation/globals]: globals.md
