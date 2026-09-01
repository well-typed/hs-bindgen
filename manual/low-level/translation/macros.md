# Macros

A C macro (or simply, a "macro") is a name associated with a replacement text.

## Terminology

/Macro definitions/ are C preprocessor directives termed "control lines". The C
standard specifies

> object-like macro:
>   # define identifier replacement-list new-line
>
> function-like macro:
>   # define identifier lparen identifier-listopt ) replacement-list new-line
>   # define identifier lparen ... ) replacement-list new-line
>   # define identifier lparen identifier-list , ... ) replacement-list new-line
>
> identifier: <complex syntax, see C standard>
> replacement-list: <preprocessing tokens including spaces>
> new-line: the new-line character
> lparen: a ( character not immediately preceded by white space
> identifier-list: identifier | identifier-list , identifier

A /macro invocation/ references a macro definition by name. Object-like macros
are invoked by their identifiers alone. Function-like macros are invoked by the
same syntax as function calls.

The C preprocessor parses macro definitions, and replaces macro invocations with
their `replacement-list`s. This process is termed /macro expansion/. Macro
expansion is a string query-replace process, and the replacement strings lack
any specification. This makes macros extremely powerful, but also terribly
error-prone. What is worse: (a) macros can be chained (e.g., `A` replaced by `B`
replaced by `C`); (b) macros have scope (macros comes into scope when they are
defined, and go out of scope when they are `#undefine`d or at the end), and (c)
C preprocessor directives can be located anywhere in the C code.

`hs-bindgen` uses `libclang` to parse C headers. By default, `libclang` expands
all macros, and so, we could opt for `hs-bindgen` being completely unaware of
the existence of macros. However, macros often carry semantic information.
For example, when translating a C header including

```c
#define PI 3.14159265
```

it is useful to obtain a binding to `PI`. To this end, `hs-bindgen` tries hard
to handle macros systematically. Similar to a compiler, it parses, resolves, and
typechecks macros before translating them to Haskell bindings.

Since the expansion of macros has no syntactic constraints, generation of
Haskell bindings to macros is best-effort. `hs-bindgen` supports common schemes
used in macros, but may fail on a specific macro you need. Let us know!

## Typical macros and their translations

Typical object-like macros:

```c
// Object-like macros translating to values

#define TRUE  1
#define FALSE 0
#define LETTER   'a'
#define GREETING "hello"

// Object-like macros translating to types
// TODO.
#define PtrInt int*
```

Typical function-like macros:

```c
// Function-like macros
#define CMP(X,Y) ( X < Y )

#define AND    &&

#define ASSERT(n)           if(!(n)){\
    printf(__FILE__ "@%d: `" #n "` - Failed | Compilation: " __DATE__ " " __TIME__ "\n", __LINE__);\
    return(-1);}
```

That is, macros can be object-like (e.g., `PI`) and function-like (e.g., `CMP`),

### Character and string literals

Object-like macros that expand to a single character or string literal are
translated to Haskell value bindings. The examples in this section come from
[`macro.h`][header:macro.h].

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
`ByteString` rather than `String` — the generated binding can be passed directly
to a C function that expects that byte sequence.

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

* Because the binding is an ordinary (pure) value rather than an `IO` action —
  unlike a [global variable][manual:translation/globals] — it can be inspected
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

<!-- sources and references -->

[creference:character-constant]: https://en.cppreference.com/w/c/language/character_constant.html
[creference:string-literal]: https://en.cppreference.com/w/c/language/string_literal.html
[hackage:base:CChar]: https://hackage.haskell.org/package/base/docs/Foreign-C-Types.html#t:CChar
[hackage:bytestring:ByteString]: https://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html#t:ByteString
[hackage:bytestring:useAsCString]: https://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html#v:useAsCString
[header:macro.h]: ../../c/macro.h
[manual:translation/globals]: globals.md


## The path of a macro through `hs-bindgen`

TODO. Ideas:

- macro languages
- `c-expr`
- raw and empty macro languages
- reparsing of declarations with macro expansions
