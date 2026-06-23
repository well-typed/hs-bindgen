# Macros

TODO: General discussion of macro translation.

## Character and string literals

Object-like macros that expand to a single character or string literal are
translated to Haskell value bindings. The examples in this section come from
[`macro.h`][header:macro.h].

### Character literals

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

### String literals

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

### Passing a string literal to C

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
