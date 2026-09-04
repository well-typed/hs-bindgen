# Nesting

A nested struct is a struct inside another struct or union. There are many ways
to declare nested structs, which are all supported by `hs-bindgen`.

In this manual section, we will show examples for the different ways of
declaring nested structs and the bindings that are generated for them. We will
use a running example where we declare a `door` struct that represents a door,
and we declare a `room` struct that represents a room, which includes one or
more nested door structs. For each example we will show what the Haskell
bindings look like, focussing on the datatype declarations and omitting other
declarations like type class instances. We will only show what the
struct-in-struct case looks like, but note that it works the same for any
nesting of structs and unions, in any order, and even recursively. See also the
[Unions/Nesting][manual:unions/nesting] manual section.

Nesting does not introduce a [scope][creference:scope] of its own. If the nested
struct has a [tag][manual:terminology-tag], that tag is visible from its
declaration onwards with the same scope the enclosing struct is declared in; for
declarations at [file scope][manual:terminology-file-scope], this is the rest of
the [translation unit][manual:terminology-translation-unit]. An
[untagged][manual:terminology-untagged-structunionenum] nested struct cannot be
referred to elsewhere.

## Example A

The most straightforward way to declare nested structs is by declaring `door`
before `room`, and then referring to `door` from the `room` declaration:

```c
struct doorA {
  float height;
  float width;
};

struct roomA {
  struct doorA door1;
  struct doorA door2;
};
```

This creates the following bindings:

```haskell
data DoorA = DoorA
  { doorA_height :: CFloat
  , doorA_width  :: CFloat
  }

data RoomA = RoomA
  { roomA_door1 :: DoorA
  , roomA_door2 :: DoorA
  }
```

## Example B

Alternatively, the `door` declaration could be moved into the `room`
declaration. Note that this code likely results in a compiler warning
("Declaration does not declare anything" in Clang), but the warning can be
ignored:

```c
struct roomB {
  struct doorB {
    float height;
    float width;
  };
  struct doorB door1;
  struct doorB door2;
};
```

The `door` struct has a top-level scope, even if its declaration is nested in
the `room` struct. As a result, we would get the same bindings as before (modulo
the struct names):

```haskell
data DoorB = DoorB
  { doorB_height :: CFloat
  , doorB_width  :: CFloat
  }

data RoomB = RoomB
  { roomB_door1 :: DoorB
  , roomB_door2 :: DoorB
  }
```

## Example C

Alternatively, the `door` declaration could be merged together with a field
declaration. This also removes the compiler warning from example B:

```c
struct roomC {
  struct doorC {
    float height;
    float width;
  } door1;
  struct doorC door2;
};
```

The `door` struct still has a top-level scope, and we can refer to it from the
other field too. As a result, we would get the same bindings as before (modulo
the struct names):


```haskell
data DoorC = DoorC
  { doorC_height :: CFloat
  , doorC_width  :: CFloat
  }

data RoomC = RoomC
  { roomC_door1 :: DooC
  , roomC_door2 :: DoorC
  }
```

## Example D

When the `door` struct declaration appears in a field declaration like before,
we can optionally omit the `door` struct name:

```c
struct roomD {
  struct {
    float height;
    float width;
  } door1;
};
```

A downside here is that we can only have a single field referring to the unnamed
struct because we can not refer to the untagged struct from a different field. In
such cases, we generate a Haskell name for the untagged struct based on the name
of the parent object (i.e., struct) and the field name. The Haskell bindings would look like
this:

```haskell
data RoomD_door1 = RoomD_door1
  { RoomD_door1_height :: CFloat
  , RoomD_door1_width  :: CFloat
  }

data RoomD = RoomD
  { roomD_door1 :: RoomD_door1
  }
```

## Example E

Finally, we could leave out the name of the `door1` field as well. Now the field
becomes an *implicit* field and the nested struct becomes an *anonymous* struct.
The definition of anonymous structs can be found in the [C
reference][creference:struct]:

> An unnamed member of a struct or union whose type is a struct without a name
> is known as an anonymous struct.

```c
struct roomE {
  struct {
    float height;
    float width;
  };
};
```

In this case, we generate a Haskell name for both the untagged struct and the
unnamed field. First, the unnamed field is named after the first field of the
anonymous struct. The field name is also prefixed with "anon'" to highlight that
the field is created for an anonymous struct/union. The tick ensures that this
name can not clash with any existing C fields because ticks are not allowed in C
identifiers.

Informally, this transforms the C code to:

```c
struct roomE {
  struct {
    float height;
    float width;
  } anon'height;
};
```

Then, the anonymous struct is named as in example D: We generate a Haskell name
for the untagged struct based on the name of the parent object (i.e., struct) and
the *newly assigned* field name. This leads to the following Haskell bindings:

```haskell
data RoomE_anon'height = RoomE_anon'height
  { RoomE_anon'height_height :: CFloat
  , RoomE_anon'height_width  :: CFloat
  }

data RoomE = RoomE
  { roomE_anon'height :: RoomE_anon'height
  }
```

This naming approach works from the bottom up if there are recursively nested
anonymous structs: we first name fields at the bottom of the nesting hierarchy,
and we work our way upwards from there. The naming of untagged structs then
follows the usual rules.

If these generated names are too unwieldy, they can always be customised using
[prescriptive binding specifications][manual:usage/binding-specs]. Moreover,
they can be shortened considerably by omitting field prefixes.

### Indirect fields

In C, *named* fields of an anonymous struct can be accessed from the parent struct *as
if* they were fields of the parent struct. This works recursively: if there are
multiple levels of anonymous structs, then their fields can be accessed from the
(top-level) parent struct. Generated Haskell bindings reflect this behaviour by
providing class instances for such *indirect* fields as if they were any other direct
field. For example, `HasField` instances such as:

```hs
instance HasField "ShapeE_height" RoomE CFloat
instance HasField "ShapeE_width" RoomE CFloat
```

Indirect fields are not represented in the Haskell datatype that is generated
for a struct, as opposed to other direct fields. Beyond that, users can treat
indirect fields as any other field, including in the context of: the [pointer
manipulation API][manual:pointer-manipulation-api], and [record dot
syntax][manual:record-dot-syntax].

Exceptions:

* Indirect fields are not yet supported in [binding
  specifications][manual:binding-specifications] ([issue #2178][is-2178])

### Limitations

For technical reasons we can only generate bindings for anonymous structs that
have at least one named (direct/indirect) field. Empty anonymous structs and
anonymous structs with only padding (specified using unnamed bit-fields) are not
supported. A warning-level trace message will be emitted in this case.



<!-- sources and references -->

[creference:scope]: https://en.cppreference.com/w/c/language/scope.html
[creference:struct]: https://en.cppreference.com/w/c/language/struct.html
[is-2178]: https://github.com/well-typed/hs-bindgen/issues/2178
[manual:binding-specifications]: ../../usage/binding-specifications.md
[manual:pointer-manipulation-api]: ../pointer-manipulation.md
[manual:record-dot-syntax]: ../record-dot-syntax.md
[manual:terminology-file-scope]: ../../../terminology.md#file-scope
[manual:terminology-tag]: ../../../terminology.md#tag
[manual:terminology-translation-unit]: ../../../terminology.md#translation-unit
[manual:terminology-untagged-structunionenum]: ../../../terminology.md#untagged-structunionenum
[manual:unions/nesting]: ../unions/nesting.md
[manual:usage/binding-specs]: ../../usage/binding-specifications.md
