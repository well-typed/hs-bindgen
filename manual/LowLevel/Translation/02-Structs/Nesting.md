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
struct because we can not refer to the unnamed struct from a different field. In
such cases, we generate a Haskell name for the unnamed struct based on the name
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

Finally, we could leave out the name of the `door1` field as well. Now the
nested struct becomes an *anonymous* struct. The definition of anonymous structs
can be found in the [C reference][c-reference:struct]:

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

In this case, we generate a Haskell name for both the unnamed struct and the
unnamed field. First, the unnamed field is named after the first field of the
anonymous struct. Informally, this transforms the C code to:

```c
struct roomE {
  struct {
    float height;
    float width;
  } height;
};
```

Then, the anonymous struct is named as in example D: We generate a Haskell name
for the unnamed struct based on the name of the parent object (i.e., struct) and
the *newly assigned* field name. This leads to the following Haskell bindings:

```haskell
data RoomE_height = RoomE_height
  { RoomE_height_height :: CFloat
  , RoomE_height_width  :: CFloat
  }

data RoomE = RoomE
  { roomE_height :: RoomE_height
  }
```

This naming approach works from the bottom up if there are recursively nested
anonymous structs: we first name fields at the bottom of the nesting hierarchy,
and we work our way upwards from there. The naming of unnamed structs then
follows the usual rules.

If these generated names are too unwieldy, they can always be customised using
[prescriptive binding specifications][manual:usage/binding-specs].

### Limitations

For technical reasons we can only generate bindings for anonymous structs that
have at least one named field. Empty anonymous structs and anonymous structs
with only padding (specified using unnamed bit-fields) are not supported. A
warning-level trace message will be emitted in this case.



<!-- sources and references -->

[c-reference:struct]: https://en.cppreference.com/w/c/language/struct.html
[manual:usage/binding-specs]: ../../Usage/06-BindingSpecifications.md
[manual:unions/nesting]: ../04-Unions/Nesting.md
