# Nesting

A nested union is a union inside another struct or union. There are many ways to
declare nested union, which are all supported by `hs-bindgen`.

In this manual section, we will show examples for the different ways of
declaring nested union and the bindings that are generated for them. We will use
a running example where we declare a `size` union that represents either a
circle radius or the length of the side of a square, and we declare a `shape`
struct, which includes a nested `size` and union tag. For each example we will
show what the Haskell bindings look like, focussing on the datatype declarations
and omitting other declarations like type class instances. We will only show
what the union-in-struct case looks like, but note that it works the same for
any nesting of structs and unions, in any order, and even recursively. See also
the [Structs/Nesting][manual:structs/nesting] manual section.

## Preliminaries

C unions do not carry any information about which alternative of the union is
used, so we need to store some information outside of the union to whether the
union is holding a radius or a length. For this, we define a `shape_tag` enum,
which can either be a circle (with a radius), or a square (with a length of a
side):

```c
enum shape_tag { circle, square };
```

This creates the following bindings:

```hs
newtype Shape_tag
pattern Circle :: Shape_tag
pattern Square :: Shape_tag
```

## Example A

The most straightforward way to declare nested unions is by declaring `size`
before `shape`, and then referring to `size` from the `shape` declaration:

```c
union sizeA {
  float radius;
  int length;
};

struct shapeA {
  enum shape_tag tag;
  union sizeA size;
};
```

This creates the following bindings:

```hs
newtype SizeA
get_sizeA_radius :: SizeA  -> CFloat
set_sizeA_radius :: CFloat -> SizeA
get_sizeA_length :: SizeA  -> CInt
set_sizeA_length :: CInt   -> SizeA

data ShapeA = ShapeA
  { shapeA_tag  :: Shape_tag
  , shapeA_size :: SizeA
  }
```

## Example B

Alternatively, the `size` declaration could be moved into the `shape`
declaration. Note that this code likely results in a compiler warning
("Declaration does not declare anything" in Clang), but the warning can be
ignored:

```c
struct shapeB {
  enum shape_tag tag;
  union sizeB {
    float radius;
    int length;
  };
  union sizeB size;
};
```

The `size` union has a top-level scope, even if its declaration is nested in the
`shape` struct. As a result, we would get the same bindings as before (modulo
the struct/union names):

```hs
newtype SizeB
get_sizeB_radius :: SizeB  -> CFloat
set_sizeB_radius :: CFloat -> SizeB
get_sizeB_length :: SizeB  -> CInt
set_sizeB_length :: CInt   -> SizeB

data ShapeB = ShapeB
  { shapeB_tag  :: Shape_tag
  , shapeB_size :: SizeB
  }
```

## Example C

Alternatively, the `size` declaration could be merged together with a field
declaration. This also removes the compiler warning from example B:

```c
struct shapeC {
  enum shape_tag tag;
  union sizeC {
    float radius;
    int length;
  } size;
};
```

The `size` union still has a top-level scope (so we could potentially refer to
it from other fields too). As a result, we would get the same bindings as before
(modulo the struct/union names):

```hs
newtype SizeC
get_sizeC_radius :: SizeC  -> CFloat
set_sizeC_radius :: CFloat -> SizeC
get_sizeC_length :: SizeC  -> CInt
set_sizeC_length :: CInt   -> SizeC

data ShapeC = ShapeC
  { shapeC_tag  :: Shape_tag
  , shapeC_size :: SizeC
  }
```

## Example D

When the `size` union declaration appears in a field declaration like before, we
can optionally omit the `size` union name:

```c
struct shapeD {
  enum shape_tag tag;
  union {
    float radius;
    int length;
  } size;
};
```

A downside here is that we can only have a single field referring to the unnamed
union because we can not refer to the unnamed union from a different field. In
such cases, we generate a Haskell name for the unnamed union based on the name
of the parent object (i.e., struct) and the field name. The Haskell bindings
would look like this:

```hs
newtype ShapedD_size
get_shapedD_size_radius :: ShapedD_size -> CFloat
set_shapedD_size_radius :: CFloat       -> ShapedD_size
get_shapedD_size_length :: ShapedD_size -> CInt
set_shapedD_size_length :: CInt         -> ShapedD_size

data ShapeD = ShapeD
  { shapeD_tag  :: Shape_tag
  , shapeD_size :: ShapedD_size
  }
```

## Example E

Finally, we could leave out the name of the `size` field as well. Now the nested
union becomes an *anonymous* union. The definition of anonymous unions can be
found in the [C reference][c-reference:union]:

> An unnamed member of a union whose type is a union without name is known as
> anonymous union.

```c
struct shapeE {
  enum shape_tag tag;
  union {
    float radius;
    int length;
  };
};
```

In this case, we generate a Haskell name for both the unnamed union and the
unnamed field. First, the unnamed field is named after the first field of the
anonymous union. Informally, this transforms the C code to:

```c
struct shapeE {
  enum shape_tag tag;
  union {
    float radius;
    int length;
  } radius;
};
```

Then, the anonymous union is named as in example D: We generate a Haskell name
for the unnamed union based on the name of the parent object (i.e., struct) and
the *newly assigned* field name. This leads to the following Haskell bindings:

```hs
newtype ShapeE_radius
get_shapeE_radius_radius :: ShapedD_size -> CFloat
set_shapeE_radius_radius :: CFloat       -> ShapedD_size
get_shapeE_radius_length :: ShapedD_size -> CInt
set_shapeE_radius_length :: CInt         -> ShapedD_size

data ShapeE = ShapeE
  { shapeE_tag    :: Shape_tag
  , shapeE_radius :: ShapeE_radius
  }
```

This naming approach works from the bottom up if there are recursively nested
anonymous unions: we first name fields at the bottom of the nesting hierarchy,
and we work our way upwards from there. The naming of unnamed unions then
follows the usual rules.

If these generated names are too unwieldy, they can always be customised using
[prescriptive binding specifications][manual:usage/binding-specs].

### Limitations

For technical reasons we can only generate bindings for anonymous unions that
have at least one named field. Empty anonymous unions and anonymous unions with
only padding (specified using unnamed bit-fields) are not supported. A
warning-level trace message will be emitted in this case.



<!-- sources and references -->

[c-reference:union]: https://en.cppreference.com/w/c/language/union.html
[manual:usage/binding-specs]: ../../Usage/06-BindingSpecifications.md
[manual:structs/nesting]: ./02-Structs/Nesting.md
