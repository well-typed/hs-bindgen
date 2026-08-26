# Record dot syntax

`hs-bindgen` makes it convenient to optionally use record dot syntax with
Haskell bindings that are generated for C datatypes and other forms of C type
sugar. Constructing, accessing, and modifying bindings for C (data)types with
record syntax looks closer to the C syntax than plain Haskell does. This can be
a stylistic preference, but it can also produce less verbose code.

`hs-bindgen` tries hard to support using record dot syntax with generated
Haskell bindings whenever getting or setting fields would be supported in the
associated C code too. For the most part this is achieved by generating
`GHC.Records.HasField` and `GHC.Records.Compat.HasField` instances, if they are
not already [generated automatically by
GHC][ghc:guide:solving-hasfield-constraints]. The record dot syntax desguars to
uses of the methods of these type classes.

## Feature list

Currently `hs-bindgen` supports record dot syntax for getting and setting:

* Struct fields
* Union fields
* Bit-fields (for both structs and unions)
* Indirect fields (for both structs and unions)
* `newtype` fields (introduced by `typedef`s, macro-defined types, and `enum`s)

Record syntax for getters only is supported for:

* The [Pointer manipulation API][manual:pointer-manipulation-api]

Only getters are supported here, because setters would require `IO` to modify
the contents of pointers.

## Example: tagged unions

Let's consider a practical example: tagged[^1] unions that emulate sum types[^2]. In
our example we have a `Shape` datatype that can either be a `Rectangle` (two
opposite `Point`s) or a `Circle` (mid`Point` and radius). Because C unions do
not track which alternative is used, we track this information in a `Shape_Tag`
field.

```c
typedef struct { int x; int y; } Point;

typedef struct {
  Point top_left;
  Point bot_right;
} Rectangle;

typedef struct {
  Point mid;
  unsigned radius;
} Circle;

enum Shape_Tag { rectangle_tag, circle_tag };

typedef struct {
  enum Shape_Tag tag;
  union {
    Rectangle rectangle;
    Circle circle;
  };
} Shape;

extern const Shape c_example_Rectangle;

Shape c_move_x (int delta, Shape s);
```

We will walk through a few example uses of the `Shape` datatype to show what the
*C syntax*, *plain Haskell syntax*, and *record dot Haskell syntax* look like
compared to each other. These example uses are not exhaustive, as there are many
ways to write C and Haskell code, but they should give you an idea of what
record dot syntax would look like if you use it yourself.

### Initialization
[t:initialization]: #initialization

In C we might [initialize][creference:struct-union-initialization] an example
rectangle `Shape` as follows:

```c
const Shape c_example_Rectangle = {
      .tag = rectangle_tag
    , .rectangle.top_left.x  = 3
    , .rectangle.top_left.y  = 7
    , .rectangle.bot_right.x = 9
    , .rectangle.bot_right.y = -17
    };
```

Doing the same with *plain syntax* in Haskell, we get something like this:

```hs
hs_example_Rectangle :: Shape
hs_example_Rectangle = Shape {
      tag = Rectangle_tag
    , anon'rectangle = Union.set @"rectangle" $
        Rectangle {
            top_left  = Point { x = 3, y = 7 }
          , bot_right = Point { x = 9, y = -17 }
          }
    }
```

With *record dot syntax* in Haskell, we instead get something like this:

```hs
ds_example_Rectangle :: Shape
ds_example_Rectangle = (Struct.zero @Shape) {
      tag = Rectangle_tag
    , rectangle.top_left.x  = 3
    , rectangle.top_left.y  = 7
    , rectangle.bot_right.x = 9
    , rectangle.bot_right.y = -17
    }
```

This final Haskell version is already much closer to the C syntax.

### Getting and setting fields

We show an example function that gets and sets fields. The example function
moves a `Shape` along its `x` axis. For simplicity we'll return in C the input
shape unchanged if the shape tag is an unknown enumerator. It might look
something like this:

```c
Shape c_move_x (int delta, Shape s) {
  switch (s.tag) {
    case rectangle_tag:
      s.rectangle.top_left.x += delta;
      s.rectangle.bot_right.x += delta;
      break;
    case circle_tag:
      s.circle.mid.x += delta;
      break;
    default:
      break;
  };
  return s;
};
```

With *record dot syntax* in Haskell, we get something like this:

```c
ds_move_x :: HasCallStack => CInt -> Shape -> Shape
ds_move_x delta s = case s.tag of
    Rectangle_tag -> s {
        rectangle.top_left.x  = s.rectangle.top_left.x  + delta
      , rectangle.bot_right.x = s.rectangle.bot_right.x + delta
      }
    Circle_tag -> s { circle.mid.x = s.circle.mid.x + delta }
    _ -> error $ "ds_move_x: unknown shape tag: " ++ show s.tag
```

Note that in this case we're throwing an error when the shape tag is unknown,
because in Haskell we can throw imprecise errors whenever we want, which is not
possible in plain C.

## Enabling record dot syntax

A few GHC language extensions have to be enabled to start using record dot
syntax.

If [`OverloadedRecordDot`][ghc:guide:overloaded-record-dot] is enabled, one can
write `a.b` to mean the `b` field of the `a` expression.

If [`OverloadedRecordUpdate`][ghc:guide:overloaded-record-update] is enabled,
one can write `a { b = x }` to update the `b` field of the `a` expression to the
expression `x`.

The [`RebindableSyntax`][ghc:guide:rebindable-syntax] extension is currently
required when using `OverloadedRecordUpdate`, but when using this extension, a
number of (prelude) functions are suddenly no longer in scope that normally are.
The `hs-bindgen-runtime` package provides a module that restores those functions
to their standard definition.

```hs
import HsBindgen.Runtime.Overloading
```

## Recommendations

### Unprefixed field names

By default, `hs-bindgen` generates longer field names such that they are
globally unique. Field names can optionally be made more succinct by [omitting
field prefixes][manual:generated-names-unprefixed-field-names]. This is
recommended when using record dot syntax, if possible: unprefixed field names
can still cause name collisions (in which case the declaration is dropped) and
ambiguity in user-space Haskell programs.

### Zero values

[Zero values for structs][manual:structs-zero-values] and [zero values for
unions][manual:unions-zero-values] can be used to acquire uninitialized struct
and union values. This is particularly useful for emulating [nested
initialization][creference:nested-initialization] in Haskell. In Haskell, nested
record fields accessors such as `{ b.c.d = x }` are only allowed for record
*updates*, not record *construction*. Zero values can be used with record
updates to look like record construction, even though it technically is not. See
for example the [`ds_example_Rectangle` function][t:initialization]. Contrast
that function with the following version that would not be accepted by GHC:

```hs
ds_example_Rectangle_bad :: Shape
ds_example_Rectangle_bad = Shape {
      tag = Rectangle_tag
    , rectangle.top_left.x  = 3
    , rectangle.top_left.y  = 7
    , rectangle.bot_right.x = 9
    , rectangle.bot_right.y = -17
    }
```

<!-- footnotes -->

[^1]: the term "tagged" here means that we track in the datatype which
    alternative of the union is used. It does *not* mean ["tagged" in the sense
    of "named"][manual:terminology-tagged].
[^2]: the tagged union example is based on a real-world use case. See [this
    comment on issue #1649][issue:1649].

<!-- sources and references -->

[creference:struct-union-initialization]: https://en.cppreference.com/c/language/struct_initialization
[creference:nested-initialization]: https://en.cppreference.com/c/language/struct_initialization#Nested_initialization
[ghc:guide:overloaded-record-dot]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html
[ghc:guide:overloaded-record-update]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_update.html
[ghc:guide:solving-hasfield-constraints]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/hasfield.html#solving-hasfield-constraints
[ghc:guide:rebindable-syntax]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rebindable_syntax.html#extension-RebindableSyntax
[issue:1649]: https://github.com/well-typed/hs-bindgen/issues/1649#issuecomment-3994425922
[manual:generated-names-unprefixed-field-names]: generated-names.md#unprefixed-field-names
[manual:pointer-manipulation-api]: pointer-manipulation.md
[manual:structs-zero-values]: structs.md#zero-values
[manual:terminology-tagged]: ../../terminology.md#tagged-structunionenum
[manual:unions-zero-values]: unions.md#zero-values
