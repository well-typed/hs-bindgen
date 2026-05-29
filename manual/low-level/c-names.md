# C names

Declarations are referred to using C names in binding specifications and select
predicates.  These names are generally the name as written in C, but special
syntax is used for types that are not given names in C.  When unsure of a C
name, generate binding specifications to confirm what name is used.

## `struct` types

The name of a `struct` type that specifies a tag is the same as in C, using
prefix `struct`.

The following example introduces one type, which has name `struct pt`.

```c
struct pt {
    int x, y;
};
```

## `union` types

The name of a `union` type that specifies a tag is the same as in C, using
prefix `union`.

The following example introduces one type, which has name `union u1`.

```c
union u1 {
    foo x;
    bar y;
};
```

## `enum` types

The name of an `enum` type that specifies a tag is the same as in C, using
prefix `enum`.

The following example introduces one type, which has name `enum e1`.

```c
enum e1 {
    A,
    B
};
```

## `typedef` types

The name of a `typedef` type is the same as in C, with no prefix.

The following example introduces one type, which has name `ident`.

```c
typedef int ident;
```

The following example introduces one type, which has name `intptr`.

```c
typedef int * intptr;
```

The following example introduces two types:

* The `struct` type has name `struct pt`.
* The `typedef` alias for that type has name `point`.

```c
typedef struct pt {
    int x, y;
} point;
```

The following example introduces one type, which has name `f1`.

```c
typedef void (*f1)(void);
```

## Type-defining macros

Some macros behave like a `typedef`, defining an alias for a type.  We
distinguish macro types using a `macro` prefix.

The following example introduces one type, which has name `macro ident`.

```c
#define ident int
```

## Anonymous types

An *anonymous type* is a `struct`, `union`, or `enum` type that is not given a
tag.  One cannot refer to an anonymous type by name in C, but we need to be able
to refer to it in binding specifications.  Special syntax is used: the `@`
character.  Since this character cannot be used in C, it emphasizes that it is
an `hs-bindgen` convention.  The character resembles a lowercase `a`, short for
"anonymous."

Anonymous names cannot be specified in select predicates, because there is not
much sense in selecting or excluding an anonymous type separate from its
context.

Anonymous types can be declared in various places in C headers, and `hs-bindgen`
assigns names based on the context.

### Anonymous types wrapped in a `typedef`

It is common to name `struct`, `union`, and `enum` types using a `typedef`,
without specifying a tag.  In this case, we construct a name based on the name
of the `typedef`.

The following example introduces two types:

* The `struct` type has name `struct point`.
* The `typedef` alias for that type has name `point`.

```c
typedef struct {
    int x, y;
} point;
```

> [!WARNING]
> In this case, the `@` syntax is not used.  This is due to a limitation of
> `libclang`.

### Anonymous global types

The declaration for a global variable can use an anonymous type.  In this case,
we construct a name based on the name of the global variable.

The following example introduces one type, which has name `struct @anon_point`.

```c
struct {
    int x, y;
} anon_point;
```

### Anonymous field types

A declaration of a field in a `struct` or `union` can use an anonymous type.
When the field has a name, we construct a name based on the names of the
`struct` or `union` and the field.

The following example introduces two types:

* The outermost `struct` has name `struct s1`.
* The anonymous `struct` has name `struct @s1_pt`.

```c
struct s1 {
    struct {
        int x, y;
    } pt;
};
```

When anonymous declarations are nested, the constructed name reflects the
hierarchy.  The following example introduces three types:

* The outermost `struct` has name `struct region`.
* The anonymous `struct` use for the `bounds` field has name
  `struct @region_bounds`.
* The innermost `struct` has name `struct @region_bounds_tl`.  Note that the
  first field name is used.

```c
struct region {
    struct {
        struct {
            int x, y;
        } tl, br;
    } bounds;
};
```

### Anonymous implicit field types

When the field does not have a name, we construct a name based on the names of
the `struct` or `union` and the first field in the anonymous `struct`.

The following example introduces two types:

* The outermost `struct` has name `struct s2`.
* The anonymous `struct` has name `struct @s2_x`.

```c
struct s2 {
    struct {
        int x, y;
    };
};
```
