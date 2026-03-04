# Unions

Suppose we have

```c
union occupation {
  struct student {
    char* university;
    int year;
  } student;

  struct employee {
    char* company;
    struct person* supervisor;
    int salary;
  } employee;
} occupation;
```

then we generate

```haskell
newtype Occupation = Occupation {
    un_Occupation :: ByteArray
  }

get_occupation_student  :: Occupation -> Student
get_occupation_employee :: Occupation -> Employee

set_occupation_student  :: Student  -> Occupation
set_occupation_employee :: Employee -> Occupation

data Student = Student {
    student_university :: Ptr CChar
  , student_year       :: CInt
  }

data Employee = Employee {
    employee_company    :: Ptr CChar
  , employee_supervisor :: Ptr Person
  , employee_salary     :: CInt
  }
```

We represent the union as an opaque [`ByteArray`][hackage:base:ByteArray]
because C unions do not carry any information about which alternative of the
union is used. Therefore we do not try to represent the union as a Haskell value
at all, and instead provide separate getters and setters which can be used to
introduce or eliminate union values. For the same reason, we _only_ provide a
`Storable` instance for unions, with no support for other type classes such as
`Show` or `Eq`.

> [!WARNING]
> It is the responsibility of the caller to ensure that the _correct_ getter is
> used (typically using information obtained elsewhere). In the above example,
> calling `get_occupation_student` on a `Occupation` union value that is an
> `Employee` will result in undefined behaviour.

> [!NOTE]
> An `Eq` instance based on the contents of the `ByteArray` is technically
> possible but likely to be wrong. The `byteArray` is large enough to hold all
> alternatives of the union; if we compare two values which both use the same
> alternative, and are equal in the relevant parts of the ByteArray, then we
> should consider them equal, even if they have different "trailing" data.

[hackage:base:ByteArray]: https://hackage.haskell.org/package/base/docs/Data-Array-Byte.html#t:ByteArray

## High-level API generation

Consider a union declaration inside of a struct:

```c
struct person {
  char* name;
  int occupation_tag;

  union occupation {
    // .. as before ..
  } occupation;
};
```

There is not really anything new here; as expected, we generate

```haskell
data Person = Person
  { person_name           :: Ptr CChar
  , person_occupation_tag :: CInt
  , person_occupation     :: Occupation
  }
```

It is worth taking a look at this though because it is a common pattern in C.

> [!NOTE]
> Recognizing _particular_ union patterns like this where we can tell which
> alternative is used, and representing these as proper Haskell ADTs, can be
> done in the high-level API generation.
> https://github.com/well-typed/hs-bindgen/issues/18

Since we cannot provide a `Show` instance for `Occupation`, we also cannot
provide a `Show` instance for `Person`. We _could_ derive a dummy instance for
`Occupation`, which would then enable us to derive a `Show` instance for
`Person`. However, a custom user-written `Show` instance for `Person` could
do a much better job showing the `Occupation` (it can take advantage of the
`person_occupation_tag` field), and so we opt not to derive a `Show` instance
for `Person` either.

## Nested unions

> [!NOTE] This section has many similarities to the [Nested structures
> section](./Structs.md#nested-structures). Consider reading both.

A nested union is a union inside another union or structure. Nested unions can
be declared _separately_ or in an _embedded_ way. If nested unions are
_embedded_, then they can either be _named_ or _anonymous_ unions.

For the most part the generated bindings are unsurprising regardless of whether
a nested union is inside a union or a structure. For simplicity we will almost
exclusively show examples of nested unions inside unions in the remainder of
this section. The one exception where we consider structs and unions separately
has to do with _anonymous_ unions.

### Separate declaration

First we declare a named union, and then refer to it from the declaration of the
enclosing union:

```c
/* Separate declaration of named union. */
union length_unit {
  int feet;
  int meter;
};

/* Use named union in declaration of nested union. */
union dimension {
  union length_unit length;
  union length_unit width;
  union length_unit height;
};
```

`hs-bindgen` generates the following bindings (instances omitted for brevity):

```haskell
newtype Length_unit = Length_unit
  { un_Length_unit :: ByteArray
  }

get_length_unit_feet :: Length_unit -> CInt
set_length_unit_feet :: CInt -> Length_unit
get_length_unit_meter :: Length_unit -> CInt
set_length_unit_meter :: CInt -> Length_unit

newtype Dimension = Dimension
  { un_Dimension :: ByteArray
  }

get_dimension_length :: Dimension -> Length_unit
set_dimension_length :: Length_unit -> Dimension
get_dimension_width :: Dimension -> Length_unit
set_dimension_width :: Length_unit -> Dimension
get_dimension_height :: Dimension -> Length_unit
set_dimension_height :: Length_unit -> Dimension
```

### Embedded declaration (with variable name)

Embedded unions can have variable names:

```c
/* Declare nested union in an embedded way. The embedded union has a variable
   name. */
union length1 {
  union {
    int feet;
    int meter;
  } feet_or_meter;
  int yard;
};
```

`hs-bindgen` generates the following bindings (instances omitted for brevity):

```hs
newtype Length1_feet_or_meter = Length1_feet_or_meter
  { un_Length1_feet_or_meter :: ByteArray
  }

get_length1_feet_or_meter_feet :: Length1_feet_or_meter -> CInt
set_length1_feet_or_meter_feet :: CInt -> Length1_feet_or_meter
get_length1_feet_or_meter_meter :: Length1_feet_or_meter -> CInt
set_length1_feet_or_meter_meter :: CInt -> Length1_feet_or_meter

newtype Length1 = Length1
  { un_Length1 :: ByteArray
  }

get_length1_feet_or_meter :: Length1 -> Length1_feet_or_meter
set_length1_feet_or_meter :: Length1_feet_or_meter -> Length1
get_length1_yard :: Length1 -> CInt
set_length1_yard :: CInt -> Length1
```

### Embedded declaration (anonymous)

The definition of an anonymous union is as follows (quoted from
[cppreference.com][cppreference:union]):

[cppreference:union]: https://en.cppreference.com/w/c/language/union.html

> Similar to struct, an unnamed member of a union whose type is a union without
> name is known as anonymous union. Every member of an anonymous union is
> considered to be a member of the enclosing struct or union keeping their union
> layout. This applies recursively if the enclosing struct or union is also
> anonymous.

Anonymous unions are sometimes used when defining nested unions in an embedded
way. This is one such example:

```c
/* Declare nested union in an embedded way. The embedded union is anonymous. */
union length2 {
  union {
    int feet;
    int meter;
  };
  int yard;
};
```

By the C reference's definition of an anonymous union, the fields of the nested
union can be accessed as if they were part of the enclosing union. The same
would be valid if the enclosing union were a structure instead. Sometimes, we
refer to such fields as _implicit fields_.

```c
union length2 l;
l.feet = 1; // valid
l.meter = 2; // valid
l.yard = 3; // valid
```

Currently `libclang` [does not provide information about the offset and
alignment of implicit
fields](https://github.com/llvm/llvm-project/issues/122257). This is problematic
for binding generation, but to varying degrees depending on whether a nested
union is enclosed by a union or a structure.

* **Nested union inside an enclosing union**: the members of the enclosing union
  are stored at offset 0 with respect to the enclosing union. Meaning that a
  nested union is also stored at offset 0, hence we do not need `libclang` to
  tell us what the offset of the nested union field is.

* **Nested union inside an enclosing structure**: the members of the enclosing
  structure are stored at different offsets with respect to the enclosing
  structure. Since we don't know these offsets, we can not generate bindings. We
  [plan to support implicit
  fields](https://github.com/well-typed/hs-bindgen/issues/682) for this scenario
  in the future.
