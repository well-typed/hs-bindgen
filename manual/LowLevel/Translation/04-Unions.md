# Unions

> [!NOTE]
> We are considering changing the approach for dealing with unions.
> <https://github.com/well-typed/hs-bindgen/pull/567>

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

## Unions nested inside of structs

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
> <https://github.com/well-typed/hs-bindgen/issues/18>

Since we cannot provide a `Show` instance for `Occupation`, we also cannot
provide a `Show` instance for `Person`. We _could_ derive a dummy instance for
`Occupation`, which would then enable us to derive a `Show` instance for
`Person`. However, a custom user-written `Show` instance for `Person` could
do a much better job showing the `Occupation` (it can take advantage of the
`person_occupation_tag` field), and so we opt not to derive a `Show` instance
for `Person` either.

> [!NOTE]
> We currently _do_ try to derive `Show` for `Person`.
> This is a bug: <https://github.com/well-typed/hs-bindgen/issues/558>

[hackage:base:ByteArray]: https://hackage.haskell.org/package/base/docs/Data-Array-Byte.html#t:ByteArray
