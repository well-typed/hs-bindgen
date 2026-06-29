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
    unwrapOccupation :: ByteArray
  }

instance ( ty ~ Student
         ) => GHC.Records.HasField
                "occupation_student" Occupation ty where
instance ( ty ~ Student
         ) => GHC.Records.Compat.HasField
                "occupation_student" Occupation ty where

instance ( ty ~ Employee
         ) => GHC.Records.HasField
                "occupation_employee" Occupation ty where
instance ( ty ~ Employee
         ) => GHC.Records.Compat.HasField
                "occupation_employee" Occupation ty where

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

> [!NOTE]
> An `Eq` instance based on the contents of the `ByteArray` is technically
> possible but likely to be wrong. The `byteArray` is large enough to hold all
> alternatives of the union; if we compare two values which both use the same
> alternative, and are equal in the relevant parts of the ByteArray, then we
> should consider them equal, even if they have different "trailing" data.

## Getters, setters, and zero values

The `HsBindgen.Runtime.Union` module from the `hs-bindgen-runtime` package
provides getters and setters via the `get` and `set` function respectively.
Getters and setters are only available for union fields of which the type is
`Storable`. For example, the `student` field[^1] of the `occupation` union can
be get and set like so:

```hs
    do
      let occupation = Union.set @"occupation_student" Student{
              student_university = nullPtr
            , student_year       = 2000
            }
      print $ Union.get @"occupation_student" occupation
      with occupation $ print_occupation 0

    do
      let occupation = Union.set @"occupation_employee" Employee{
              employee_company    = nullPtr
            , employee_supervisor = nullPtr
            , employee_salary     = 100_000
            }
      print $ Union.get @"occupation_employee" occupation
      with occupation $ print_occupation 1
```

The `set` and `get` functions use `GHC.Records.HasField` and
`GHC.Records.Compat.HasField` instances under the hood. These instances are
generated for all union fields of which the type is `Storable`. The instances
can be used directly through the `getField`, `setField`, and `hasField`
functions that these classes provide, or they can be used with the
`OverloadedRecordDot` or `OverloadedRecordUpdate` language extensions.

In some cases a union value is required when it is not yet clear which union
alternative is going to be used (if any). In such cases, the `zero` function
from the `IsUnion` class provides a union value that is initialised to all
zeroes. Instances for `IsUnion` are generated automatically for generated union
types.

> [!WARNING]
> It is the responsibility of the caller to ensure that the _correct_ getter is
> used (typically using information obtained elsewhere). In the above example,
> calling `Unions.get @"occupation_student"` on a `Occupation` union value that
> is an `Employee` will result in undefined behaviour.

## High-level API generation

Consider a union declaration inside of a struct:

```c
struct person {
  char* name;
  int occupation_tag;

  union occupation occupation; // defined above
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

## Nesting

See the [Unions/Nesting][manual:unions/nesting] manual section.



<!-- footnotes -->

[^1]: Generated Haskell names for union fields are described in the [Generated
    names][manual:generated-names] section.

<!-- sources and references -->

[hackage:base:ByteArray]: https://hackage.haskell.org/package/base/docs/Data-Array-Byte.html#t:ByteArray
[manual:generated-names]: generated-names.md
[manual:unions/nesting]: unions/nesting.md
