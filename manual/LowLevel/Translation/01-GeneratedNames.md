# Generated names

There are two aspects to name generation: producing a name _candidate_, and then
_fixing_ the candidate to conform to Haskell's naming rules. We discuss these
two aspects separately below, but it's worth emphasizing up-front that the
primary goal of the _default_ name mangler is to stick as close as possible to
the names as they are in the C code. This makes it easier to guess what the
Haskell name will be, and also is less likely to result in name clashes.

> [!NOTE]
> Many aspects of naming are configurable by providing an alternative
> implementation of the `NameMangler`. Currently this requires using
> `hs-bindgen` as a library; providing a syntax for modifying some aspects of
> name mangling through the CLI is future work. Here we discuss the defaults.
> The two aspects of name generation are captured by two separate abstractions
> in the library: `ProduceCandidate` and `FixCandidate`.

## Name candidates

By default, we use the name in the C header wherever one exists. An obvious
example is function definitions, where the candidate name for a function `foo`
is simply `foo`. Similarly, if a `struct` is given a tag or a `typedef`,

```c
struct foo { .. };
typedef struct { .. } foo;
```

then we use `foo` as the candidate name for both the Haskell type name and its
constructor. There are however cases where we need to construct a derived name.

### Field names

Fields in structs and unions do of course have a name in C, and we _could_ use
those as the Haskell record field labels. Unlike their C counterparts, however,
Haskell record field labels must be _globally_ unique. To avoid name clashes we
therefore prefix fields with the name of the type. For example, given

```c
typedef struct triple {
    int a;
    int b;
    int c;
} triple;
```

we generate

```haskell
data Triple = Triple {
    triple_a :: CInt
  , triple_b :: CInt
  , triple_c :: CInt
  }
```

(The capitalization is a result of candidate name _fixing_, see below.)

> [!NOTE]
> Alternatively we could take advantage of `DuplicateRecordFields` or
> `OverloadedRecordDot`/`OverloadedRecordUpdate`.
> <https://github.com/well-typed/hs-bindgen/issues/69>

### Accessors

For newtypes we add a prefix `un_` to the getter; for example, given

```c
typedef enum index {
   A,
   B,
   C
} index;
```

we generate

```haskell
newtype Index = Index {
   un_Index :: CUInt
 }
```

### Getters and setters

For unions we generate getters and setters (see detailed section on unions,
below); for those we use `get_` and `set_` prefixes. For example,

```c
union occupation {
 struct student  { .. } student;
 struct employee { .. } employee;
} occupation;
```

results in

```haskell
get_occupation_student  :: Occupation -> Student
get_occupation_employee :: Occupation -> Employee

set_occupation_student  :: Student  -> Occupation
set_occupation_employee :: Employee -> Occupation
```

> [!NOTE]
> Here too we could in principle take advantage of overloaded record syntax.
> <https://github.com/well-typed/hs-bindgen/issues/557>

### Anonymous types

For anonymous type declarations (such as untagged `struct` definitions, with
no corresponding `typedef`), we try and construct a name that makes sense in
context.

#### Inside `struct` or `union`

When the type is declared as part of a struct or union field, we use the name of
that field as the type name. For example, given

```c
struct rect {
  struct {
    int x;
    int y;
  } lower_left;

  struct {
    int x;
    int y;
  } upper_right;
};
```

we generate

```haskell
data Rect_lower_left = Rect_lower_left {
    rect_lower_left_x :: CInt
  , rect_lower_left_y :: CInt
  }

data Rect_upper_right = Rect_upper_right {
    rect_upper_right_x :: CInt
  , rect_upper_right_y :: CInt
  }

data Rect = Rect {
    rect_lower_left  :: Rect_lower_left
  , rect_upper_right :: Rect_upper_right
  }
```

> [!NOTE]
> With external bindings it might be possible to avoid this duplication, and use
> a _single_ struct definition with two fields `x` and `y`.
> <https://github.com/well-typed/hs-bindgen/issues/536>

#### Pointer `typedef`s

For `typedef`s that are _pointers_ to structs, we add a `_Deref` suffix. For
example, given

```c
typedef struct {
  int width;
  int height;
} *config;
```

we generate

```haskell
newtype Config = Config {
    un_Config :: Ptr Config_Deref
  }

data Config_Deref = Config_Deref {
    config_Deref_width  :: CInt
  , config_Deref_height :: CInt
  }
```

#### Top-level anonymous types

For top-level anonymous type declarations we don't generate any code (such
declarations are unusable).

## Fixing candidates

Once we have a candidate name, we need to fix it to make sure that it adheres
to Haskell's naming rules.

### Invalid characters

The name might contain invalid characters (characters that are invalid
_anywhere_ in a Haskell name). By default we escape such characters.

Note that in particular any character which does not satisfy
[`isAlphaNum`][hackage:base:isAlphaNum] is always invalid. This is sometimes
counter-intuitive; for example, `isAlphaNum` returns `True` for both `'ó'` and
`'你'`, but `"adiós"` contains a combining diacritical mark after the `o` for
which `isAlphaNum` returns `False`. After escaping, this name becomes
`adio'0301s` (or `Adio'0301s`) in Haskell.

> [!NOTE]
> We could instead _drop_ such invalid characters, but since this could result
> in name clashes we do not do this by default.

> [!NOTE]
> For this _particular_ example we could generate a better name if we
> transformed it to [NFC][unicode:NFC] first. Indeed, `gcc` (but not `clang`)
> will issue a warning that this name is not in NFC.
> <https://github.com/well-typed/hs-bindgen/issues/560>

> [!NOTE]
> C _functions_ with names that contain characters that are invalid in Haskell
> identifiers can currently not be imported, because `ghc` applies Haskell
> naming rules to C identifiers. This is unlikely to be an issue, but if it is,
> we'd need to either patch ghc or generate a C wrapper.
> <https://github.com/well-typed/hs-bindgen/issues/569>

### Capitalization

Haskell has two lexical groups of names: variables and type variables, which
_cannot_ start with an uppercase letter, and constructors, type constructors,
type classes and module names, all of which _must_ start with an uppercase
letter. Mostly we can ensure that this rule is satisfied simply by changing the
first letter to uppercase or lowercase, but there are edge cases.

#### "Caseless" characters

Some characters are _neither_ uppercase nor lowercase (that is also why we used
the phrase "cannot start with an uppercase letter" above instead of "must start
with a lowercase letter"). Such characters are therefore fine in variable names;
for example, for a C function

```c
void 拜拜(void);
```

we can simply generate

```haskell
foreign import capi safe "example.h 拜拜" 拜拜 :: IO ()
```

because Chinese characters like `拜` are not uppercase. However, they also
cannot be _made_ uppercase, so something like

```c
typedef int 数字;
```

is problematic, because the corresponding Haskell type must start with an
uppercase letter. In this case, we add a `"C"` prefix:

```haskell
newtype C数字 = C数字 {
    un_C数字 :: CInt
  }
```

#### Missing lowercase forms

Some characters _are_ considered uppercase but don't have a corresponding
lowercase variant. Such characters are rare, but do exist; when this happens, we
add a `"c"` prefix. For example, given

```c
void ϒ(void);
```

we generate

```haskell
foreign import capi safe "example.h ϒ" cϒ :: IO ()
```

### Reserved names

Finally, we have to deal with reserved names. In this case we add a `'` suffix,
which is invalid in C and so cannot result in clashes. For example, for

```c
void import(void);
```

we generate

```haskell
foreign import capi safe "example.h import" import' :: IO ()
```

We don't do this if capitalization is already sufficient to avoid the reserved
name; for example, for

```c
typedef int data;
```

we simply generate

```haskell
newtype Data = Data {
    un_Data :: CInt
  }
```

[hackage:base:isAlphaNum]: https://hackage.haskell.org/package/base/docs/Data-Char.html#v:isAlphaNum
[unicode:NFC]: https://unicode.org/reports/tr15/
