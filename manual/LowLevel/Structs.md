# Structs

In this section we will consider the details of translating [C
structures](https://en.wikipedia.org/wiki/Struct_(C_programming_language))
(`struct`s). The examples are available in the [C header file
`structs.h`](/manual/c/structs.h).

In the [Introduction](/manual/LowLevel/Introduction.md), we have seen bindings
created for a named C `struct` (structure) storing a triple of integers:

```c
struct triple {
  int a;
  int b;
  int c;
};
```

```haskell
data Triple = Triple
  { triple_a :: CInt
  , triple_b :: CInt
  , triple_c :: CInt
  }

instance F.Storable Triple where ...
deriving stock instance Show Triple
deriving stock instance Eq Triple
```

## Structures with and without `typedef`

Adding a `typedef` matching the name of the structure, or using a `typedef` in
place of a structure name, does not change the generated bindings. For more
details, see the [section on name
generation](/manual/LowLevel/GeneratedNames.md).

```c
typedef struct triple triple;
```

Adding a `typedef` with a new name induces generation of a `newtype`.

```c
typedef struct triple triple_t;
```

```haskell
newtype Triple_t = Triple_t { un_Triple_t :: Triple }

deriving newtype instance F.Storable Triple_t
deriving stock instance Eq Triple_t
deriving stock instance Show Triple_t
```

## Nested structures

> [!NOTE] This section has many similarities to the [Nested unions
> section](./Unions.md#nested-unions). Consider reading both.

A nested structure is a structure inside another union or structure. Nested
structures can be declared _separately_ or in an _embedded_ way. If nested
structures are _embedded_, then they can either be _named_ or _anonymous_
structures.

For the most part the generated bindings are unsurprising regardless of whether
a nested structures is inside a union or a structure. For simplicity we will
almost exclusively show examples of nested structures inside structures in the
remainder of this section. The one exception where we consider structs and
unions separately has to do with _anonymous_ structures.

### Separate declaration

First we declare a named structure, and then refer to it from the declaration of
the enclosing structure:

```c
/* Separate declaration of named structure. */
struct door {
  float height;
  float width;
};

/* Use named structure in declaration of nested structure. */
struct room {
  struct door door1;
  struct door door2;
};
```

`hs-bindgen` generates the following bindings (instances omitted for brevity):

```haskell
data Door = Door
  { door_height :: CFloat
  , door_width :: CFloat
  }

data Room = Room
  { room_door1 :: Door
  , room_door2 :: Door
  }
```


### Embedded declaration (with variable name)

Embedded structures can have variable names:

```c
/* Declare nested structure in an embedded way. The embedded structure has a
   variable name.  */
struct aula1 {
  struct {
    float height;
    float width;
  } door;
  int n_doors;
};
```

`hs-bindgen` generates the following bindings (instances omitted for brevity):

```haskell
data Aula1_door = Aula1_door
  { aula1_door_height :: CFloat
  , aula1_door_width :: CFloat
  }

data Aula1 = Aula1
  { aula1_door :: Aula1_door
  , aula1_n_doors :: CInt
  }
```

### Embedded declaration (anonymous)

The definition of an anonymous structure is as follows (quoted from
[cppreference.com][cppreference:struct]):

[cppreference:struct]: https://en.cppreference.com/w/c/language/struct.html

> Similar to union, an unnamed member of a struct whose type is a struct without
> name is known as anonymous struct. Every member of an anonymous struct is
> considered to be a member of the enclosing struct or union, keeping their
> structure layout. This applies recursively if the enclosing struct or union is
> also anonymous.

Anonymous structures are sometimes used when defining nested structures in an
embedded wa. This is one such example:

```c
/* Declare nested structure in an embedded way. The embedded structure is
   anonymous. */
struct aula2 {
  struct {
    float door_height;
    float door_width;
  };
  int n_doors;
};
```

By the C reference's definition of an anonymous structure, the fields of the
nested structure can be accessed as if they were part of the enclosing
structure. The same would be valid if the enclosing structure were a union
instead. Sometimes, we refer to such fields as _implicit fields_.

```c
struct aula2 x;
x.door_height = 1; // valid
x.door_width = 2; // valid
x.n_doors = 3; // valid
```

Currently `libclang` [does not provide information about the offset and
alignment of implicit
fields](https://github.com/llvm/llvm-project/issues/122257). This is problematic
for binding generation, but to varying degrees depending on whether a nested
structure is enclosed by a union or a structure.

* **Nested structure inside an enclosing union**: the members of the enclosing
  union are stored at offset 0 with respect to the enclosing union. Meaning that
  a nested structure is also stored at offset 0, hence we do not need `libclang`
  to tell us what the offset of the nested structure field is.

* **Nested structure inside an enclosing structure**: the members of the
  enclosing structure are stored at different offsets with respect to the
  enclosing structure. Since we don't know these offsets, we can not generate
  bindings. We [plan to support implicit
  fields](https://github.com/well-typed/hs-bindgen/issues/682) for this scenario
  in the future.

## Bitfields

[_Bitfields_](https://www.geeksforgeeks.org/bit-fields-c/) are structures or
unions with elements of individual size. For example,

```c
struct aula_setup {
  char window_id;
  int tilt : 1;
  int close_blinds : 1;
  char projector_id;
  int power_mode : 2;
};
```

declares a structure with two member flags `tilt`, and `close_blinds`, covering
1 bit of memory, as well as a member `power_mode` covering two bits. The
generated Haskell bindings only expose the non-standard alignment of the C
structure in their `Storable` instance:

```haskell
data Aula_setup = Aula_setup
  { aula_setup_window_id :: CChar
  , aula_setup_tilt :: CInt
  , aula_setup_close_blinds :: CInt
  , aula_setup_projector_id :: CChar
  , aula_setup_power_mode :: CInt
  }

instance F.Storable Aula_setup where
  sizeOf = \_ -> (4 :: Int)
  alignment = \_ -> (4 :: Int)
  peek =
    \ptr0 ->
          pure Aula_setup
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (8 :: Int) (1 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (9 :: Int) (1 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (24 :: Int) (2 :: Int)
  poke = ...
deriving stock instance Show Aula_setup
deriving stock instance Eq Aula_setup
```

As you can see, the `hs-bindgen` runtime, which is required when the bindings
are used, provides some helper functions. For example,

```haskell
peekBitOffWidth :: Bitfield a => Ptr b -> Int -> Int -> IO a
peekBitOffWidth pointer offset width = ...
```
obtains the bitfield member of type `a` at the destination of the provided
`pointer` with `offset`, and `width`.

## Flexible array members

Members of unknown size can only appear at the end of structure declarations.
These are called [_flexible array members_
(FLAMs)](https://en.wikipedia.org/wiki/Flexible_array_member). For example,

```c
struct surname {
  int len;
  char data[];
};
```

Note: The `sizeof` C operator, when applied to a structure with a FLAM, gives
the size of the structure as if the FLAM were empty.

In a similar spirit, and since we do not know the length of the FLAM, the
generated Haskell data type only contains the fixed-size members of the
structure:

```haskell
data Surname = Surname
  { surname_len :: CInt
  }
```

We provide additional tools to handle the FLAM. First, the generated data type
is instance of `HasFlexibleArrayMember`:

```haskell
instance HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember CChar Surname where
  flexibleArrayMemberOffset = \_ty0 -> 4
```

Second, the user can define the length of the FLAM, if known:

```haskell
class HasFlexibleArrayMember element struct => HasFlexibleArrayLength element struct | struct -> element where
  flexibleArrayMemberLength :: struct -> Int
```

Let us define such an instance:

```haskell
instance HasFlexibleArrayLength CChar Surname where
  flexibleArrayMemberLength x = fromIntegral (surname_len x)
```

Then, we can use the FLAM-specific `peek` and `poke` functions `peekWithFLAM`,
and `pokeWithFLAM`. The type signatures specialized to `Surname` are:

```haskell
peekWithFLAM :: (Storable Surname, Storable CChar, HasFlexibleArrayLength CChar Surname)
  => Ptr Surname -> IO (WithFlexibleArrayMember CChar Surname)

pokeWithFLAM :: (Storable Surname, Storable CChar, HasFlexibleArrayLength CChar Surname)
  => Ptr Surname -> WithFlexibleArrayMember CChar Surname -> IO ()
```

where `WithFlexibleArrayMember` combines the structure with the FLAM:

```haskell
data WithFlexibleArrayMember element struct = WithFlexibleArrayMember
    { flamStruct :: struct
    , flamExtra  :: Vector element
    }
```

For example,
```haskell
bracket (withCString "Rich" $ \cstr -> surname_init cstr) surname_free $
  \ptr -> do
    (surname :: Surname) <- peek ptr
    putStrLn $ "The length of the surname is: " <> show (surname_len surname)
    (surnameWithFlam :: WithFlexibleArrayMember CChar Surname) <-
      FLAM.peekWithFLAM ptr
    let name :: Vector CChar
        name = FLAM.flamExtra surnameWithFlam
    print $ Vector.map castCCharToChar name
```

## Opaque structs

Opaque objects conceal their implementation details, providing an interface that
ensures specific constraints are maintained throughout the object's lifetime.
For example,

```c
struct square;

struct square create_square(double side_length);
```

Consequently, `hs-bindgen` generates an opaque data type, while also generating
bindings for the provided interface:

```haskell
data Square

foreign import ccall safe "Structs_create_square" create_square
  :: CDouble -> IO (Ptr Square)
```

Note that opaque types do not get a Storable instance, and therefore can not be
used by value.
