# Structs

In this section we will consider the details of translating [C
structures](https://en.wikipedia.org/wiki/Struct_(C_programming_language))
(`struct`s). The examples are available in the [C header file
`structs.h`](/manual/c/structs.h).

In the [Introduction](../Introduction.md), we have seen bindings
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
  deriving stock (Eq, Show)

instance F.Storable Triple where ...
```

## Structures with and without `typedef`

Adding a `typedef` matching the name of the structure, or using a `typedef` in
place of a structure name, does not change the generated bindings. For more
details, see the [section on name generation](01-GeneratedNames.md).

```c
typedef struct triple triple;
```

Adding a `typedef` with a new name induces generation of a `newtype`.

```c
typedef struct triple triple_t;
```

```haskell
newtype Triple_t = Triple_t { un_Triple_t :: Triple }
  deriving stock   (Eq, Show)
  deriving newtype (Storable)
```

## Nested structures

A nested structure is a structure inside another structure. Nested structures
can be declared _separately_ or in an _embedded_ way.

### Separate declaration

First we declare a named structure, and then refer to it from the declaration of
the nested structure:

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

creates the following bindings (instances omitted for brevity):

```haskell
data Door = Door
  { door_height :: CFloat
  , door_width  :: CFloat
  }

data Room = Room
  { room_door1 :: Door
  , room_door2 :: Door
  }
```

### Embedded declaration (anonymous)

Anonymous structures do not have a name nor a `typedef`. They are sometimes used
when defining nested structures in an embedded way:

```c
/* Declare nested structure in an embedded way. The embedded structure is
   anonymous. */
struct aula1 {
  struct {
    float door_height;
    float door_width;
  };
  int n_doors;
};
```

Sometimes, we refer to such fields as _implicit fields_. `libclang` [does not
provide enough information about the alignment of implicit
fields](https://github.com/llvm/llvm-project/issues/122257), and so `hs-bindgen`
does not support implicit fields yet. We [plan to support implicit
fields](https://github.com/well-typed/hs-bindgen/issues/659) in the future.

### Embedded declaration (with variable name)

Embedded structures can also have variable names:

```c
/* Declare nested structure in an embedded way. The embedded structure has a
   variable name.  */
struct aula2 {
  struct {
    float height;
    float width;
  } door;
  int n_doors;
};
```

`hs-bindgen` generates the following bindings (instances omitted for brevity):

```haskell
data Aula2_door = Aula2_door
  { aula2_door_height :: CFloat
  , aula2_door_width  :: CFloat
  }

data Aula2 = Aula2
  { aula2_door :: Aula2_door
  , aula2_n_doors :: CInt
  }
```

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
  { aula_setup_window_id    :: CChar
  , aula_setup_tilt         :: CInt
  , aula_setup_close_blinds :: CInt
  , aula_setup_projector_id :: CChar
  , aula_setup_power_mode   :: CInt
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
is instance of `HasFlam`:

```haskell
instance HsBindgen.Runtime.FLAM.HasFlam CChar Surname where
  flexibleArrayMemberOffset = \_ty0 -> 4
```

Second, the user can define the length of the FLAM, if known:

```haskell
class HasFlam element struct => HasFlexibleArrayLength element struct | struct -> element where
  flexibleArrayMemberLength :: struct -> Int
```

Let us define such an instance:

```haskell
instance HasFlexibleArrayLength CChar Surname where
  flexibleArrayMemberLength x = fromIntegral (surname_len x)
```

Then, we can use the FLAM-specific `peek` and `poke` functions `FLAM.peek`,
and `FLAM.poke`. The type signatures specialized to `Surname` are:

```haskell
peek :: (Storable Surname, Storable CChar, HasFlexibleArrayLength CChar Surname)
  => Ptr Surname -> IO (WithFlam CChar Surname)

poke :: (Storable Surname, Storable CChar, HasFlexibleArrayLength CChar Surname)
  => Ptr Surname -> WithFlam CChar Surname -> IO ()
```

where `WithFlam` combines the structure with the FLAM:

```haskell
data WithFlam element struct = WithFlam
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
    (surnameWithFlam :: WithFlam CChar Surname) <-
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

Note that opaque types do not get a `Storable` instance, and therefore can not be
used by value.
