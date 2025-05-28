# Alternative union proposal

## Preliminaries

Pull out from functionality from `Storable` into separate classes (we already
need this for other purposes also):

```haskell
class StaticSize a where
  staticSizeOf    :: Proxy a -> Int
  staticAlignment :: Proxy a -> Int

class WriteRaw a where
  writeRaw :: Ptr a -> a -> IO ()

class ReadRaw a where
  readRaw :: Ptr a -> IO a
```

## Variation on 'ReadRaw' with additional context

We can write unions without difficulty, but we need to be told which variant to
expect when reading:

```haskell
class ReadRawWithCtxt ctxt a where
  readRawWithCtxt :: ctxt -> Ptr a -> IO a
```

### Generated instances

Suppose we have some C code

```c
typedef struct rectangle { .. } rectangle;
typedef struct circle    { .. } circle;

typedef union shape_details {
    rectangle rectangle;
    circle    circle;
} shape_details;
```

then `hs-bindgen` can generate these definitions:

```haskell
data Shape_details =
    ShapeRectangle Rectangle
  | ShapeCircle Circle

instance StaticSize Shape_details where
  staticSizeOf    _ = 16
  staticAlignment _ = 4

instance WriteRaw Shape_details where
  writeRaw p (ShapeRectangle x) = poke (castPtr p) x
  writeRaw p (ShapeCircle    x) = poke (castPtr p) x

instance ReadRawWithCtxt CUInt Shape_details where
  readRawWithCtxt 0 p = ShapeRectangle <$> peek (castPtr p)
  readRawWithCtxt 1 p = ShapeCircle    <$> peek (castPtr p)
  readRawWithCtxt n _ = error $ "Shape_details: invalid tag " ++ show n
```

### Usage

If we then have a C function such as

```c
void random_shape_details(int* tag, shape_details* details);
```

with Haskell binding

```haskell
foreign import capi safe "shape.h random_shape_details"
  random_shape_details :: Ptr CUInt -> Ptr Shape_details -> IO ()
```

we can call this function as follows

```haskell
getRandomShape :: IO Shape
getRandomShape =
    alloca       $ \ptrTag ->
    staticAlloca $ \ptrDetails -> do
      random_shape_details ptrTag ptrDetails
      tag     <- peek ptrTag
      details <- readRawWithCtxt tag ptrDetails
      return $ Shape (Shape_tag tag) details
```

## Enclosing contexts

Suppose we have

```c
typedef enum shape_tag {
    RECT = 0,
    CIRCLE
} shape_tag;

typedef struct shape {
    shape_tag     tag;
    shape_details details;
} shape;
```

### Generated instances

We can generate the Haskell types easily enough:

```haskell
newtype Shape_tag = Shape_tag {
      un_Shape_tag :: CUInt
    }

data Shape = Shape {
      shape_tag     :: Shape_tag
    , shape_details :: Shape_details
    }
```

In the `Storable` instance for `Shape` we can provide a partially constructed
`Shape` as the context:

```haskell
instance ReadRawWithCtxt Shape Shape_details => Storable Shape where
  sizeOf    = ..
  alignment = ..
  poke      = ..

  peek p = do
      shape_tag <- peekByteOff p 0
      let partial = Shape{shape_tag, shape_details = undefined}
      shape_details <- readRawWithCtxtOff partial p 4
      return Shape{shape_tag, shape_details}
```

(The `poke` definition is almost standard, except that we need to use
`writeRawOff` instead of `pokeByteOff`.)

### User-provided instances

This would require the user to handwrite an instance

```haskell
instance ReadRawWithCtxt Shape Shape_details where
  readRawWithCtxt = readRawWithCtxt . un_Shape_tag . shape_tag
```

## Disambiguation

There is an edge case we _might_ have to worry about though: what if the
enclosing struct contains _two_ instances of the _same_ union?

```c
typedef struct double_shape {
    shape_tag     tag1;
    shape_details details1;
    shape_tag     tag2;
    shape_details details2;
} double_shape;
```

This is probably quite rare, but in such cases we could define a wrapper to
distinguish between these different fields.

```haskell
newtype StructField (field :: Symbol) a = StructField a
```

### Generated instances

We can then make use of this wrapper in the `Storable` instance:

```haskell
instance ( ReadRawWithCtxt
             (StructField "double_shape.details1" Double_shape)
             Shape_details
         , ReadRawWithCtxt
             (StructField "double_shape.details2" Double_shape)
             Shape_details
         ) => Storable Double_shape where
  sizeOf    = ..
  alignment = ..
  poke      = ..

  peek p = do
      double_shape_tag1 <- peekByteOff p 0
      double_shape_tag2 <- peekByteOff p 20
      let partial = Double_shape{
              double_shape_tag1
            , double_shape_details1 = undefined
            , double_shape_tag2
            , double_shape_details2 = undefined
            }
          ctxt_details1 = StructField @"double_shape.details1" partial
          ctxt_details2 = StructField @"double_shape.details2" partial
      double_shape_details1 <- readRawWithCtxtOff ctxt_details1 p 4
      double_shape_details2 <- readRawWithCtxtOff ctxt_details2 p 24
      return Double_shape{
          double_shape_tag1
        , double_shape_details1
        , double_shape_tag2
        , double_shape_details2
        }
```

### User-provided instances

This then requires handwritten instances

```haskell
instance ReadRawWithCtxt
           (StructField "double_shape.details1" Double_shape)
           Shape_details where
  readRawWithCtxt (StructField partial) =
      readRawWithCtxt (un_Shape_tag $ double_shape_tag1 partial)

instance ReadRawWithCtxt
           (StructField "double_shape.details2" Double_shape)
           Shape_details where
  readRawWithCtxt (StructField partial) =
      readRawWithCtxt (un_Shape_tag $ double_shape_tag2 partial)
```











