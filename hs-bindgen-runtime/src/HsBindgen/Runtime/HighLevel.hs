-- | Lift a low-level FFI binding into a high-level Haskell wrapper.
--
-- Chain 'input' / 'output' / 'scratch' and a closer ('resultPure', 'resultIO', or
-- 'discardResult') with @($)@, then run with 'toHighLevel':
--
-- > hsStrncmp :: String -> ByteString -> IO Int
-- > hsStrncmp = toHighLevel ( input  withCStringIn
-- >                         $ input  useAsByteStringLenIn
-- >                         $ resultPure fromIntegral
-- >                         ) c_strncmp
--
-- Outputs and the kept result come back as a flat tuple, result last
-- (@output a $ output b $ resultPure f@ gives @IO (a, b, c)@). The marshalling
-- combinators live in "HsBindgen.Runtime.HighLevel.Marshaller" (ready-made ones
-- in "HsBindgen.Runtime.HighLevel.Marshaller.Utils").
--
-- "HsBindgen.Runtime.HighLevel.Defaults" fills the mundane positions: a default
-- conversion per type, and @auto@, which reads them straight off the signature. When
-- filling a spec against typed holes, prefer 'input1' \/ 'input2' \/ 'input3' (see
-- \"Building against typed holes\" below).
--
module HsBindgen.Runtime.HighLevel (
    -- * The 'ToHighLevel' type
    ToHighLevel
  , toHighLevel
    -- * Building a wrapper
  , input
  , input1
  , input2
  , input3
  , output
  , scratch
  , scratchArray
  , resultPure
  , resultIO
  , discardResult
    -- ** Building against typed holes
    -- $holes
    -- * Dropping a struct marshaller into a wrapper position
  , asArgument
  , asOutput
  , asResult
    -- ** A worked struct example
    -- $structs
    -- * Error-aware combinators
  , throwOn
  , throwOnNonZero
  , throwOnOut
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (void, (>=>))
import Data.Proxy (Proxy (Proxy))
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)

import HsBindgen.Runtime.HighLevel.Internal.Threading (ThreadIn (..),
                                                       ThreadOut (..))
import HsBindgen.Runtime.HighLevel.Marshaller (Marshal (..), MarshalStruct,
                                               UnmarshalStruct (..),
                                               Unmarshaller (..),
                                               runUnmarshalStruct, unmarshalOut,
                                               unmarshalOutWith, withStruct)
import HsBindgen.Runtime.Marshal (ReadRaw, StaticSize, WriteRaw, readRaw,
                                  staticAlignment, staticSizeOf)

{-------------------------------------------------------------------------------
  The ToHighLevel type
-------------------------------------------------------------------------------}

-- | A spec for lifting a low-level callable @lo@ into a high-level wrapper @hi@.
-- Read it as a recipe that turns @lo@ into @hi@: it is literally that @lo -> hi@
-- function, which 'toHighLevel' applies. Build it with 'input' / 'output' /
-- 'scratch' and a closer.
--
newtype ToHighLevel lo hi = ToHighLevel (lo -> hi)

-- | Apply a spec to a low-level callable, producing the wrapper.
toHighLevel :: ToHighLevel lo hi -> lo -> hi
toHighLevel (ToHighLevel f) = f
{-# INLINE toHighLevel #-}

{-------------------------------------------------------------------------------
  Building a wrapper
-------------------------------------------------------------------------------}

-- | Add one input at the head of the spec. The wrapper gains one argument @hs@;
-- the marshaller consumes the C argument(s) it needs off @lo@, leaving @lo'@.
--
-- @input@ takes a marshaller of /any/ arity, which is all a finished spec needs. But
-- a typed hole in an @input@ position (@input _@) is under-determined and infers
-- poorly, so when filling a spec against holes prefer 'input1' \/ 'input2' \/ 'input3'
-- for the common one-, two-, and three-argument marshallers (see \"Building against
-- typed holes\" below).
--
input
  :: ThreadIn lo' hi
  => Marshal hs lo lo'
  -> ToHighLevel lo' hi
  -> ToHighLevel lo (hs -> hi)
input (Marshal run) (ToHighLevel rest) =
  ToHighLevel $ \lo hs -> threadIn (run hs lo) rest
{-# INLINE input #-}

-- | 'input' for a marshaller that fills exactly one C argument, and the
-- inference-friendly form to reach for first. The arity in the type (the @c1 ->@)
-- ties the residual callable @lo'@ to the concrete low-level type, so a hole here
-- (@input1 _@) reports a fully determined marshaller type (GHC even offers the
-- matching marshaller as a valid hole fit), and every position after it stays
-- concrete too. It is 'input' with a pinned arity (@input1 = input@), so a finished
-- spec reads the same whichever you use.
--
input1
  :: ThreadIn lo' hi
  => Marshal hs (c1 -> lo') lo'
  -> ToHighLevel lo' hi
  -> ToHighLevel (c1 -> lo') (hs -> hi)
input1 = input
{-# INLINE input1 #-}

-- | 'input' for a marshaller that fills exactly two C arguments (the @(const char *,
-- size_t)@ shape of @useAsByteStringLenIn@, say). See 'input1' for why the fixed arity
-- helps inference and typed holes.
--
input2
  :: ThreadIn lo' hi
  => Marshal hs (c1 -> c2 -> lo') lo'
  -> ToHighLevel lo' hi
  -> ToHighLevel (c1 -> c2 -> lo') (hs -> hi)
input2 = input
{-# INLINE input2 #-}

-- | 'input' for a marshaller that fills exactly three C arguments (a matrix as
-- @(rows, cols, data)@, say). See 'input1' for why the fixed arity helps inference
-- and typed holes. Beyond three, use plain 'input' with an explicit marshaller type
-- (it handles any arity, at the cost of weaker inference in a hole).
--
input3
  :: ThreadIn lo' hi
  => Marshal hs (c1 -> c2 -> c3 -> lo') lo'
  -> ToHighLevel lo' hi
  -> ToHighLevel (c1 -> c2 -> c3 -> lo') (hs -> hi)
input3 = input
{-# INLINE input3 #-}

-- | Add one output at the head of the spec. The wrapper gains no argument; the
-- output's value joins the result tuple. A value that is itself a tuple is
-- flattened into the result tuple, so a @(Int, Int)@ output becomes two
-- components and shifts the rest; wrap it in a @newtype@ to keep it as one
-- component. The result tuple tops out at eight components.
--
output
  :: ThreadOut c hs hi hi'
  => Unmarshaller c hs
  -> ToHighLevel lo' hi
  -> ToHighLevel (c -> lo') hi'
output (Unmarshaller m) (ToHighLevel rest) =
  ToHighLevel $ \lo -> threadOut m $ \c -> rest (lo c)
{-# INLINE output #-}

-- | Add a scratch position: a bracket supplying one C argument the callee writes
-- into and the caller never sees. It contributes nothing to the wrapper type (no
-- argument, no result component). Most callers want 'scratchArray'. To hand in a
-- buffer you allocated yourself, wrap it as a trivial bracket: @scratch (\\k -> k buf)@.
--
scratch
  :: ThreadIn lo' hi
  => (forall r. (c -> IO r) -> IO r)
  -> ToHighLevel lo' hi
  -> ToHighLevel (c -> lo') hi
scratch br (ToHighLevel rest) =
  ToHighLevel $ \lo -> threadIn (\k -> br (\c -> k (lo c))) rest
{-# INLINE scratch #-}

-- | Allocate an @n@-element array of @a@ as a scratch C argument the callee writes
-- into and the caller never sees. @'scratchArray' n = 'scratch' ('allocaArray' n)@.
--
scratchArray
  :: (Storable a, ThreadIn lo' hi)
  => Int -> ToHighLevel lo' hi -> ToHighLevel (Ptr a -> lo') hi
scratchArray n = scratch (allocaArray n)
{-# INLINE scratchArray #-}

-- | Close the spec with a pure @c -> hs@ conversion of the C return value. The
-- converted value is the last tuple component, applied with 'fmap'.
--
resultPure :: (c -> hs) -> ToHighLevel (IO c) (IO hs)
resultPure f = ToHighLevel (fmap f)
{-# INLINE resultPure #-}

-- | Close the spec with an effectful @c -> 'IO' hs@ conversion of the C return
-- value, for a conversion that copies out memory, frees a pointer, and the like.
--
resultIO :: (c -> IO hs) -> ToHighLevel (IO c) (IO hs)
resultIO k = ToHighLevel $ \cFn -> cFn >>= k
{-# INLINE resultIO #-}

-- | Close the spec, discarding the C return value (the result is @()@). Reads
-- clearer than @resultPure (const ())@. For a C function that returns @void@,
-- @auto@ (or @defaultRes@) closes it through the @DefaultRes () ()@ instance; reach
-- for 'discardResult' when the C return carries a value you want to drop.
--
discardResult :: ToHighLevel (IO c) (IO ())
discardResult = ToHighLevel $ \cFn -> void cFn
{-# INLINE discardResult #-}

{-------------------------------------------------------------------------------
  Dropping a struct marshaller into a wrapper position
-------------------------------------------------------------------------------}

-- | Drop a 'MarshalStruct' into an 'input' position: the wrapper gains one @hi@ argument
-- and the C call receives a @'Ptr' s@. The struct is written into a zeroed slot
-- (see 'withStruct'), so padding reaches C as zeros.
--
asArgument
  :: (StaticSize s, WriteRaw s)
  => MarshalStruct hi s
  -> Marshal hi (Ptr s -> lo') lo'
asArgument sm =
  Marshal $ \hi lo k -> withStruct sm hi (\p -> k (lo p))
{-# INLINE asArgument #-}

-- | Drop an 'UnmarshalStruct' into an 'output' position: allocate a @'Ptr' struct@ for
-- the call to fill, then read the high-level value back. It frees no heap the
-- struct's fields point to, so a callee-allocated pointer field must be freed by
-- the 'UnmarshalStruct' (copy, then free, inside @unmarshalField@). The slot is
-- uninitialized, unlike the zeroed slot of 'asArgument', so the callee must fill
-- every field the 'UnmarshalStruct' reads.
--
asOutput
  :: forall struct hi. (StaticSize struct, ReadRaw struct)
  => UnmarshalStruct struct hi
  -> Unmarshaller (Ptr struct) hi
asOutput sm = unmarshalOutWith allocStruct (readRaw >=> runUnmarshalStruct sm)
  where
    allocStruct :: (Ptr struct -> IO r) -> IO r
    allocStruct =
      allocaBytesAligned (staticSizeOf (Proxy @struct)) (staticAlignment (Proxy @struct))
{-# INLINE asOutput #-}

-- | Close a spec whose C call returns the low-level struct by value: read the
-- high-level value out of the returned struct.
--
asResult :: UnmarshalStruct struct hi -> ToHighLevel (IO struct) (IO hi)
asResult sm = resultIO (runUnmarshalStruct sm)
{-# INLINE asResult #-}

{-------------------------------------------------------------------------------
  Error-aware combinators

  Every position runs in 'IO', so a check can sit wherever the error signal lives,
  and a throw unwinds every bracket opened before it. 'throwOnNonZero' is a closer,
  'throwOn' wraps an existing closer, and 'throwOnOut' is an output marshaller.
-------------------------------------------------------------------------------}

-- | Wrap a result closer with an error check: run it, then classify the converted
-- value. 'Left' throws, 'Right' yields the refined value. The check sees the
-- converted value (@throwOn check ('resultPure' 'fromIntegral')@ classifies the
-- 'Int', not the raw 'Foreign.C.Types.CInt') and may change the result type.
--
throwOn
  :: Exception e
  => (hs -> Either e hs')
  -> ToHighLevel (IO c) (IO hs)
  -> ToHighLevel (IO c) (IO hs')
throwOn classify (ToHighLevel close) = ToHighLevel $ \cFn -> do
  hs <- close cFn
  either throwIO pure (classify hs)
{-# INLINE throwOn #-}

-- | Close the spec, throwing when the C status code is non-zero and returning
-- @()@ otherwise.
--
throwOnNonZero
  :: (Eq c, Num c, Exception e)
  => (c -> e)
  -> ToHighLevel (IO c) (IO ())
throwOnNonZero mk = ToHighLevel $ \cFn -> do
  c <- cFn
  if c == 0 then pure () else throwIO (mk c)
{-# INLINE throwOnNonZero #-}

-- | An error-aware output: peek the C value, classify it, throw on 'Left',
-- otherwise keep the refined value. For out-parameters that signal failure
-- themselves (a NULL out-pointer, an errcode in the slot).
--
throwOnOut
  :: (Storable c, Exception e)
  => (c -> Either e hs)
  -> Unmarshaller (Ptr c) hs
throwOnOut classify = unmarshalOut (either throwIO pure . classify)
{-# INLINE throwOnOut #-}

{-------------------------------------------------------------------------------
  Haddock named sections
-------------------------------------------------------------------------------}

-- $holes
-- A spec is rarely written from a blank line. Write the wrapper's type first, then
-- grow the spec one position at a time: hold a hole @_@ in the position you are
-- filling and stub the rest of the chain with @undefined@, so the spec type-checks
-- while you work and GHC reports the current hole concretely.
--
-- Take @parse_int :: PtrConst CChar -> Ptr CInt -> IO CInt@ and a target
-- @hsParseInt :: String -> IO (Int, Int)@. The first C argument is one @const char *@,
-- so reach for 'input1' (its fixed arity ties the residual callable to the concrete C
-- type, which is what keeps the hole determined):
--
-- > hsParseInt = toHighLevel (input1 _ $ undefined) parse_int
-- >   -- Found hole: _ :: Marshal String (PtrConst CChar -> Ptr CInt -> IO CInt) (Ptr CInt -> IO CInt)
-- >   --   Valid hole fits include withCStringIn
--
-- Plain @input _@ in that slot leaves the residual open (the hole comes back
-- @Marshal String (...) lo'@ with @lo'@ ambiguous), so 'input1' \/ 'input2' \/ 'input3'
-- infer far better while a position is a hole. Fill it, then move the hole on, tail
-- still @undefined@:
--
-- > hsParseInt = toHighLevel (input1 withCStringIn $ output _ $ undefined) parse_int
-- >   -- Found hole: _ :: Unmarshaller (Ptr CInt) Int
--
-- The output hole is concrete even with the tail @undefined@: its value type is
-- recovered backwards from the result tuple. Replace the last @undefined@ with the
-- closer ('resultPure' \/ 'resultIO' \/ a default) and the spec is done.

-- $structs
-- A struct is two halves: a 'MarshalStruct' that writes a high-level value into the C
-- layout, and an 'UnmarshalStruct' that reads one back. Build each from the
-- "HsBindgen.Runtime.HighLevel.Marshaller" vocabulary, then drop it into a wrapper
-- with 'asArgument' (by-value argument), 'asOutput' (out-parameter), or 'asResult'
-- (by-value return). Take a nested pair of structs whose outer record carries a
-- @(name, name_len)@ string and a nullable @label@:
--
-- > struct Inner { int x; int y; };
-- > struct Outer { struct Inner pos; const char *name; size_t name_len;
-- >                const char *label;  /* nullable */   double weight; };
--
-- hs-bindgen emits the low-level structs (with their @StaticSize@ \/ @ReadRaw@ \/
-- @WriteRaw@ instances); you write the high-level types you want to expose:
--
-- > data Inner = Inner CInt CInt
-- > data Outer = Outer Inner (PtrConst CChar) CSize (PtrConst CChar) CDouble
-- >
-- > data InnerHi = InnerHi { ix :: Int, iy :: Int }
-- > data OuterHi = OuterHi
-- >   { oPos :: InnerHi, oName :: ByteString, oLabel :: Maybe String, oWeight :: Double }
--
-- The write side chains the fields in source order with @>>>@. @at@ aims each field
-- at its marshaller; @marshalNested@ inlines the sub-struct; @useAsByteStringLenIn@ fills the
-- @(name, name_len)@ pair from one 'Data.ByteString.ByteString'; @marshalOptional@ sends
-- 'Nothing' to a NULL @label@:
--
-- > innerIn :: MarshalStruct InnerHi Inner
-- > innerIn = struct Inner (at ix (scalar fromIntegral) >>> at iy (scalar fromIntegral))
-- >
-- > outerIn :: MarshalStruct OuterHi Outer
-- > outerIn = struct Outer
-- >   ( at oPos    (marshalNested innerIn)
-- >   >>> at oName   useAsByteStringLenIn
-- >   >>> at oLabel  (marshalOptional ($ nullCharPtr) withCStringIn)
-- >   >>> at oWeight (scalar realToFrac) )
-- >   where nullCharPtr = PtrConst.unsafeFromPtr nullPtr  -- a NULL const char *
--
-- The read side is an 'UnmarshalStruct', assembled under the high-level constructor with
-- @<$>@ \/ @<*>@. @unmarshalFieldPure@ \/ @unmarshalField@ read one field (@unmarshalField@ in 'IO'); @unmarshalOptional@
-- turns a NULL pointer into 'Nothing'; @unmarshalNested@ reads the sub-struct:
--
-- > innerOut :: UnmarshalStruct Inner InnerHi
-- > innerOut = InnerHi <$> unmarshalFieldPure (\(Inner x _) -> x) fromIntegral
-- >                    <*> unmarshalFieldPure (\(Inner _ y) -> y) fromIntegral
-- >
-- > outerOut :: UnmarshalStruct Outer OuterHi
-- > outerOut = OuterHi
-- >   <$> unmarshalNested    (\(Outer pos _ _ _ _) -> pos) innerOut
-- >   <*> unmarshalField     (\(Outer _ p n _ _) -> (p, n)) packName
-- >   <*> unmarshalOptional  (\(Outer _ _ _ lbl _) -> PtrConst.unsafeToPtr lbl) peekCString
-- >   <*> unmarshalFieldPure (\(Outer _ _ _ _ w) -> w) (\(CDouble d) -> d)
-- >   where packName (p, n) = BS.packCStringLen (PtrConst.unsafeToPtr p, fromIntegral n)
--
-- Drop them into a wrapper through the adapter that matches how C takes the struct:
--
-- > int          takes_outer(struct Outer *);  // by-value argument
-- > int          fill_inner(struct Inner *);   // out-parameter
-- > struct Inner make_inner(void);             // by-value return
--
-- > hsTakesOuter :: OuterHi -> IO Int
-- > hsTakesOuter = toHighLevel (input (asArgument outerIn) $ resultPure fromIntegral) c_takesOuter
-- >
-- > hsFillInner :: IO (InnerHi, Int)
-- > hsFillInner = toHighLevel (output (asOutput innerOut) $ resultPure fromIntegral) c_fillInner
-- >
-- > hsMakeInner :: IO InnerHi
-- > hsMakeInner = toHighLevel (asResult innerOut) c_makeInner
--
-- 'asArgument' writes the struct into a fresh zeroed slot (so padding reaches C as
-- zeros) and keeps the field brackets open across the call, so a @Just@ label still
-- points at live memory. To use a struct outside a wrapper, 'withStruct' writes one
-- and hands over the @Ptr@, and 'runUnmarshalStruct' reads one back.
