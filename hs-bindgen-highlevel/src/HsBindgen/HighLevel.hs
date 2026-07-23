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
-- combinators live in "HsBindgen.HighLevel.Marshaller" (ready-made ones
-- in "HsBindgen.HighLevel.Marshaller.Utils").
--
-- "HsBindgen.HighLevel.Defaults" fills the mundane positions: a default
-- conversion per type, and @auto@, which reads them straight off the signature. When
-- filling a spec against typed holes, prefer 'input1' \/ 'input2' \/ 'input3' (see
-- \"Building against typed holes\" below).
--
module HsBindgen.HighLevel (
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
  , fixed
  , resultPure
  , resultIO
  , discardResult
  , dropTrailingUnit
    -- ** Building against typed holes
    -- $holes
    -- ** Reading the type errors
    -- $errors
    -- * Dropping a struct marshaller into a wrapper position
  , asArgument
  , asArgumentC
  , asOutput
  , asResult
    -- ** A worked struct example
    -- $structs
    -- * Error-aware combinators
  , throwOn
  , throwOnNonZero
  , throwOnOut
    -- * Exposing a deterministic call as pure
    --
    -- | 'assertPure' exposes a wrapper over a deterministic C call as a pure
    -- function. See "HsBindgen.HighLevel.Result".
  , Purify
  , Purifiable (..)
  , assertPure
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (void, (>=>))
import Data.Proxy (Proxy (Proxy))
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)

import HsBindgen.Runtime.Marshal (ReadRaw, StaticSize, WriteRaw, readRaw,
                                  staticAlignment, staticSizeOf)
import HsBindgen.Runtime.PtrConst (PtrConst)

import HsBindgen.HighLevel.Internal.Spec (ToHighLevel (..), toHighLevel)
import HsBindgen.HighLevel.Internal.Threading (DropUnit (..), ThreadIn (..),
                                               ThreadOut (..))
import HsBindgen.HighLevel.Marshaller (Marshal (..), MarshalStruct,
                                       UnmarshalStruct (..), Unmarshaller (..),
                                       asConstArg, runUnmarshalStruct,
                                       unmarshalOut, unmarshalOutWith,
                                       withStruct)
import HsBindgen.HighLevel.Result (Purifiable (..), Purify, assertPure)

{-------------------------------------------------------------------------------
  Building a wrapper
-------------------------------------------------------------------------------}

-- | Add one input at the head of the spec. The wrapper gains one argument @hs@;
-- the marshaller consumes the C argument(s) it needs off @lo@, leaving @lo'@.
--
-- @input@ takes a marshaller of /any/ arity, which is all a finished spec needs. But
-- a hole in an @input@ position (@input _@) comes back under-determined, so when
-- filling a spec against holes prefer 'input1' \/ 'input2' \/ 'input3' for the common
-- one-, two-, and three-argument marshallers (see \"Building against typed holes\"
-- below).
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
-- inference-friendly form to reach for first: with a hole here (@input1 _@) GHC
-- reports a fully determined marshaller type (and can even offer the matching
-- marshaller as a valid hole fit), and every position after it stays concrete too. A
-- finished spec reads the same whether written with 'input1' or 'input'.
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
-- output's value is prepended as one component of the result tuple. Chaining
-- outputs builds a flat tuple (@output a $ output b $ ...@ gives @(a, b, ...)@,
-- not nested pairs); an output whose own value is a tuple stays a single
-- component of that tuple. The result tuple tops out at eight components.
--
-- For an /unlifted/ by-value out-parameter (a @W@ struct buffer) see
-- @outputUnlifted@ in "HsBindgen.HighLevel.Unlifted".
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

-- | Supply a constant (or otherwise fixed) C argument the wrapper does not expose.
-- Where 'scratch' hides a buffer the callee fills, 'fixed' hides an argument you pin
-- to a chosen value (a flags word, a NULL callback, a context handle).
--
fixed :: ThreadIn lo' hi => c -> ToHighLevel lo' hi -> ToHighLevel (c -> lo') hi
fixed c = scratch (\k -> k c)
{-# INLINE fixed #-}

-- | Close the spec with a pure @c -> hs@ conversion of the C return value. The
-- converted value is the last tuple component, applied with 'fmap'.
--
-- Also how a status is classified: @resultPure (== 0)@ closes an @int@ to a 'Bool',
-- @resultPure (\\c -> if c == 0 then Just () else Nothing)@ to a 'Maybe'.
--
resultPure :: (c -> hs) -> ToHighLevel (IO c) (IO hs)
resultPure f = ToHighLevel (fmap f)
{-# INLINE resultPure #-}

-- | Close the spec with an effectful @c -> 'IO' hs@ conversion of the C return
-- value, for a conversion that copies out memory, frees a pointer, and the like.
--
resultIO :: (c -> IO hs) -> ToHighLevel (IO c) (IO hs)
resultIO k = ToHighLevel (>>= k)
{-# INLINE resultIO #-}

-- | Close the spec, discarding the C return value; the result is @()@. Alone it
-- makes an @IO ()@ wrapper; beside outputs it leaves a trailing @()@ in the tuple
-- (@output a $ discardResult@ gives @IO (a, ())@), which 'dropTrailingUnit' removes.
-- Reach for it when the C return carries a value you want to drop.
--
discardResult :: ToHighLevel (IO c) (IO ())
discardResult = ToHighLevel void
{-# INLINE discardResult #-}

-- | Drop the trailing @()@ a void closer leaves beside outputs. A void closer
-- ('discardResult', 'throwOnNonZero', or 'throwOn' over one) contributes @()@ as the
-- final result component, so @output a $ output b $ discardResult@ has result
-- @(a, b, ())@. Wrapping the finished spec in 'dropTrailingUnit' drops it to @(a, b)@;
-- a lone @(a, ())@ collapses to @a@. It threads under any remaining wrapper arguments
-- and works at any width, so it replaces an ad-hoc 'fst'.
--
-- Apply it at the head of the finished spec:
--
-- > repositoryOpen = toHighLevel
-- >   (dropTrailingUnit $ output outHandle $ input1 pathIn $ throwStatus) c_open
-- >   -- :: String -> IO Repository, not IO (Repository, ())
--
dropTrailingUnit :: DropUnit hi hi' => ToHighLevel lo hi -> ToHighLevel lo hi'
dropTrailingUnit (ToHighLevel f) = ToHighLevel (dropUnit . f)
{-# INLINE dropTrailingUnit #-}

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

-- | Drop a 'MarshalStruct' into a @const T *@ argument position, the @const@ form of
-- 'asArgument': it fills a non-@const@ @'Ptr' s@ and 'asConstArg' retags it @const@,
-- so one struct marshaller serves a C argument typed @const T *@.
--
asArgumentC
  :: (StaticSize s, WriteRaw s)
  => MarshalStruct hi s
  -> Marshal hi (PtrConst s -> lo') lo'
asArgumentC = asConstArg . asArgument
{-# INLINE asArgumentC #-}

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

-- | Close the spec, throwing when the C status code is non-zero; the result is
-- @()@ (a trailing filler beside outputs, removed with 'dropTrailingUnit').
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
-- so reach for 'input1':
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

-- $errors
-- The combinators lean on the type checker, so a spec that does not line up shows
-- up as a type error, not a runtime failure. Four come up often, each with a short
-- fix.
--
-- @No default input marshaller for type ...@ (or @output@ \/ @result@) means @auto@,
-- or an explicit 'HsBindgen.HighLevel.Defaults.defaultIn' \/ @defaultOut@ \/
-- @defaultRes@, met a type with no default. Pass an explicit marshaller for that
-- position, or give the type its own default in one instance (see
-- 'HsBindgen.HighLevel.Defaults.DefaultIn').
--
-- @auto cannot line the high-level type up with the C function@ means @auto@ ran
-- out of high-level arguments while C still expects one. That leftover C argument
-- is one the wrapper does not expose: give it an explicit 'output' or 'scratch', or
-- add the missing argument to the wrapper's signature. @auto@ fills inputs and the
-- closer, nothing else.
--
-- @This wrapper builds more than eight result components@ means the flat result
-- tuple overflowed. It holds the kept return value plus up to seven outputs. Combine
-- outputs into a struct or a @newtype@-wrapped tuple, or drop a write-only one with
-- 'scratch'.
--
-- An ambiguous result type usually means the closer default has nothing to resolve
-- against: 'HsBindgen.HighLevel.Defaults.defaultRes' reads the result off
-- the signature, so a wrapper written without a result annotation cannot pick one.
-- Give every wrapper a type signature.
--
-- A hole in an @input@ slot that comes back with an ambiguous residual is not an
-- error, only under-determined. Switch to 'input1' \/ 'input2' \/ 'input3', whose
-- fixed arity pins it (see above).

-- $structs
-- A struct is two halves: a 'MarshalStruct' that writes a high-level value into the C
-- layout, and an 'UnmarshalStruct' that reads one back. Build each from the
-- "HsBindgen.HighLevel.Marshaller" vocabulary, then drop it into a wrapper
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
