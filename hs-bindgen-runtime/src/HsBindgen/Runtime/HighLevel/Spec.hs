{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NoFieldSelectors   #-}

-- | A value-level DSL for lifting low-level FFI bindings to high-level
-- Haskell wrappers.
--
-- A binding spec @'Spec' lo hi@ is a recipe for turning a low-level callable
-- of type @lo@ into a high-level wrapper of type @hi@. The spec is a small
-- GADT (§ "The Spec GADT") with three constructors: 'Done' closes the spec
-- by converting the C return value; 'In' and 'Out' each add /one position/
-- at the head of the inner spec. The companion 'toHighLevel' function
-- pattern-matches the GADT to materialise the wrapper.
--
-- See @PROPOSAL.md@ at the project root for the full design rationale.
--
-- == Quick example
--
-- Given a generated low-level binding
--
-- @
-- parse_int :: 'PtrConst' 'CChar' -> 'Ptr' 'CInt' -> 'IO' 'CInt'
-- @
--
-- the corresponding high-level wrapper @String -> IO (Int, Int)@ (parsed
-- value plus status code) reads top-down in C-arg order:
--
-- @
-- hsParseInt :: 'String' -> 'IO' ('Int', 'Int')
-- hsParseInt = 'toHighLevel' ( 'input'  'withCStringIn'                  -- 1st C arg: const char* \<- String
--                          $ 'output' ('peekOut' int)              -- 2nd C arg: int*       -> Int
--                          $ 'result' ('pureRes' 'fromIntegral')   -- C return:  int        -> Int
--                          ) parse_int
--   where int (CInt n) = pure (fromIntegral n)
-- @
--
-- == Composition order
--
-- Combinators chain right-to-left ('$' is right-associative): every
-- combinator wraps the spec to its right by adding one position at the
-- head. The result is that the user reads the spec top-to-bottom in
-- C-argument order while the type-level shape is built bottom-up.
--
-- == Free interleaving of inputs and outputs
--
-- 'input' and 'output' combinators may appear in any order in the
-- chain — the chain shape matches C-arg order. The 'ThreadIn'
-- constraint on 'In' and the 'ThreadOut' constraint on 'Out' both
-- walk past any Hs args the inner spec has already accumulated,
-- firing their bracket at the innermost 'IO'. C APIs with
-- interleaved outputs (\"out, in, out\") map directly to specs of
-- the same shape.
--
-- == Multi-output result shape
--
-- Each 'output' prepends one component to the inner 'IO''s payload.
-- With /N/ outputs plus a 'result', the wrapper returns a
-- /right-associative/ nested tuple of N+1 components — e.g.
-- @IO (a, (b, c))@ for two outputs plus a result. For up to two
-- outputs this is unobtrusive; for more, users may want to fmap a
-- flattener at the call site.
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.HighLevel.Spec qualified as Spec
module HsBindgen.Runtime.HighLevel.Spec (
    -- * The Spec GADT
    Spec (..)
    -- * Running a spec
  , toHighLevel
    -- * Smart constructors
    --
    -- $smart-constructors
  , input
  , output
  , result
  , discardRes
    -- * Marshaller types
    --
    -- $marshaller-types
  , InMarshaller (..)
  , OutMarshaller (..)
  , ResMarshaller (..)
    -- * Marshaller combinators
  , mapOutM
    -- * Multi-argument slots
    --
    -- $multi-arg-slots
  , Slot2 (..)
  , Slot3 (..)
  , Slot4 (..)
    -- * Standard input marshallers
  , pureIn
  , withCStringIn
  , useAsByteStringLenIn
  , nullableIn
  , funPtrIn
    -- * Standard output marshallers
  , allocaOut
  , peekOut
    -- * Standard result marshallers
  , pureRes
  , ioRes
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (Storable, peek)

import HsBindgen.Runtime.HighLevel.Call (Spread (..))
import HsBindgen.Runtime.HighLevel.ToC (Nullable (..))
import HsBindgen.Runtime.Internal.FunPtr (ToFunPtr, withFunPtr)
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

{-------------------------------------------------------------------------------
  Marshaller record types
-------------------------------------------------------------------------------}

-- $marshaller-types
--
-- Three record types, one per role. The /slot/ parameter is the C-side
-- shape this marshaller fills: a single type for single-arg slots, or a
-- 'Slot2' \/ 'Slot3' \/ 'Slot4' constructor for the
-- c2hs @&@ pattern (one Haskell value spanning multiple C arguments).

-- | Carries a Haskell value of type @hs@ to a C-side @slot@ value under
-- bracket semantics (the slot is valid only inside the continuation).
--
-- Build one directly when no standard marshaller fits:
--
-- @
-- doubleAsCFloat :: 'InMarshaller' 'Double' 'CFloat'
-- doubleAsCFloat = 'InMarshaller' $ \\d k -> k (realToFrac d)
-- @
data InMarshaller hs slot = InMarshaller
  { withIn :: forall r. hs -> (slot -> IO r) -> IO r }

-- | Allocates a C-side @slot@, runs a continuation against it, and peeks
-- an @hs@ value back. The continuation's result is paired with the peeked
-- value.
--
-- Build one directly when no standard marshaller fits:
--
-- @
-- intOut :: 'OutMarshaller' 'Int' ('Ptr' 'CInt')
-- intOut = 'OutMarshaller' $ \\k -> 'alloca' $ \\p -> do
--   r <- k p
--   c <- 'peek' p
--   pure ('fromIntegral' c, r)
-- @
--
-- Use 'allocaOut' for the trivial case (slot is a single @'Ptr' c@) and
-- 'peekOut' when the peeked C value needs effectful conversion.
data OutMarshaller hs slot = OutMarshaller
  { withOutM :: forall r. (slot -> IO r) -> IO (hs, r) }

-- | Converts the C return value @c@ to a Haskell value @hs@, effectfully.
-- The pure case @c -> hs@ is covered by 'pureRes', a one-line wrapper
-- around 'ioRes' that lifts the conversion with 'pure'.
newtype ResMarshaller c hs = ResMarshaller
  { runResM :: c -> IO hs }

-- | Map an effectful function over the result of a 'ResMarshaller'.
--
-- @
-- 'fmap' show ('pureRes' fromIntegral :: 'ResMarshaller' 'CInt' 'Int')
--   :: 'ResMarshaller' 'CInt' 'String'
-- @
instance Functor (ResMarshaller c) where
  fmap f (ResMarshaller k) = ResMarshaller (fmap f . k)

-- | Map an effectful function over the @hs@ produced by an 'OutMarshaller'.
--
-- Use it to layer a post-call conversion onto a slot-allocating
-- marshaller. The standard 'peekOut' is exactly @'mapOutM' f 'allocaOut'@:
--
-- @
-- peekCIntAsInt :: 'OutMarshaller' 'Int' ('Ptr' 'CInt')
-- peekCIntAsInt = 'mapOutM' (\\(CInt n) -> pure ('fromIntegral' n)) 'allocaOut'
-- @
mapOutM
  :: (hs -> IO hs')
  -> OutMarshaller hs  slot
  -> OutMarshaller hs' slot
mapOutM f (OutMarshaller m) = OutMarshaller $ \k -> do
  (hs, r) <- m k
  hs'     <- f hs
  pure (hs', r)

{-------------------------------------------------------------------------------
  Multi-argument slot data types
-------------------------------------------------------------------------------}

-- $multi-arg-slots
--
-- When a single Haskell value spans multiple consecutive C arguments
-- (c2hs's @&@ pattern), the marshaller's @slot@ parameter is a
-- multi-argument data constructor below. These appear /only/ inside
-- marshaller types — never in user-facing high-level signatures.

-- | Two C arguments treated as one logical slot. The 'Spread' instance
-- below applies @'Slot2' a b@ to a curried function @a -> b -> r@.
data Slot2 a b = Slot2 a b

-- | Three C arguments as one slot.
data Slot3 a b c = Slot3 a b c

-- | Four C arguments as one slot.
data Slot4 a b c d = Slot4 a b c d

-- 'Spread' (from "HsBindgen.Runtime.HighLevel.Call") applies a slot
-- value to a curried function, consuming one C arg per slot. The
-- 'Slot<N>' instances below live in this module — not orphans, because
-- the data types are also defined here.

instance Spread (Slot2 a b) (a -> b -> lo) lo where
  spread (Slot2 a b) f = f a b

instance Spread (Slot3 a b c) (a -> b -> c -> lo) lo where
  spread (Slot3 a b c) f = f a b c

instance Spread (Slot4 a b c d) (a -> b -> c -> d -> lo) lo where
  spread (Slot4 a b c d) f = f a b c d

{-------------------------------------------------------------------------------
  ThreadIn and ThreadOut: dual bracket-threaders
-------------------------------------------------------------------------------}

-- $threaders
--
-- 'ThreadIn' and 'ThreadOut' are the load-bearing piece of the design.
-- They exist because combinators are applied /right-to-left/: by the
-- time an 'input' or 'output' fires at runtime, the inner spec may
-- already have accumulated zero or more Hs args. The marshaller's
-- bracket (carried by 'InMarshaller' for 'In' and 'OutMarshaller' for
-- 'Out') must defer until those args have been supplied at the call
-- site, then fire at the innermost 'IO'. That deferral is structural
-- recursion on @(->)@.
--
-- The two classes are duals: same recursion shape, two-instance
-- structure, but the bracket produces @'IO' r@ for 'ThreadIn' versus
-- @'IO' (hs, r)@ for 'ThreadOut'. The difference of one tuple
-- component is what threads an output's contribution to the result
-- tuple at the correct nesting depth.

-- | Thread an input bracket through a function-arrow chain, firing at
-- the innermost 'IO'. Dual of 'ThreadOut'; see § "$threaders" above.
class ThreadIn slot hi where
  threadIn :: (forall r. (slot -> IO r) -> IO r) -> (slot -> hi) -> hi

instance ThreadIn slot (IO r) where
  threadIn br f = br f

instance ThreadIn slot rest
       => ThreadIn slot (arg -> rest) where
  threadIn br f = \arg -> threadIn br (\s -> f s arg)

-- | Thread an output bracket through a function-arrow chain, firing at
-- the innermost 'IO' and prepending @hs@ to its payload.
--
-- Dual of 'ThreadIn': the bracket produces @'IO' (hs, r)@ instead of
-- @'IO' r@, and the result type @hi'@ is @hi@ with @hs@ prepended at the
-- innermost 'IO'.
class ThreadOut slot hs hi hi' | slot hs hi -> hi' where
  threadOut :: (forall r. (slot -> IO r) -> IO (hs, r))
            -> (slot -> hi) -> hi'

instance ThreadOut slot hs (IO r) (IO (hs, r)) where
  threadOut br f = br f

instance ThreadOut slot hs rest rest'
       => ThreadOut slot hs (arg -> rest) (arg -> rest') where
  threadOut br f = \arg -> threadOut br (\s -> f s arg)

{-------------------------------------------------------------------------------
  The Spec GADT
-------------------------------------------------------------------------------}

-- | A binding spec. @'Spec' lo hi@ is a recipe for turning a low-level
-- callable of type @lo@ into a high-level wrapper of type @hi@.
--
-- Each constructor adds /one position/ at the head of the inner spec. The
-- type indices track exactly what has been consumed from the C signature
-- and what shape is being built on the Haskell side.
--
-- Users normally write specs via the lowercase smart constructors
-- 'input', 'output', 'result', 'discardRes' chained with @$@.
data Spec lo hi where
  -- | Close a spec by converting the C return value.
  Done
    :: ResMarshaller c hs
    -> Spec (IO c) (IO hs)

  -- | Add an input slot at the head of the C signature.
  --
  -- The 'Spread' constraint consumes the slot's C arguments (one for a
  -- single slot, N for a 'Slot<N>' slot). The 'ThreadIn'
  -- constraint walks past any Hs args the inner spec has already
  -- accumulated and fires the input bracket at the innermost 'IO'.
  In
    :: (Spread slot lo lo', ThreadIn slot rest)
    => InMarshaller hs slot
    -> Spec lo' rest
    -> Spec lo (hs -> rest)

  -- | Add an output slot at the head of the C signature.
  --
  -- The 'Spread' constraint consumes the slot's C arguments; the
  -- 'ThreadOut' constraint walks past any remaining Hs args in the
  -- inner spec and prepends @hs@ to the innermost 'IO''s payload.
  -- This lets 'output' appear anywhere in the chain — including
  -- between two 'input's.
  Out
    :: (Spread slot lo lo', ThreadOut slot hs rest rest')
    => OutMarshaller hs slot
    -> Spec lo' rest
    -> Spec lo  rest'

{-------------------------------------------------------------------------------
  Running a spec
-------------------------------------------------------------------------------}

-- | Apply a spec to a low-level callable, producing the high-level wrapper.
--
-- The 'Spread' constraint on each 'In' and 'Out' branch comes into scope
-- via the GADT pattern match, which is what lets 'spread' typecheck.
-- Both branches thread their bracket through any remaining Hs arguments:
-- 'In' via 'ThreadIn', 'Out' via the dual 'ThreadOut' (which additionally
-- prepends the output's @hs@ to the inner @IO@'s payload).
toHighLevel :: Spec lo hi -> lo -> hi
toHighLevel (Done (ResMarshaller k))     cFn = cFn >>= k
toHighLevel (In  (InMarshaller  m) spec) cFn = \hs -> threadIn  (m hs)
                                             $ \slot -> toHighLevel spec (spread slot cFn)
toHighLevel (Out (OutMarshaller m) spec) cFn =        threadOut  m
                                             $ \slot -> toHighLevel spec (spread slot cFn)

{-------------------------------------------------------------------------------
  Smart constructors
-------------------------------------------------------------------------------}

-- $smart-constructors
--
-- The four smart constructors below are trivial wrappers around the GADT
-- constructors (@'input' = 'In'@ etc.). They exist so that user-written
-- specs read in lowercase, matching the convention of most combinator
-- libraries. The GADT constructors are exported as well, for users who
-- want to fold over a spec (introspection or alternate interpreters).

-- | Wrap an inner spec by adding one input slot at the head. See 'In'.
input
  :: (Spread slot lo lo', ThreadIn slot rest)
  => InMarshaller hs slot
  -> Spec lo' rest
  -> Spec lo (hs -> rest)
input = In

-- | Wrap an inner spec by adding one output slot at the head. See 'Out'.
output
  :: (Spread slot lo lo', ThreadOut slot hs rest rest')
  => OutMarshaller hs slot
  -> Spec lo' rest
  -> Spec lo  rest'
output = Out

-- | Close a spec by converting the C return. See 'Done'.
result :: ResMarshaller c hs -> Spec (IO c) (IO hs)
result = Done

-- | Close a spec by discarding the C return value.
--
-- Derived combinator: @discardRes = 'result' ('pureRes' ('const' ()))@.
discardRes :: Spec (IO c) (IO ())
discardRes = result (pureRes (const ()))

{-------------------------------------------------------------------------------
  Standard input marshallers
-------------------------------------------------------------------------------}

-- | An input marshaller that applies a pure @hs -> c@ conversion at the
-- boundary. Use when no allocation or bracket is needed.
--
-- @
-- intToCSize :: 'InMarshaller' 'Int' 'CSize'
-- intToCSize = 'pureIn' 'fromIntegral'
-- @
pureIn :: (hs -> c) -> InMarshaller hs c
pureIn f = InMarshaller $ \x k -> k (f x)

-- | Marshal a 'String' as a NUL-terminated @const char *@ via
-- 'withCString'. The C side must not retain the pointer past the call.
withCStringIn :: InMarshaller String (PtrConst CChar)
withCStringIn = InMarshaller $ \s k ->
  withCString s (k . PtrConst.unsafeFromPtr)

-- | Marshal a 'ByteString' as a @(const char *, size_t)@ pair via
-- 'BS.useAsCStringLen' — c2hs's @&@ pattern.
useAsByteStringLenIn
  :: InMarshaller ByteString (Slot2 (PtrConst CChar) CSize)
useAsByteStringLenIn = InMarshaller $ \bs k ->
  BS.useAsCStringLen bs $ \(p, n) ->
    k (Slot2 (PtrConst.unsafeFromPtr p) (fromIntegral n))

-- | Lift any single-pointer-slot input marshaller to accept 'Maybe',
-- with 'Nothing' mapping to the slot's null value.
--
-- The 'Nullable' constraint (from "HsBindgen.Runtime.HighLevel.ToC")
-- restricts this to pointer-shaped slots — @'nullableIn' ('pureIn'
-- 'fromIntegral' :: 'InMarshaller' 'Int' 'CInt')@ is a deliberate
-- compile error, since there is no sensible null for an integer.
--
-- @
-- hsCheckPresent :: 'Maybe' 'String' -> 'IO' 'Bool'
-- hsCheckPresent = 'toHighLevel' ( 'input'  ('nullableIn' 'withCStringIn')
--                              $ 'result' ('pureRes' (/= 0))
--                              ) c_check_present
-- @
nullableIn
  :: Nullable slot
  => InMarshaller hs slot
  -> InMarshaller (Maybe hs) slot
nullableIn (InMarshaller m) = InMarshaller $ \mhs k -> case mhs of
  Nothing -> k nullValue
  Just hs -> m hs k

-- | Marshal a Haskell function as a C function pointer, with bracketed
-- lifetime via 'withFunPtr' — the 'FunPtr' is freed when the foreign
-- call returns.
--
-- Suitable for callbacks invoked /during/ the call (@qsort_r@-style).
-- For \"register a callback\" APIs that retain the pointer past the
-- call, write a leaked variant that drops the cleanup:
--
-- @
-- funPtrLeaked :: 'ToFunPtr' a => 'InMarshaller' a ('FunPtr' a)
-- funPtrLeaked = 'InMarshaller' $ \\f k -> 'toFunPtr' f '>>=' k
-- @
funPtrIn :: ToFunPtr a => InMarshaller a (FunPtr a)
funPtrIn = InMarshaller withFunPtr

{-------------------------------------------------------------------------------
  Standard output marshallers
-------------------------------------------------------------------------------}

-- | Allocate a single C-side slot, run the call, peek the value back.
-- The peeked value is returned unchanged.
--
-- For a post-call conversion (the common case where the Haskell type
-- differs from the C type), use 'peekOut' instead.
allocaOut :: Storable c => OutMarshaller c (Ptr c)
allocaOut = OutMarshaller $ \k -> alloca $ \p -> do
  r <- k p
  c <- peek p
  pure (c, r)

-- | Allocate a slot, run the call, peek the value, then apply an effectful
-- conversion @c -> 'IO' hs@.
--
-- Equivalent to @'mapOutM' f 'allocaOut'@; provided as a single-step
-- alias because the @peek-then-convert@ pattern is by far the most
-- common shape for out-parameters.
peekOut :: Storable c => (c -> IO hs) -> OutMarshaller hs (Ptr c)
peekOut f = mapOutM f allocaOut

{-------------------------------------------------------------------------------
  Standard result marshallers
-------------------------------------------------------------------------------}

-- | A result marshaller that applies a pure @c -> hs@ conversion.
--
-- Derived from 'ioRes' by lifting @f@ into 'IO' via 'pure'.
--
-- @
-- 'pureRes' 'fromIntegral' :: 'ResMarshaller' 'CInt' 'Int'
-- @
pureRes :: (c -> hs) -> ResMarshaller c hs
pureRes f = ioRes (pure . f)

-- | A result marshaller built from an effectful @c -> 'IO' hs@ conversion.
-- Use for results that need to copy out memory, throw exceptions, or
-- otherwise sequence effects after the foreign call returns.
--
-- @
-- -- Wrap a freshly-malloc'd buffer into a ByteArray, with a copy:
-- 'ioRes' bufferToByteArray :: 'ResMarshaller' ('Ptr' Void) ByteArray
-- @
ioRes :: (c -> IO hs) -> ResMarshaller c hs
ioRes = ResMarshaller
