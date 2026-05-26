{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | A small DSL for lifting low-level FFI bindings to high-level Haskell
-- wrappers. @'Refine' lo hi@ describes how to refine a low-level callable
-- of type @lo@ into a high-level wrapper of type @hi@; 'toHighLevel'
-- interprets it.
module HsBindgen.Runtime.HighLevel.Refine (
    -- * The 'Refine' GADT
    Refine (..)
    -- * Running a refinement
  , toHighLevel
    -- * Smart constructors
  , input
  , output
  , result
  , discardRes
    -- * Marshaller types
  , InMarshaller (..)
  , OutMarshaller (..)
  , ResMarshaller (..)
    -- * Slot dispatch
  , Spread (..)
    -- * Nullable pointer-shaped slots
  , Nullable (..)
    -- * Standard input marshallers
  , pureIn
  , withCStringIn
  , useAsByteStringLenIn
  , withConstIncompleteArrayIn
  , nullableIn
  , funPtrIn
    -- * Standard output marshallers
  , allocaOut
  , peekOut
  , peekCStringOut
  , peekIncompleteArrayOut
  , scratchOut
    -- * Standard result marshallers
  , pureRes
  , ioRes
    -- * Error-aware result marshallers
  , throwOn
  , throwOnNonZero
  ) where

import Control.Exception (Exception, throwIO)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor.Contravariant (Contravariant (..))
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CChar, CSize)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (Storable, peek)

import HsBindgen.Runtime.IncompleteArray (IncompleteArray)
import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.Internal.FunPtr (ToFunPtr, withFunPtr)
import HsBindgen.Runtime.IsArray qualified as IsA
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

{-------------------------------------------------------------------------------
  Marshaller record types
-------------------------------------------------------------------------------}

-- | Bracket a Haskell value into a C-side slot: the slot is valid only inside
-- the continuation.
data InMarshaller slot hs = InMarshaller
  { runIn :: forall r. hs -> (slot -> IO r) -> IO r }

instance Contravariant (InMarshaller slot) where
  contramap f (InMarshaller m) = InMarshaller $ \hs k -> m (f hs) k

-- | Allocate a C-side slot, run the call, peek a Haskell value back. The
-- peeked value is paired with the continuation's result.
data OutMarshaller slot hs = OutMarshaller
  { runOut :: forall r. (slot -> IO r) -> IO (hs, r) }

instance Functor (OutMarshaller slot) where
  fmap f (OutMarshaller m) = OutMarshaller (fmap (first f) . m)

-- | Convert the C return value @c@ to a Haskell value @hs@, effectfully.
newtype ResMarshaller c hs = ResMarshaller
  { runRes :: c -> IO hs }

instance Functor (ResMarshaller c) where
  fmap f (ResMarshaller k) = ResMarshaller (fmap f . k)

{-------------------------------------------------------------------------------
  Spread: dispatch a slot value onto a curried C signature
-------------------------------------------------------------------------------}

-- | Apply a slot to a curried function. For a single-arg slot this is
-- function application; for a tuple slot it's /N/ applications in a row.
-- Tuple instances ship up to arity 3.
class Spread slot lo lo' | slot lo -> lo' where
  spread :: slot -> lo -> lo'

instance {-# OVERLAPPABLE #-} Spread t (t -> lo) lo where
  spread t f = f t

instance Spread (a, b) (a -> b -> lo) lo where
  spread (a, b) f = f a b

instance Spread (a, b, c) (a -> b -> c -> lo) lo where
  spread (a, b, c) f = f a b c

{-------------------------------------------------------------------------------
  ThreadIn and ThreadOut: bracket-threading helpers
-------------------------------------------------------------------------------}

-- | Thread an input bracket past any Haskell arguments the inner refinement
-- has accumulated, firing it at the innermost 'IO'. Needed because 'input'
-- and 'output' may interleave.
class ThreadIn slot hi where
  threadIn :: (forall r. (slot -> IO r) -> IO r) -> (slot -> hi) -> hi

instance ThreadIn slot (IO r) where
  threadIn br f = br f

instance ThreadIn slot rest => ThreadIn slot (arg -> rest) where
  threadIn br f = \arg -> threadIn br (\s -> f s arg)

-- | Dual of 'ThreadIn' for outputs: also prepends @hs@ to the innermost
-- 'IO''s payload.
class ThreadOut slot hs hi hi' | slot hs hi -> hi' where
  threadOut :: (forall r. (slot -> IO r) -> IO (hs, r))
            -> (slot -> hi) -> hi'

instance ThreadOut slot hs (IO r) (IO (hs, r)) where
  threadOut br f = br f

instance ThreadOut slot hs rest rest'
       => ThreadOut slot hs (arg -> rest) (arg -> rest') where
  threadOut br f = \arg -> threadOut br (\s -> f s arg)

{-------------------------------------------------------------------------------
  The Refine GADT
-------------------------------------------------------------------------------}

-- | A refinement from a low-level callable @lo@ to a high-level wrapper @hi@.
-- Build with 'input' / 'output' / 'result' / 'discardRes' chained with @($)@.
data Refine lo hi where
  -- | Close the refinement by converting the C return value.
  Done :: ResMarshaller c hs
       -> Refine (IO c) (IO hs)

  -- | Add an input slot at the head of the C signature.
  In :: (Spread slot lo lo', ThreadIn slot rest)
     => InMarshaller slot hs
     -> Refine lo' rest
     -> Refine lo (hs -> rest)

  -- | Add an output slot at the head of the C signature.
  Out :: (Spread slot lo lo', ThreadOut slot hs rest rest')
      => OutMarshaller slot hs
      -> Refine lo' rest
      -> Refine lo  rest'

{-------------------------------------------------------------------------------
  Running a refinement
-------------------------------------------------------------------------------}

-- | Apply a refinement to a low-level callable, producing the high-level
-- wrapper.
toHighLevel :: Refine lo hi -> lo -> hi
toHighLevel (Done (ResMarshaller k))     cFn = cFn >>= k
toHighLevel (In  (InMarshaller  m) spec) cFn = \hs -> threadIn  (m hs)
                                             $ \slot -> toHighLevel spec (spread slot cFn)
toHighLevel (Out (OutMarshaller m) spec) cFn =        threadOut  m
                                             $ \slot -> toHighLevel spec (spread slot cFn)
{-# INLINABLE toHighLevel #-}

{-------------------------------------------------------------------------------
  Smart constructors
-------------------------------------------------------------------------------}

-- | Add one input slot at the head of the refinement.
input
  :: (Spread slot lo lo', ThreadIn slot rest)
  => InMarshaller slot hs
  -> Refine lo' rest
  -> Refine lo (hs -> rest)
input = In
{-# INLINE input #-}

-- | Add one output slot at the head of the refinement.
output
  :: (Spread slot lo lo', ThreadOut slot hs rest rest')
  => OutMarshaller slot hs
  -> Refine lo' rest
  -> Refine lo  rest'
output = Out
{-# INLINE output #-}

-- | Close the refinement by converting the C return value.
result :: ResMarshaller c hs -> Refine (IO c) (IO hs)
result = Done
{-# INLINE result #-}

-- | Close the refinement, discarding the C return value.
discardRes :: Refine (IO c) (IO ())
discardRes = result (pureRes (const ()))

{-------------------------------------------------------------------------------
  Nullable: slot types with a designated null value
-------------------------------------------------------------------------------}

-- | A slot type with a designated null value. Used by 'nullableIn'.
class Nullable a where
  nullValue :: a

instance Nullable (Ptr a) where
  nullValue = nullPtr

instance Nullable (PtrConst a) where
  nullValue = PtrConst.unsafeFromPtr nullPtr

{-------------------------------------------------------------------------------
  Standard input marshallers
-------------------------------------------------------------------------------}

-- | A pure conversion at the boundary. No allocation, no bracket.
pureIn :: (hs -> c) -> InMarshaller c hs
pureIn f = InMarshaller $ \x k -> k (f x)

-- | Marshal a 'String' as a NUL-terminated @const char *@ via 'withCString'.
-- C must not retain the pointer past the call.
withCStringIn :: InMarshaller (PtrConst CChar) String
withCStringIn = InMarshaller $ \s k ->
  withCString s (k . PtrConst.unsafeFromPtr)

-- | Marshal a 'ByteString' as a @(const char *, size_t)@ pair.
useAsByteStringLenIn
  :: InMarshaller (PtrConst CChar, CSize) ByteString
useAsByteStringLenIn = InMarshaller $ \bs k ->
  BS.useAsCStringLen bs $ \(p, n) ->
    k (PtrConst.unsafeFromPtr p, fromIntegral n)

-- | View an 'IncompleteArray' as a read-only @const T *@.
withConstIncompleteArrayIn
  :: Storable a => InMarshaller (PtrConst a) (IncompleteArray a)
withConstIncompleteArrayIn = InMarshaller $ \arr k ->
  IsA.withElemPtr arr $ \p -> k (PtrConst.unsafeFromPtr p)

-- | Accept @Maybe@: 'Nothing' maps to the slot's null value, 'Just'
-- delegates to the wrapped marshaller.
nullableIn
  :: Nullable slot
  => InMarshaller slot hs
  -> InMarshaller slot (Maybe hs)
nullableIn (InMarshaller m) = InMarshaller $ \mhs k -> case mhs of
  Nothing -> k nullValue
  Just hs -> m hs k

-- | Pass a Haskell function as a C function pointer, bracketed via
-- 'withFunPtr'. The 'FunPtr' is freed when the call returns, so this is
-- only safe for callbacks invoked /during/ the call.
funPtrIn :: ToFunPtr a => InMarshaller (FunPtr a) a
funPtrIn = InMarshaller withFunPtr

{-------------------------------------------------------------------------------
  Standard output marshallers
-------------------------------------------------------------------------------}

-- | Allocate one C slot, run the call, peek the value.
allocaOut :: Storable c => OutMarshaller (Ptr c) c
allocaOut = peekOut pure

-- | Allocate, run, peek, then apply an effectful @c -> 'IO' hs@.
peekOut :: Storable c => (c -> IO hs) -> OutMarshaller (Ptr c) hs
peekOut f = OutMarshaller $ \k -> alloca $ \p -> do
  r  <- k p
  c  <- peek p
  hs <- f c
  pure (hs, r)

-- | Allocate a fixed-size byte buffer, run the call, peek a NUL-terminated
-- 'String' from it.
peekCStringOut :: Int -> OutMarshaller (Ptr CChar) String
peekCStringOut cap = OutMarshaller $ \k -> allocaBytes cap $ \p -> do
  r <- k p
  s <- peekCString p
  pure (s, r)

-- | Allocate a fixed-size array buffer, run the call, peek the buffer back
-- as an 'IncompleteArray'.
peekIncompleteArrayOut
  :: Storable a => Int -> OutMarshaller (Ptr a) (IncompleteArray a)
peekIncompleteArrayOut n = OutMarshaller $ \k -> allocaArray n $ \p -> do
  r   <- k p
  arr <- IA.peekArray n (IA.toPtr p)
  pure (arr, r)

-- | Allocate a scratch buffer C writes into, then discard. The payload is
-- @()@.
scratchOut :: Storable a => Int -> OutMarshaller (Ptr a) ()
scratchOut n = OutMarshaller $ \k -> allocaArray n $ \p -> do
  r <- k p
  pure ((), r)

{-------------------------------------------------------------------------------
  Standard result marshallers
-------------------------------------------------------------------------------}

-- | A pure @c -> hs@ conversion.
pureRes :: (c -> hs) -> ResMarshaller c hs
pureRes f = ioRes (pure . f)

-- | An effectful @c -> 'IO' hs@ conversion.
ioRes :: (c -> IO hs) -> ResMarshaller c hs
ioRes = ResMarshaller

{-------------------------------------------------------------------------------
  Error-aware result marshallers
-------------------------------------------------------------------------------}

-- | Throw @e@ if the classifier returns @Just e@; otherwise delegate to the
-- inner marshaller.
throwOn
  :: Exception e
  => (c -> Maybe e)
  -> ResMarshaller c hs
  -> ResMarshaller c hs
throwOn classify (ResMarshaller k) = ResMarshaller $ \c -> case classify c of
  Just e  -> throwIO e
  Nothing -> k c

-- | Throw when the C status code is non-zero, otherwise return @()@.
throwOnNonZero
  :: (Eq c, Num c, Exception e)
  => (c -> e)
  -> ResMarshaller c ()
throwOnNonZero mk = ResMarshaller $ \c ->
  if c == 0 then pure () else throwIO (mk c)
