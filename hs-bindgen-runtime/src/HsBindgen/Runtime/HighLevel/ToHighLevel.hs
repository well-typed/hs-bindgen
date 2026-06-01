-- | Combinators for lifting low-level FFI bindings to high-level Haskell
-- wrappers. @'ToHighLevel' lo hi@ lifts a low-level function @lo@ into a wrapper
-- @hi@; 'toHighLevel' applies it.
--
-- Chain 'input' / 'output' / 'scratch' and a closer ('resultPure', 'resultIO', or
-- 'discardRes') with @($)@, then run with 'toHighLevel':
--
-- > hsStrncmp :: String -> ByteString -> IO Int
-- > hsStrncmp = toHighLevel ( input  withCStringIn
-- >                         $ input  useAsByteStringLenIn
-- >                         $ resultPure fromIntegral
-- >                         ) c_strncmp
--
-- Each 'input' marshals one Haskell argument into the C argument(s) it consumes;
-- each 'output' allocates a C argument, peeks it after the call, and adds it to
-- the result; the closer converts the C return value. Outputs and the kept result
-- come back as a flat tuple in declaration order, result last.
--
-- The combinator constructors ('mkIn', 'mkOut', 'pureIn') live here; ready-made
-- marshallers for common types live in
-- "HsBindgen.Runtime.HighLevel.ToHighLevel.Marshallers".
--
-- Building a spec hole-first works well: write the closer, then add one 'input' or
-- 'output' at a time, leaving the one you are filling as a typed hole.
--
module HsBindgen.Runtime.HighLevel.ToHighLevel (
    -- * The 'ToHighLevel' type
    ToHighLevel
    -- * Running a spec
  , toHighLevel
    -- * Smart constructors
  , input
  , output
  , scratch
  , resultPure
  , resultIO
  , discardRes
    -- * Marshaller types
  , InMarshaller (..)
  , OutMarshaller
    -- * Input marshaller constructors
  , mkIn
  , pureIn
  , optionalIn
    -- * Output marshaller constructors
  , mkOut
  , peekOut
  , peekOutPure
    -- * Scratch buffers
  , scratchArray
    -- * Error-aware combinators
  , throwOn
  , throwOnNonZero
  , throwOnOut
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (void, (>=>))
import Data.Functor.Contravariant (Contravariant (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, peek)
import GHC.TypeLits (TypeError)

import HsBindgen.Runtime.HighLevel.ToHighLevel.Errors (TooManyOutputs,
                                                       UnitOutput)

{-------------------------------------------------------------------------------
  The ToHighLevel type
-------------------------------------------------------------------------------}

-- | A spec for lifting a low-level callable @lo@ into a high-level wrapper @hi@.
-- Build it with 'input' / 'output' / 'scratch' and a closer, then run it with
-- 'toHighLevel'.
--
newtype ToHighLevel lo hi = ToHighLevel (lo -> hi)

-- | Apply a spec to a low-level callable, producing the wrapper.
toHighLevel :: ToHighLevel lo hi -> lo -> hi
toHighLevel (ToHighLevel f) = f
{-# INLINE toHighLevel #-}

{-------------------------------------------------------------------------------
  Marshaller types
-------------------------------------------------------------------------------}

-- | Marshal a Haskell value @hs@ into the C argument(s) it fills. Given @hs@ and
-- the curried callable @lo@, it applies the leading C arguments and hands the
-- remainder @lo'@ to the continuation. The arrow gap between @lo@ and @lo'@ is the
-- number of C arguments consumed:
--
-- > withCStringIn        :: InMarshaller (PtrConst CChar          -> lo') lo' String      -- 1 C arg
-- > useAsByteStringLenIn :: InMarshaller (PtrConst CChar -> CSize -> lo') lo' ByteString  -- 2 C args
--
-- Build one with 'pureIn' or 'mkIn' (single C argument), or the 'InMarshaller'
-- constructor directly (several).
--
newtype InMarshaller lo lo' hs =
    InMarshaller (forall r. hs -> lo -> (lo' -> IO r) -> IO r)

-- | 'contramap' adapts the Haskell type a marshaller accepts, e.g.
-- @contramap unpack withCStringIn@ to take a @Text@.
--
instance Contravariant (InMarshaller lo lo') where
  contramap f (InMarshaller m) = InMarshaller (\hs -> m (f hs))

-- | Allocate the C argument, run the call, peek a Haskell value back. The peeked
-- value pairs with the continuation's result and joins the result tuple. Peeking
-- an out-parameter is always 'IO', so there is no pure variant; the conversion can
-- still be pure (see 'peekOutPure'). The C return value has no marshaller: a closer
-- ('resultPure' \/ 'resultIO') converts it.
newtype OutMarshaller c hs =
    OutMarshaller (forall r. (c -> IO r) -> IO (hs, r))

-- | 'fmap' adapts the Haskell type an output yields, e.g.
-- @fmap toVector (peekIncompleteArrayOut n)@. It runs on the already-peeked value,
-- inside the readback's safe window.
instance Functor (OutMarshaller c) where
  fmap f (OutMarshaller m) = OutMarshaller $ \k -> do
    (hs, r) <- m k
    pure (f hs, r)
  {-# INLINE fmap #-}

{-------------------------------------------------------------------------------
  Output accumulation: prepend each output's value into the flat result tuple
-------------------------------------------------------------------------------}

-- | Prepend an output's value @a@ onto the result accumulated so far @t@, giving
-- the flat tuple @r@. The closer seeds @t@ (@()@ for 'discardRes', the converted
-- value for a result closer); each 'output' prepends one component, innermost
-- first, so the result reads in declaration order with the kept result last. Two
-- outputs over a result give a flat @IO (a, b, c)@, not @IO (a, (b, c))@.
--
-- The three fundeps run the relation in every direction, so the wrapper's declared
-- return type drives inference and call sites need no annotation. The tuple tops
-- out at six components ('TooManyOutputs' beyond that).
--
-- Two shapes need a hand-rolled 'OutMarshaller', since no shipped marshaller
-- produces them:
--
--   * a @()@-valued output collides with the empty accumulator ('UnitOutput'
--     error); use 'scratch' for a write-only out-parameter;
--   * a tuple-valued output flattens into the result on the inference path
--     (@output intOut $ output tupOut@ infers @(Int,Int,Int)@); write the nested
--     signature @IO (Int, (Int, Int))@ to keep it separate.
class Prepend a t r | a t -> r, a r -> t, r t -> a where
  prepend :: a -> t -> r

instance {-# OVERLAPPING #-} Prepend a (x, y, z, w, v) (a, x, y, z, w, v) where
  prepend a (x, y, z, w, v) = (a, x, y, z, w, v)
  {-# INLINE prepend #-}
instance {-# OVERLAPPING #-} Prepend a (x, y, z, w) (a, x, y, z, w) where
  prepend a (x, y, z, w) = (a, x, y, z, w)
  {-# INLINE prepend #-}
instance {-# OVERLAPPING #-} Prepend a (x, y, z) (a, x, y, z) where
  prepend a (x, y, z) = (a, x, y, z)
  {-# INLINE prepend #-}
instance {-# OVERLAPPING #-} Prepend a (x, y) (a, x, y) where
  prepend a (x, y) = (a, x, y)
  {-# INLINE prepend #-}
instance {-# OVERLAPPING #-} Prepend a () a where
  prepend a () = a
  {-# INLINE prepend #-}
instance {-# OVERLAPPABLE #-} (r ~ (a, x)) => Prepend a x r where
  prepend a x = (a, x)
  {-# INLINE prepend #-}

-- | A seventh kept output. Fire a clear error rather than nest the tuple silently.
instance {-# OVERLAPPING #-} TypeError TooManyOutputs
      => Prepend a (x, y, z, w, v, u) (a, x, y, z, w, v, u) where
  prepend = errorWithoutStackTrace "ToHighLevel.Prepend: too many outputs"
-- | A @()@-valued output prepended onto the empty accumulator would be swallowed,
-- collapsing the result arity. Guard only this ground case, so the ordinary "no
-- kept result" prepend (@'prepend' x ()@ for a non-@()@ @x@) still resolves below.
instance {-# OVERLAPPING #-} TypeError UnitOutput => Prepend () () () where
  prepend = errorWithoutStackTrace "ToHighLevel.Prepend: ()-valued output"

{-------------------------------------------------------------------------------
  ThreadIn and ThreadOut: bracket-threading helpers

  An input or scratch marshaller may open a bracket (e.g. 'withCString'). It has
  to stay open while later Haskell arguments are applied and the C call runs, so it
  is threaded to the innermost 'IO'. 'ThreadOut' is the dual for outputs: it
  threads the output's 'alloca' the same way and prepends the peeked value.
-------------------------------------------------------------------------------}

-- | Thread a bracket producing the call remainder @lo'@ past the Haskell arguments
-- the inner spec still accepts, firing it at the innermost 'IO'.
--
class ThreadIn lo hi where
  threadIn :: (forall r. (lo -> IO r) -> IO r) -> (lo -> hi) -> hi

instance ThreadIn lo (IO r) where
  threadIn br f = br f
  {-# INLINE threadIn #-}

instance ThreadIn lo rest => ThreadIn lo (arg -> rest) where
  threadIn br f = \arg -> threadIn br (\l -> f l arg)
  {-# INLINE threadIn #-}

-- | Dual of 'ThreadIn' for outputs: thread the output's 'alloca' past later
-- Haskell arguments and 'prepend' its peeked value into the result.
class ThreadOut c hs hi hi' | c hs hi -> hi' where
  threadOut :: (forall r. (c -> IO r) -> IO (hs, r))
            -> (c -> hi) -> hi'

instance Prepend hs t r
      => ThreadOut c hs (IO t) (IO r) where
  threadOut br f = do
    (hs, t) <- br f
    pure (prepend hs t)
  {-# INLINE threadOut #-}

instance ThreadOut c hs rest rest'
       => ThreadOut c hs (arg -> rest) (arg -> rest') where
  threadOut br f = \arg -> threadOut br (\s -> f s arg)
  {-# INLINE threadOut #-}

{-------------------------------------------------------------------------------
  Smart constructors
-------------------------------------------------------------------------------}

-- | Add one input at the head of the spec. The wrapper gains one argument @hs@;
-- the marshaller consumes the C argument(s) it needs off @lo@, leaving @lo'@.
--
input
  :: ThreadIn lo' hi
  => InMarshaller lo lo' hs
  -> ToHighLevel lo' hi
  -> ToHighLevel lo (hs -> hi)
input (InMarshaller run) (ToHighLevel rest) =
  ToHighLevel $ \lo hs -> threadIn (run hs lo) rest
{-# INLINE input #-}

-- | Add one output at the head of the spec. The wrapper gains no argument; the
-- output's value joins the result tuple.
--
output
  :: ThreadOut c hs hi hi'
  => OutMarshaller c hs
  -> ToHighLevel lo' hi
  -> ToHighLevel (c -> lo') hi'
output (OutMarshaller m) (ToHighLevel rest) =
  ToHighLevel $ \lo -> threadOut m $ \c -> rest (lo c)
{-# INLINE output #-}

-- | Add a scratch position: a bracket supplying one C argument the callee writes
-- into and the caller never sees. It contributes nothing to the wrapper type (no
-- argument, no result component). Most callers want 'scratchArray'.
--
scratch
  :: ThreadIn lo' hi
  => (forall r. (c -> IO r) -> IO r)
  -> ToHighLevel lo' hi
  -> ToHighLevel (c -> lo') hi
scratch br (ToHighLevel rest) =
  ToHighLevel $ \lo -> threadIn (\k -> br (\c -> k (lo c))) rest
{-# INLINE scratch #-}

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

-- | Close the spec, discarding the C return value. It contributes no component,
-- so it leaves no trailing @()@ in the result tuple.
--
discardRes :: ToHighLevel (IO c) (IO ())
discardRes = ToHighLevel $ \cFn -> void cFn
{-# INLINE discardRes #-}

{-------------------------------------------------------------------------------
  Standard input marshallers
-------------------------------------------------------------------------------}

-- | Build an input marshaller from a bracket that turns the Haskell value into one
-- C argument, kept live across the call. 'pureIn' and the single-argument
-- marshallers in "HsBindgen.Runtime.HighLevel.ToHighLevel.Marshallers" are built
-- this way. For several C arguments from one value, use the 'InMarshaller'
-- constructor.
--
mkIn :: (forall r. hs -> (c -> IO r) -> IO r) -> InMarshaller (c -> lo') lo' hs
mkIn bracket = InMarshaller $ \hs lo k -> bracket hs (\c -> k (lo c))
{-# INLINE mkIn #-}

-- | A pure conversion at the boundary, consuming one C argument: no allocation, no
-- bracket. Like 'mkIn', this is the single-argument case; a pure value filling
-- several C arguments uses the 'InMarshaller' constructor (e.g. a @Complex@ filling
-- a @(re, im)@ pair).
--
pureIn :: (hs -> c) -> InMarshaller (c -> lo') lo' hs
pureIn f = mkIn (\hs k -> k (f hs))
{-# INLINE pureIn #-}

-- | Accept @Maybe@ at the boundary: 'Nothing' fills the C argument(s) with a
-- caller-supplied default, 'Just' delegates to the wrapped marshaller.
--
-- The default is a gap-filler @lo -> lo'@ that applies the default C values to the
-- callable, so one combinator covers a one- or many-argument default alike (its
-- arity is fixed by the marshaller it wraps):
--
-- > optionalIn ($ nullCharPtr)       withCStringIn         -- 1 C arg : Maybe String,     Nothing -> NULL
-- > optionalIn (\lo -> lo nullPtr 0) useAsByteStringLenIn  -- 2 C args: Maybe ByteString, Nothing -> (NULL, 0)
--
-- The filler opens no bracket; any bracket comes from the wrapped marshaller in the
-- 'Just' case.
--
optionalIn
  :: (lo -> lo')                     -- ^ fill the C argument(s) for 'Nothing'
  -> InMarshaller lo lo' hs          -- ^ marshaller used for 'Just'
  -> InMarshaller lo lo' (Maybe hs)
optionalIn fill (InMarshaller m) = InMarshaller $ \mhs lo k -> case mhs of
  Nothing -> k (fill lo)
  Just hs -> m hs lo k
{-# INLINE optionalIn #-}

{-------------------------------------------------------------------------------
  Standard output marshallers
-------------------------------------------------------------------------------}

-- | Build an output marshaller from an allocator and a reader: allocate the C
-- argument, run the call first so the callee populates the buffer, then read the
-- value back and pair it with the call's result. Use it for a custom readback
-- (e.g. an out-pointer that walks a linked list).
--
-- @readBack@ must fully evaluate before it returns: the pointer is freed once it
-- completes, so a deferred peek or a retained pointer reads freed memory.
-- @allocate@ must be a bracket (like 'Foreign.Marshal.Alloc.alloca'); 'mkOut' adds
-- no cleanup, so a bare @malloc@ \/ @free@ would leak the buffer if the call throws.
--
mkOut
  :: (forall r. (Ptr c -> IO r) -> IO r) -- ^ allocate the out-pointer
  -> (Ptr c -> IO hs)                    -- ^ read the value back, /after/ the call
  -> OutMarshaller (Ptr c) hs
mkOut allocate readBack = OutMarshaller $ \k -> allocate $ \p -> do
  r  <- k p
  hs <- readBack p
  pure (hs, r)
{-# INLINE mkOut #-}

-- | Allocate, run the call, peek, then apply an effectful @c -> 'IO' hs@. See
-- 'peekOutPure' for a pure conversion (pass 'id' for the raw value).
--
peekOut :: Storable c => (c -> IO hs) -> OutMarshaller (Ptr c) hs
peekOut f = mkOut alloca (peek >=> f)
{-# INLINE peekOut #-}

-- | Allocate, run, peek, then convert purely. The common case where an
-- out-parameter needs only a pure @c -> hs@ (e.g. @'CInt' -> 'Int'@); the peek is
-- the only 'IO'.
--
peekOutPure :: Storable c => (c -> hs) -> OutMarshaller (Ptr c) hs
peekOutPure f = mkOut alloca (fmap f . peek)
{-# INLINE peekOutPure #-}

{-------------------------------------------------------------------------------
  Scratch buffers
-------------------------------------------------------------------------------}

-- | Allocate an @n@-element array of @a@ as a scratch C argument the callee writes
-- into and the caller never sees. @'scratchArray' n = 'scratch' ('allocaArray' n)@.
--
scratchArray
  :: (Storable a, ThreadIn lo' hi)
  => Int -> ToHighLevel lo' hi -> ToHighLevel (Ptr a -> lo') hi
scratchArray n = scratch (allocaArray n)
{-# INLINE scratchArray #-}

{-------------------------------------------------------------------------------
  Error-aware combinators

  Every position runs in 'IO', so a check can sit wherever the error signal lives:
  the return value (the closers below), an output ('throwOnOut'), or an input
  (throw inside the marshaller). Bracket threading unwinds every open bracket on a
  throw, so this is exception-safe by construction.
-------------------------------------------------------------------------------}

-- | Wrap a result closer with an error check: run it, then classify the converted
-- value. 'Left' throws, 'Right' yields the refined value. The check sees the
-- converted value (@throwOn check ('resultPure' 'fromIntegral')@ classifies the
-- 'Int', not the raw 'CInt') and may change the result type.
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
-- @throwOnOut classify = 'peekOut' ('either' 'throwIO' 'pure' . classify)@.
--
throwOnOut
  :: (Storable c, Exception e)
  => (c -> Either e hs)
  -> OutMarshaller (Ptr c) hs
throwOnOut classify = peekOut (either throwIO pure . classify)
{-# INLINE throwOnOut #-}
