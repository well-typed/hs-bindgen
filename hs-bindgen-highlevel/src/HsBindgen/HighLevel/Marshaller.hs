-- | The marshalling combinators: composable pieces for moving one value across the
-- C boundary, independent of how the value is used.
--
-- Everything is built on one type, 'Marshal'. 'Unmarshaller' is its out-parameter
-- form (allocate, run, peek) and 'UnmarshalStruct' the read-back form (build a high-level
-- value from an already-peeked struct). Ready-made marshallers live in
-- "HsBindgen.HighLevel.Marshaller.Utils"; to lift a low-level /function/,
-- see "HsBindgen.HighLevel".
--
module HsBindgen.HighLevel.Marshaller (
    -- * The marshaller type
    Marshal (..)
  , at
  , scalar
  , bracket
  , marshalOptional
  , asConstArg
  , asMutableArg
  , (>>>)
    -- * Marshalling a struct (write)
  , MarshalStruct
  , struct
  , marshalNested
  , withStruct
    -- * Out-parameter marshallers
  , Unmarshaller (..)
  , unmarshalOutWith
  , unmarshalOut
  , unmarshalOutPure
    -- * Unmarshalling a struct (read)
  , UnmarshalStruct (..)
  , unmarshalField
  , unmarshalFieldPure
  , unmarshalOptional
  , unmarshalNested
  ) where

import Prelude hiding (id, (.))

import Control.Category (Category (..), (>>>))
import Control.Monad ((>=>))
import Data.Kind (Type)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable, peek)
import GHC.Exts (RuntimeRep, TYPE)

import HsBindgen.Runtime.Marshal (StaticSize, WriteRaw, withZero)
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

{-------------------------------------------------------------------------------
  The marshaller type
-------------------------------------------------------------------------------}

-- | A @Marshal e a b@ turns one Haskell value @e@ into the leading C argument(s) of
-- a curried callable. Read @a@ and @b@ as that callable before and after this
-- marshaller has supplied its arguments: it takes @a@, fills the argument(s) it is
-- responsible for, and hands the shorter callable @b@ to a continuation (so a
-- resource such as a 'Foreign.C.String.withCString' buffer stays live across the
-- call). The number of arrows between @a@ and @b@ is how many C arguments it fills:
--
-- > withCStringIn        :: Marshal String     (PtrConst CChar          -> lo') lo'  -- 1 C arg
-- > useAsByteStringLenIn :: Marshal ByteString (PtrConst CChar -> CSize -> lo') lo'  -- 2 C args
--
-- Build a single marshaller with 'scalar' or 'bracket' (one C argument) or the
-- 'Marshal' constructor directly (several); for a struct, see 'struct'.
--
newtype Marshal e a b =
    Marshal (forall r. e -> a -> (b -> IO r) -> IO r)

-- | Chain a struct's fields in source order with '>>>':
-- @at f1 m1 >>> at f2 m2@ marshals field 1 then field 2. This is
-- 'Control.Category.Category' composition: @id@ marshals nothing, and @g . f@ runs
-- @f@ then @g@. The function side uses 'HsBindgen.HighLevel.input' (or
-- @input1@ \/ @input2@ \/ @input3@ for a fixed C-argument count) instead.
instance Category (Marshal e) where
  id = Marshal (\_ a k -> k a)
  Marshal g . Marshal f = Marshal (\e a k -> f e a (\b -> g e b k))
  {-# INLINE id #-}
  {-# INLINE (.) #-}

-- | Aim a marshaller at one field: @at oName useAsByteStringLenIn@ feeds the
-- @oName@ field to the marshaller. With any other function it adapts its
-- input type, e.g. @at Data.Text.unpack withCStringIn@ takes a @Text@.
--
-- @at@ checks the field's C type and its order, not which selector was projected.
-- Two fields of the same Haskell type can be swapped without a type error, so the
-- selector is the caller's responsibility.
at :: (e' -> e) -> Marshal e a b -> Marshal e' a b
at g (Marshal m) = Marshal (\e' -> m (g e'))
{-# INLINE at #-}

-- | @bracket br@: a marshaller that holds a resource open while its one C argument
-- is live, tearing down when the continuation returns. For several C arguments from
-- one value, use the 'Marshal' constructor.
--
-- For an /unlifted/ by-value C argument (an @R@ struct payload) see @bracketUnlifted@
-- in "HsBindgen.HighLevel.Unlifted".
--
bracket :: (forall r. e -> (c -> IO r) -> IO r) -> Marshal e (c -> lo') lo'
bracket br = Marshal $ \e lo k -> br e (\c -> k (lo c))
{-# INLINE bracket #-}

-- | A marshaller with a pure conversion and no resource (@scalar f = bracket (\\e k
-- -> k (f e))@), the common case. A pure value filling several C arguments uses the
-- 'Marshal' constructor (e.g. a @Complex@ filling a @(re, im)@ pair).
--
scalar :: (e -> c) -> Marshal e (c -> lo') lo'
scalar f = bracket (\e k -> k (f e))
{-# INLINE scalar #-}

-- | Accept @Maybe@ at the boundary: 'Nothing' fills the C argument(s) with a
-- caller-supplied default, 'Just' delegates to the wrapped marshaller. The default
-- is a gap-filler @lo -> lo'@ whose arity matches the wrapped marshaller, so the
-- same combinator handles one- or many-argument defaults:
--
-- > marshalOptional ($ nullCharPtr)           withCStringIn         -- 1 C arg : Maybe String,     Nothing -> NULL
-- > marshalOptional (\lo -> lo nullCharPtr 0) useAsByteStringLenIn  -- 2 C args: Maybe ByteString, Nothing -> (NULL, 0)
--
marshalOptional
  :: (lo -> lo')                     -- ^ fill the C argument(s) for 'Nothing'
  -> Marshal hs lo lo'               -- ^ marshaller used for 'Just'
  -> Marshal (Maybe hs) lo lo'
marshalOptional fill (Marshal m) = Marshal $ \mhs lo k -> case mhs of
  Nothing -> k (fill lo)
  Just hs -> m hs lo k
{-# INLINE marshalOptional #-}

-- | Retag a marshaller that fills a mutable @'Ptr' a@ so it fills a read-only
-- @'PtrConst' a@ instead, letting one marshaller serve both a @T *@ and a
-- @const T *@ argument.
--
asConstArg :: Marshal e (Ptr a -> lo) lo -> Marshal e (PtrConst a -> lo) lo
asConstArg (Marshal m) = Marshal $ \e loC k -> m e (loC . PtrConst.unsafeFromPtr) k
{-# INLINE asConstArg #-}

-- | Retag a marshaller that fills a read-only @'PtrConst' a@ so it fills a mutable
-- @'Ptr' a@ instead, the inverse of 'asConstArg'. Use it only for a @T *@ argument
-- the callee does not actually write.
--
asMutableArg :: Marshal e (PtrConst a -> lo) lo -> Marshal e (Ptr a -> lo) lo
asMutableArg (Marshal m) = Marshal $ \e lo k -> m e (lo . PtrConst.unsafeToPtr) k
{-# INLINE asMutableArg #-}

{-------------------------------------------------------------------------------
  Marshalling a struct (write)
-------------------------------------------------------------------------------}

-- | A struct marshaller with its constructor captured: it marshals a high-level
-- value @hi@ into a low-level struct @s@. Build one with 'struct', then 'marshalNested' it
-- inside another or run it with 'withStruct' \/
-- 'HsBindgen.HighLevel.asArgument'.
--
newtype MarshalStruct hi s = MarshalStruct (forall r. hi -> (s -> IO r) -> IO r)

-- | Build a 'MarshalStruct' from a field chain, supplying the constructor once:
--
-- > point :: MarshalStruct PointHi Point
-- > point = struct Point (at px (scalar fromIntegral) >>> at py (scalar fromIntegral))
--
struct :: ctor -> Marshal hi ctor s -> MarshalStruct hi s
struct ctor (Marshal m) = MarshalStruct (\hi k -> m hi ctor k)
{-# INLINE struct #-}

-- | Marshal a sub-struct as one field of an enclosing struct, inline with no
-- intermediate buffer.
--
marshalNested :: MarshalStruct sub s -> Marshal sub (s -> lo') lo'
marshalNested (MarshalStruct inner) = Marshal $ \sub lo k -> inner sub (\built -> k (lo built))
{-# INLINE marshalNested #-}

-- | Marshal the value into a fresh aligned slot and hand the pointer to the
-- continuation. The slot is zeroed first, so padding bytes reach C as zeros. Field
-- brackets stay open across the continuation, so a @const char *@ field still
-- points at live memory while the pointer is in use.
--
withStruct
  :: (StaticSize s, WriteRaw s)
  => MarshalStruct hi s
  -> hi
  -> (Ptr s -> IO r)
  -> IO r
withStruct (MarshalStruct m) hi k = m hi (\built -> withZero built k)
{-# INLINE withStruct #-}

{-------------------------------------------------------------------------------
  Out-parameter marshallers
-------------------------------------------------------------------------------}

-- | An out-parameter marshaller: allocate the C argument @c@, run the call, peek a
-- Haskell value @hs@ back. Read the type as "wrap a call that needs a @c@ and, on
-- top of whatever the call returns, hand back an @hs@": the continuation is the
-- call (it receives the @c@), and its result is paired with the peeked @hs@. Peeking
-- is always 'IO'; a pure conversion still goes through 'unmarshalOutPure'.
--
-- @c@ is representation-polymorphic, so an 'Unmarshaller' can front a lifted
-- @'Ptr' c@ output or an unlifted by-value @W@ struct output. See
-- 'HsBindgen.HighLevel.output' \/ 'HsBindgen.HighLevel.outputUnlifted'.
type Unmarshaller ::
     forall (rep :: RuntimeRep). TYPE rep -> Type -> Type
newtype Unmarshaller c hs =
    Unmarshaller (forall r. (c -> IO r) -> IO (hs, r))

-- | 'fmap' adapts the Haskell type an output yields, e.g.
-- @fmap toVector (peekIncompleteArrayOut n)@. It runs on the already-peeked value,
-- inside the readback's safe window.
instance Functor (Unmarshaller c) where
  fmap f (Unmarshaller m) = Unmarshaller $ \k -> do
    (hs, r) <- m k
    pure (f hs, r)
  {-# INLINE fmap #-}

-- | Build an output marshaller from an allocator and a reader: allocate, run the
-- call so the callee fills the buffer, then read the value back and pair it with
-- the result. For a custom readback (e.g. walking a linked list).
--
-- @readBack@ must finish before it returns: the pointer is freed once 'unmarshalOutWith'
-- completes, so a deferred peek reads freed memory. @allocate@ must be a bracket
-- (like 'Foreign.Marshal.Alloc.alloca'); 'unmarshalOutWith' adds no cleanup, so a bare
-- @malloc@ would leak on a thrown call.
--
unmarshalOutWith
  :: (forall r. (Ptr c -> IO r) -> IO r) -- ^ allocate the out-pointer
  -> (Ptr c -> IO hs)                    -- ^ read the value back, /after/ the call
  -> Unmarshaller (Ptr c) hs
unmarshalOutWith allocate readBack = Unmarshaller $ \k -> allocate $ \p -> do
  r  <- k p
  hs <- readBack p
  pure (hs, r)
{-# INLINE unmarshalOutWith #-}

-- | Allocate, run the call, peek, then apply an effectful @c -> 'IO' hs@. See
-- 'unmarshalOutPure' for a pure conversion (pass 'Prelude.id' for the raw value).
--
unmarshalOut :: Storable c => (c -> IO hs) -> Unmarshaller (Ptr c) hs
unmarshalOut f = unmarshalOutWith alloca (peek >=> f)
{-# INLINE unmarshalOut #-}

-- | Allocate, run, peek, then convert purely (the peek is the only 'IO'), e.g.
-- @'Foreign.C.Types.CInt' -> 'Int'@. The common scalar case.
--
unmarshalOutPure :: Storable c => (c -> hs) -> Unmarshaller (Ptr c) hs
unmarshalOutPure f = unmarshalOutWith alloca (fmap f . peek)
{-# INLINE unmarshalOutPure #-}

{-------------------------------------------------------------------------------
  Unmarshalling a struct (read)
-------------------------------------------------------------------------------}

-- | A reader from an already-peeked low-level struct @lo@ to a high-level value
-- @hi@ (the read-back counterpart of 'Marshal'). Read it as a plain @lo -> 'IO' hi@:
-- the 'IO' is there because rebuilding a field can be effectful (copying a
-- @const char *@ into a 'Data.ByteString.ByteString', say), and 'fmap' \/ '<*>'
-- assemble field readers under the @hi@ constructor.
--
newtype UnmarshalStruct lo hi = UnmarshalStruct {
    -- | Run a struct reader on an already-peeked low-level struct. Most bindings
    -- reach it through @asOutput@ or @asResult@; call it directly to read a struct
    -- you peeked yourself.
    runUnmarshalStruct :: lo -> IO hi
  }

-- | Adapt the high-level value a reader yields, leaving the C fields it reads
-- untouched.
instance Functor (UnmarshalStruct lo) where
  fmap f (UnmarshalStruct g) = UnmarshalStruct (fmap f . g)
  {-# INLINE fmap #-}

-- | Assemble field readers under the high-level constructor with @<$>@ \/ @<*>@;
-- every reader sees the same low-level struct. 'pure' ignores it.
instance Applicative (UnmarshalStruct lo) where
  pure x = UnmarshalStruct (\_ -> pure x)
  UnmarshalStruct f <*> UnmarshalStruct x = UnmarshalStruct (\lo -> f lo <*> x lo)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

-- | Read one field: project its C field(s) out and convert them (effectfully) to
-- the Haskell value. The projection may pull several C fields for one value (e.g.
-- @(ptr, len)@ for a 'Data.ByteString.ByteString'). It is the read-side 'at',
-- and carries the same caveat: the projection is unchecked beyond its type, so a
-- wrong but same-typed field still compiles. Matching field to projection is the
-- caller's responsibility.
--
unmarshalField :: (lo -> c) -> (c -> IO hsf) -> UnmarshalStruct lo hsf
unmarshalField prj conv = UnmarshalStruct (\lo -> conv (prj lo))
{-# INLINE unmarshalField #-}

-- | Read one field with a pure conversion (no 'IO'), e.g. @'Foreign.C.Types.CInt'
-- -> 'Int'@. The common scalar case.
--
unmarshalFieldPure :: (lo -> c) -> (c -> hsf) -> UnmarshalStruct lo hsf
unmarshalFieldPure prj conv = UnmarshalStruct (\lo -> pure (conv (prj lo)))
{-# INLINE unmarshalFieldPure #-}

-- | Read a nullable pointer field: 'Nothing' when NULL, else the converted value.
-- The read-side counterpart of 'marshalOptional'. For a @const@ field, project through
-- 'HsBindgen.Runtime.PtrConst.unsafeToPtr'.
--
unmarshalOptional :: (lo -> Ptr a) -> (Ptr a -> IO hsf) -> UnmarshalStruct lo (Maybe hsf)
unmarshalOptional prj conv = UnmarshalStruct $ \lo ->
  case prj lo of
    p | p == nullPtr -> pure Nothing
      | otherwise    -> Just <$> conv p
{-# INLINE unmarshalOptional #-}

-- | Read a nested (inline) sub-struct as one field of the enclosing struct, the
-- read-side counterpart of 'marshalNested'.
--
unmarshalNested :: (lo -> sub) -> UnmarshalStruct sub hi -> UnmarshalStruct lo hi
unmarshalNested prj inner = UnmarshalStruct (\lo -> runUnmarshalStruct inner (prj lo))
{-# INLINE unmarshalNested #-}
