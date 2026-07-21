-- | Moving one value across the C boundary, independently of where that value sits.
--
-- The write side is built on one type, t'Marshal', which says how a Haskell value
-- fills the C argument(s) it is responsible for. A marshaller is independent of where
-- the value sits, so the same one serves both a function argument and a struct field:
-- 'HsBindgen.HighLevel.input' drops it into the former, 'at' aims it at the latter.
-- Write the conversion once and use it in both.
--
-- > withCStringIn :: Marshal String (PtrConst CChar -> lo') lo'
-- >
-- > input withCStringIn                   -- filling a function argument
-- > struct Outer (at oName withCStringIn)  -- filling a struct field
--
-- There are two read-side types as well. t'Unmarshaller' is the out-parameter form:
-- allocate a slot, let C fill it during the call, read it back afterwards.
-- t'UnmarshalStruct' builds a high-level value out of an already-peeked struct.
--
-- Ready-made marshallers for the common Haskell and C pairings are in
-- "HsBindgen.HighLevel.Marshaller.Utils"; to lift a whole low-level /function/, see
-- "HsBindgen.HighLevel".
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

-- | A @Marshal hs lo lo'@ turns one Haskell value @hs@ into the leading C
-- argument(s) of a curried callable. Read @lo@ and @lo'@ as that callable before and
-- after this marshaller has supplied its arguments: it takes @lo@, fills the
-- argument(s) it is responsible for, and hands the shorter callable @lo'@ to a
-- continuation, so a resource such as a 'Foreign.C.String.withCString' buffer stays
-- live across the call. The number of arrows between @lo@ and @lo'@ is how many C
-- arguments it fills:
--
-- > withCStringIn        :: Marshal String     (PtrConst CChar          -> lo') lo'  -- 1 C arg
-- > useAsByteStringLenIn :: Marshal ByteString (PtrConst CChar -> CSize -> lo') lo'  -- 2 C args
--
-- Build a single marshaller with 'scalar' or 'bracket' (one C argument) or the
-- t'Marshal' constructor directly (several); for a struct, see 'struct'.
--
-- On the struct side @lo@ and @lo'@ are the struct's own constructor, before and
-- after this field has been applied to it. Either way it is a curried thing being
-- filled one field at a time.
--
newtype Marshal hs lo lo' =
    Marshal (forall r. hs -> lo -> (lo' -> IO r) -> IO r)

-- | Chain a struct's fields in source order with '>>>':
-- @at f1 m1 >>> at f2 m2@ marshals field 1 then field 2. This is
-- 'Control.Category.Category' composition: @id@ marshals nothing, and @g . f@ runs
-- @f@ then @g@. The function side uses 'HsBindgen.HighLevel.input' (or
-- @input2@ \/ @input3@ \/ @inputN@ for other C-argument counts) instead.
instance Category (Marshal hs) where
  id = Marshal (\_ lo k -> k lo)
  Marshal g . Marshal f = Marshal (\hs lo k -> f hs lo (\lo' -> g hs lo' k))
  {-# INLINE id #-}
  {-# INLINE (.) #-}

-- | Aim a marshaller at one field: @at oName useAsByteStringLenIn@ feeds the
-- @oName@ field to the marshaller. With any other function it adapts its
-- input type, e.g. @at Data.Text.unpack withCStringIn@ takes a @Text@.
--
-- @at@ checks the field's C type and its order, not which selector was projected.
-- Two fields of the same Haskell type can be swapped without a type error, so the
-- selector is the caller's responsibility.
at :: (hs' -> hs) -> Marshal hs lo lo' -> Marshal hs' lo lo'
at g (Marshal m) = Marshal (\hs' -> m (g hs'))
{-# INLINE at #-}

-- | @bracket br@: a marshaller that holds a resource open while its one C argument
-- is live, tearing down when the continuation returns. For several C arguments from
-- one value, use the t'Marshal' constructor.
--
-- For an /unlifted/ by-value C argument (an @R@ struct payload) see @bracketUnlifted@
-- in "HsBindgen.HighLevel.Unlifted".
--
bracket :: (forall r. hs -> (c -> IO r) -> IO r) -> Marshal hs (c -> lo') lo'
bracket br = Marshal $ \hs lo k -> br hs (\c -> k (lo c))
{-# INLINE bracket #-}

-- | A marshaller with a pure conversion and no resource (@scalar f = bracket
-- (\\hs k -> k (f hs))@), the common case. A pure value filling several C arguments
-- uses the t'Marshal' constructor (e.g. a @Complex@ filling a @(re, im)@ pair).
--
scalar :: (hs -> c) -> Marshal hs (c -> lo') lo'
scalar f = bracket (\hs k -> k (f hs))
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
asConstArg :: Marshal hs (Ptr a -> lo') lo' -> Marshal hs (PtrConst a -> lo') lo'
asConstArg (Marshal m) = Marshal $ \hs loC k -> m hs (loC . PtrConst.unsafeFromPtr) k
{-# INLINE asConstArg #-}

-- | Retag a marshaller that fills a read-only @'PtrConst' a@ so it fills a mutable
-- @'Ptr' a@ instead, the inverse of 'asConstArg'. Use it only for a @T *@ argument
-- C does not actually write.
--
asMutableArg :: Marshal hs (PtrConst a -> lo') lo' -> Marshal hs (Ptr a -> lo') lo'
asMutableArg (Marshal m) = Marshal $ \hs lo k -> m hs (lo . PtrConst.unsafeToPtr) k
{-# INLINE asMutableArg #-}

{-------------------------------------------------------------------------------
  Marshalling a struct (write)
-------------------------------------------------------------------------------}

-- | A struct marshaller with its constructor captured: it marshals a high-level
-- value @hi@ into a low-level struct @s@. Build one with 'struct', then 'marshalNested' it
-- inside another or run it with 'withStruct' \/
-- 'HsBindgen.HighLevel.asArgument'.
--
newtype MarshalStruct hi struct =
    MarshalStruct (forall r. hi -> (struct -> IO r) -> IO r)

-- | Build a t'MarshalStruct' from a field chain, supplying the constructor once:
--
-- > point :: MarshalStruct PointHi Point
-- > point = struct Point (at px (scalar fromIntegral) >>> at py (scalar fromIntegral))
--
struct :: ctor -> Marshal hi ctor struct -> MarshalStruct hi struct
struct ctor (Marshal m) = MarshalStruct (\hi k -> m hi ctor k)
{-# INLINE struct #-}

-- | Marshal a sub-struct as one field of an enclosing struct, inline with no
-- intermediate buffer.
--
marshalNested :: MarshalStruct sub struct -> Marshal sub (struct -> lo') lo'
marshalNested (MarshalStruct inner) =
    Marshal $ \sub lo k -> inner sub (\built -> k (lo built))
{-# INLINE marshalNested #-}

-- | Marshal the value into a fresh aligned slot and hand the pointer to the
-- continuation. The slot is zeroed first, so padding bytes reach C as zeros. Field
-- brackets stay open across the continuation, so a @const char *@ field still
-- points at live memory while the pointer is in use.
--
withStruct
  :: (StaticSize struct, WriteRaw struct)
  => MarshalStruct hi struct
  -> hi
  -> (Ptr struct -> IO r)
  -> IO r
withStruct (MarshalStruct m) hi k = m hi (\built -> withZero built k)
{-# INLINE withStruct #-}

{-------------------------------------------------------------------------------
  Out-parameter marshallers
-------------------------------------------------------------------------------}

-- | An out-parameter marshaller, in two halves: a bracket that /allocates/ the C
-- argument @c@, and a reader that turns the filled slot into a Haskell value @hs@.
-- 'HsBindgen.HighLevel.output' opens the bracket on the way in and runs the reader
-- once the C call has returned. Reading is always 'IO'; a pure conversion still
-- goes through 'unmarshalOutPure'.
--
-- The reader must finish before it returns: the slot is freed when the bracket
-- closes, so a deferred peek reads freed memory. The allocator must be a bracket
-- (like 'Foreign.Marshal.Alloc.alloca'); nothing here adds cleanup, so a bare
-- @malloc@ would leak on a thrown call.
--
-- @c@ is representation-polymorphic, so an t'Unmarshaller' can front a lifted
-- @'Ptr' c@ output or an unlifted by-value @W@ struct output. See
-- 'HsBindgen.HighLevel.output' \/ 'HsBindgen.HighLevel.Unlifted.outputUnlifted'.
type Unmarshaller ::
     forall (rep :: RuntimeRep). TYPE rep -> Type -> Type
data Unmarshaller c hs =
    Unmarshaller
      (forall r. (c -> IO r) -> IO r) -- ^ allocate the slot
      (c -> IO hs)                    -- ^ read it back, /after/ the call

-- | 'fmap' adapts the Haskell type an output yields, e.g.
-- @fmap toVector (peekIncompleteArrayOut n)@. It runs on the already-read value,
-- inside the read-back's safe window.
instance Functor (Unmarshaller c) where
  fmap f (Unmarshaller allocate readBack) =
    Unmarshaller allocate (fmap f . readBack)
  {-# INLINE fmap #-}

-- | Build an output marshaller from an allocator and a reader, for a custom read-back
-- (e.g. walking a linked list). See t'Unmarshaller' for the contract both halves must
-- honour.
--
unmarshalOutWith
  :: (forall r. (Ptr c -> IO r) -> IO r) -- ^ allocate the out-pointer
  -> (Ptr c -> IO hs)                    -- ^ read the value back, /after/ the call
  -> Unmarshaller (Ptr c) hs
unmarshalOutWith = Unmarshaller
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

-- | A reader from an already-peeked low-level struct to a high-level value @hi@
-- (the read-back counterpart of t'Marshal'). Read it as a plain @struct -> 'IO' hi@:
-- the 'IO' is there because rebuilding a field can be effectful (copying a
-- @const char *@ into a 'Data.ByteString.ByteString', say), and 'fmap' \/ '<*>'
-- assemble field readers under the @hi@ constructor.
--
newtype UnmarshalStruct struct hi = UnmarshalStruct {
    -- | Run a struct reader on an already-peeked low-level struct. Most bindings
    -- reach it through @asOutput@ or @asResult@; call it directly to read a struct
    -- you peeked yourself.
    runUnmarshalStruct :: struct -> IO hi
  }

-- | Adapt the high-level value a reader yields, leaving the C fields it reads
-- untouched.
instance Functor (UnmarshalStruct struct) where
  fmap f (UnmarshalStruct g) = UnmarshalStruct (fmap f . g)
  {-# INLINE fmap #-}

-- | Assemble field readers under the high-level constructor with @<$>@ \/ @<*>@;
-- every reader sees the same low-level struct. 'pure' ignores it.
instance Applicative (UnmarshalStruct struct) where
  pure x = UnmarshalStruct (\_ -> pure x)
  UnmarshalStruct f <*> UnmarshalStruct x =
    UnmarshalStruct (\s -> f s <*> x s)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

-- | Read one field: project its C field(s) out and convert them (effectfully) to
-- the Haskell value. The projection may pull several C fields for one value (e.g.
-- @(ptr, len)@ for a 'Data.ByteString.ByteString'). It is the read-side 'at',
-- and carries the same caveat: the projection is unchecked beyond its type, so a
-- wrong but same-typed field still compiles. Matching field to projection is the
-- caller's responsibility.
--
unmarshalField :: (struct -> c) -> (c -> IO hs) -> UnmarshalStruct struct hs
unmarshalField prj conv = UnmarshalStruct (\s -> conv (prj s))
{-# INLINE unmarshalField #-}

-- | Read one field with a pure conversion (no 'IO'), e.g. @'Foreign.C.Types.CInt'
-- -> 'Int'@. The common scalar case.
--
unmarshalFieldPure :: (struct -> c) -> (c -> hs) -> UnmarshalStruct struct hs
unmarshalFieldPure prj conv = UnmarshalStruct (\s -> pure (conv (prj s)))
{-# INLINE unmarshalFieldPure #-}

-- | Read a nullable pointer field: 'Nothing' when NULL, else the converted value.
-- The read-side counterpart of 'marshalOptional'. For a @const@ field, project through
-- 'HsBindgen.Runtime.PtrConst.unsafeToPtr'.
--
unmarshalOptional
  :: (struct -> Ptr a) -> (Ptr a -> IO hs) -> UnmarshalStruct struct (Maybe hs)
unmarshalOptional prj conv = UnmarshalStruct $ \s ->
  case prj s of
    p | p == nullPtr -> pure Nothing
      | otherwise    -> Just <$> conv p
{-# INLINE unmarshalOptional #-}

-- | Read a nested (inline) sub-struct as one field of the enclosing struct, the
-- read-side counterpart of 'marshalNested'.
--
unmarshalNested
  :: (struct -> sub) -> UnmarshalStruct sub hi -> UnmarshalStruct struct hi
unmarshalNested prj inner =
    UnmarshalStruct (\s -> runUnmarshalStruct inner (prj s))
{-# INLINE unmarshalNested #-}
