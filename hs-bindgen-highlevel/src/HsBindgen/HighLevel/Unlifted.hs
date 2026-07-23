-- | Supplying and reading /unlifted/, by-value C values through the @ToHighLevel@
-- combinators.
--
-- == The pattern, in short
--
-- Some C APIs pass small structs /by value/. GHC's FFI cannot pass a struct by
-- value, so a binding holds the struct's raw bytes as an /unlifted boxed/ value: an
-- @R@ (a read-only byte array) for an argument, a @W@ (a mutable byte array) for an
-- out-parameter (see @Clang.Internal.ByValue@ for a worked example of this
-- representation).
--
-- Most of the vocabulary already accepts @R@ \/ @W@ unchanged. In particular an @R@
-- argument needs no new combinator: build the marshaller with the t'Marshal'
-- constructor (or 'bracketUnlifted' below) and drop it into
-- 'HsBindgen.HighLevel.input' as usual.
--
-- This module supplies the two remaining combinators: 'bracketUnlifted' for an @R@
-- argument and 'outputUnlifted' for a @W@ out-parameter. A single 'bracketUnlifted' \/
-- 'outputUnlifted' handles every @R@ \/ @W@ struct. They are the unlifted counterparts
-- of 'HsBindgen.HighLevel.Marshaller.bracket' and 'HsBindgen.HighLevel.output', meant
-- to be imported qualified alongside them.
--
-- == Building the marshallers
--
-- The argument side is a one-liner. With @Clang.Internal.ByValue@, an @R@ argument is
-- supplied by @onHaskellHeap@, which is already a bracket:
--
-- > cursorArg :: Marshal (OnHaskellHeap CXCursor_) (R CXCursor_ -> lo') lo'
-- > cursorArg = bracketUnlifted onHaskellHeap
--
-- The out-parameter side takes more care. An t'Unmarshaller' holds an allocator and a
-- reader /separately/, because 'outputUnlifted' has to open the allocation before the
-- C call and run the read-back after it. Some bindings supply a primitive that does
-- both at once, allocating, running a continuation and freezing in a single bracket,
-- and that has to be taken apart. The code below is written against one such
-- primitive, @preallocate@, from the @libclang-bindings@ package that the
-- @libclang-ffi@ example is built on. Keep the allocation and discard the value it
-- produces:
--
-- > allocW :: forall tag r. HasKnownSize tag => (W tag -> IO r) -> IO r
-- > allocW = fmap snd . preallocate @(OnHaskellHeap tag)
--
-- then write the freeze the reader needs. Both @W@ and @R@ are unlifted, so the
-- frozen array goes to a continuation rather than coming back in 'IO':
--
-- > withFrozenW :: W tag -> (R tag -> IO r) -> IO r
-- > withFrozenW (W marr) k = IO $ \s0 ->
-- >     case unsafeFreezeByteArray# marr s0 of
-- >       (# s1, arr #) -> unIO (k (R arr)) s1
-- >   where
-- >     unIO (IO f) = f
--
-- With those two, the out-parameter is an ordinary t'Unmarshaller':
--
-- > cxStringOut :: Unmarshaller (W CXString_) Text
-- > cxStringOut = Unmarshaller allocW $ \w ->
-- >     withFrozenW w $ \r -> copyOutUtf8 r `finally` wrap_disposeString r
--
-- Note the explicit lambda: @t'Unmarshaller' allocW (copyOutUtf8 . freeze)@ would not
-- compile, because @(.)@ cannot pass an unlifted value between its two functions, and
-- the frozen @R tag@ is one. @allocW@ above composes freely, on the other hand: what
-- passes through its @(.)@ is a @W tag -> IO r@ and an @IO@ action, both lifted.
--
-- For a complete worked version, the @libclang-ffi@ example runs all of this against a
-- real C library that passes structs by value. The README links it along with the
-- others.
module HsBindgen.HighLevel.Unlifted (
    outputUnlifted
  , bracketUnlifted
  ) where

import GHC.Exts (UnliftedType)

import HsBindgen.HighLevel.Internal.Spec (Outputs (..), ToHighLevel (..))
import HsBindgen.HighLevel.Internal.Threading (ThreadIn (..))
import HsBindgen.HighLevel.Marshaller (Marshal (..), Unmarshaller (..))

-- | 'HsBindgen.HighLevel.output' for an /unlifted/ out-parameter: a by-value
-- struct written into a @W@ buffer ('UnliftedType'), the read-back done by an
-- t'Unmarshaller' built over e.g. @Clang.Internal.ByValue.preallocate@. One
-- 'outputUnlifted' covers every @W@ struct.
--
-- It behaves exactly like 'HsBindgen.HighLevel.output' in a spec, with no
-- tail-position restriction: the unlifted value is only ever captured in a closure,
-- never bound by representation-polymorphic code, so high-level arguments and further
-- outputs may follow an unlifted output.
--
outputUnlifted
  :: forall (c :: UnliftedType) hs lo' hi os.
     ThreadIn hi
  => Unmarshaller c hs
  -> ToHighLevel (hs : os) lo'        hi
  -> ToHighLevel os        (c -> lo') hi
outputUnlifted (Unmarshaller allocate readBack) (ToHighLevel rest) =
  ToHighLevel $ \pending lo ->
    -- 'pending' before 'readBack', as in 'HsBindgen.HighLevel.output': spec order.
    threadIn (\k -> allocate (\c -> k (lo c, (\outs v -> v :* outs) <$> pending <*> readBack c)))
             (\(loRest, pending') -> rest pending' loRest)
{-# INLINE outputUnlifted #-}

-- | 'HsBindgen.HighLevel.Marshaller.bracket' for an /unlifted/ C argument: a by-value
-- struct payload passed as an @R@ ('UnliftedType'). One 'bracketUnlifted' covers every
-- @R@ struct. The bracket supplies the value with e.g.
-- @Clang.Internal.ByValue.onHaskellHeap@, and the marshaller drops into
-- 'HsBindgen.HighLevel.input' unchanged.
--
bracketUnlifted
  :: forall (c :: UnliftedType) hs lo'.
     (forall r. hs -> (c -> IO r) -> IO r)
  -> Marshal hs (c -> lo') lo'
bracketUnlifted br = Marshal $ \hs lo k -> br hs (\c -> k (lo c))
{-# INLINE bracketUnlifted #-}
