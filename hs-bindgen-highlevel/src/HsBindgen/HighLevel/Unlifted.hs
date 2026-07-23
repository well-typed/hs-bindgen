-- | Supplying and reading /unlifted/, by-value C values through the @ToHighLevel@
-- combinators.
--
-- == The pattern, in short
--
-- Some C APIs pass small structs /by value/. GHC's FFI cannot pass a struct by
-- value, so a binding holds the struct's raw bytes as an /unlifted boxed/ value: an
-- @R@ (a read-only byte array) for an argument, a @W@ (a mutable byte array) for an
-- out-parameter (see @Clang.Internal.ByValue@ for a worked example of this
-- representation). "Unlifted" means a /different runtime representation/ than an
-- ordinary lifted 'Data.Kind.Type', and you cannot instantiate a @Type@ variable
-- with one. That is the wall the ordinary combinators hit.
--
-- Most of the vocabulary is past that wall already. 'Marshal',
-- 'HsBindgen.HighLevel.input', 'Unmarshaller', and
-- 'HsBindgen.HighLevel.Internal.Threading.ThreadOut' mention the C-side type
-- only in a function-argument position and never /bind/ a value of it, so they are
-- representation-polymorphic and accept @R@ \/ @W@ unchanged. In particular an @R@
-- argument needs no new combinator: build the marshaller with the 'Marshal'
-- constructor (or 'bracketUnlifted' below) and drop it into
-- 'HsBindgen.HighLevel.input' as usual.
--
-- This module is the two remaining spots. 'bracketUnlifted' and 'outputUnlifted'
-- must /bind/ the C value to thread it into the call, and a
-- representation-polymorphic binder is illegal, so each pins its C type to the
-- unlifted-boxed representation. That is legal (it is a concrete representation), and
-- one definition of each covers /every/ @R@ \/ @W@, since they all share it. They are
-- the exact unlifted counterparts of 'bracket' and
-- 'HsBindgen.HighLevel.output', built to be imported qualified alongside
-- them.
--
-- == Building the marshallers
--
-- Construct the 'Unmarshaller' \/ 'Marshal' over the binding's by-value primitives.
-- With @Clang.Internal.ByValue@, an @R@ argument uses @onHaskellHeap@ and a @W@
-- out-parameter uses @preallocate@:
--
-- > cxStringOut :: Unmarshaller (W CXString_) Text
-- > cxStringOut = Unmarshaller $ \k -> do
-- >   (onHeapString, r) <- preallocate k          -- fill the W buffer
-- >   text <- copyAndDispose onHeapString          -- read it back
-- >   pure (text, r)
-- >
-- > cursorArg :: Marshal (OnHaskellHeap CXCursor_) (R CXCursor_ -> lo') lo'
-- > cursorArg = bracketUnlifted onHaskellHeap
--
-- == Limitation
--
-- 'outputUnlifted' supports an unlifted output only in __tail position__ (no wrapper
-- argument after it); see its own note. By-value out-parameters are the return slot
-- and sit last, so this is rarely a constraint.
module HsBindgen.HighLevel.Unlifted (
    outputUnlifted
  , bracketUnlifted
  ) where

import GHC.Exts (UnliftedType)

import HsBindgen.HighLevel.Internal.Spec (ToHighLevel (..))
import HsBindgen.HighLevel.Internal.Threading (ThreadOut (..))
import HsBindgen.HighLevel.Marshaller (Marshal (..), Unmarshaller (..))

-- | 'HsBindgen.HighLevel.output' for an /unlifted/ out-parameter: a by-value
-- struct written into a @W@ buffer ('UnliftedType'), the read-back done by an
-- 'Unmarshaller' built over e.g. @Clang.Internal.ByValue.preallocate@. The body binds
-- the C value to thread it into the call, so (unlike the representation-polymorphic
-- 'Unmarshaller' and 'ThreadOut' it shares) it pins the unlifted-boxed
-- representation; that single sibling covers every @W@.
--
-- The unlifted output must be in __tail position__ (no wrapper argument after it):
-- threading an output past a later argument binds the C value in the recursive
-- 'ThreadOut' case, which stays lifted. In practice a by-value out-parameter is the
-- return slot and sits last, so this is rarely a constraint.
--
outputUnlifted
  :: forall (c :: UnliftedType) hs lo' hi hi'.
     ThreadOut c hs hi hi'
  => Unmarshaller c hs
  -> ToHighLevel lo' hi
  -> ToHighLevel (c -> lo') hi'
outputUnlifted (Unmarshaller m) (ToHighLevel rest) =
  ToHighLevel $ \lo -> threadOut m $ \c -> rest (lo c)
{-# INLINE outputUnlifted #-}

-- | 'bracket' for an /unlifted/ C argument: a by-value struct payload passed as an
-- @R@ ('UnliftedType'). Identical in shape to 'bracket', but @c@ is pinned to the
-- unlifted-boxed representation. That is the one spot the lifted 'bracket' \/
-- 'HsBindgen.HighLevel.Marshaller.scalar' cannot reach, because the bracket
-- binds the C value and a representation-polymorphic binder is illegal; pinning the
-- representation makes the binder legal, and one definition covers every @R@ (all
-- share that representation). The bracket supplies the value with e.g.
-- @Clang.Internal.ByValue.onHaskellHeap@, and the marshaller drops into
-- 'HsBindgen.HighLevel.input' unchanged.
--
bracketUnlifted
  :: forall (c :: UnliftedType) e lo'.
     (forall r. e -> (c -> IO r) -> IO r)
  -> Marshal e (c -> lo') lo'
bracketUnlifted br = Marshal $ \e lo k -> br e (\c -> k (lo c))
{-# INLINE bracketUnlifted #-}
