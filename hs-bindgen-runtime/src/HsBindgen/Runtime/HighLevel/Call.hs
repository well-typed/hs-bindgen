{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The 'hl' combinator: lift a low-level binding to a user-chosen
-- high-level signature.
--
-- 'HighLevel' is the analogue of @servant@'s @HasServer@: a recursive
-- elaboration over the high-level signature with two helper classes
-- for the tricky bits.
--
-- * 'Spread' applies a tupled 'CSlot' to a curried C function.
-- * 'Thread' threads a bracket through the remaining function shape.
--
-- 'HighLevel' itself has two instances (base + recursive); arbitrary
-- Haskell arity is supported via structural recursion on @(->)@.
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.HighLevel.Call qualified as Call
module HsBindgen.Runtime.HighLevel.Call (
    -- * The class
    HighLevel (..)
    -- * Helpers (exposed for advanced use; usually not needed)
  , Spread (..)
  , Thread (..)
  ) where

import HsBindgen.Runtime.HighLevel.FromC (FromC (..))
import HsBindgen.Runtime.HighLevel.ToC (CSlot, ToC (..))

{-------------------------------------------------------------------------------
  Spread: apply a tupled slot to a curried C function.
-------------------------------------------------------------------------------}

-- | Apply @t@'s components, in order, to a curried function. For
-- non-tuple @t@, applies @t@ directly. Per-tuple-arity instances ship
-- up to 4-tuples — real C functions rarely need more.
class Spread t lo lo' | t lo -> lo' where
  spread :: t -> lo -> lo'

instance Spread (a, b) (a -> b -> lo) lo where
  spread (a, b) f = f a b

instance Spread (a, b, c) (a -> b -> c -> lo) lo where
  spread (a, b, c) f = f a b c

instance Spread (a, b, c, d) (a -> b -> c -> d -> lo) lo where
  spread (a, b, c, d) f = f a b c d

-- | Non-tuple fallback. 'OVERLAPPABLE' so the tuple instances win.
instance {-# OVERLAPPABLE #-} Spread t (t -> lo) lo where
  spread t f = f t

{-------------------------------------------------------------------------------
  Thread: thread a bracket through the remaining function shape.
-------------------------------------------------------------------------------}

-- | Thread a Codensity-style bracket @forall r. (b -> IO r) -> IO r@
-- through a remaining-function-shape @g@. The bracket fires at the
-- innermost 'IO'.
--
-- The step case structurally introduces a fresh lambda for each
-- remaining argument; this is /not/ plain monadic composition
-- (there is no @(>>=)@ that traverses a function arrow).
--
-- Worked example, lifting
-- @c_strncmp :: PtrConst CChar -> PtrConst CChar -> CSize -> IO CInt@
-- through one 'String' argument:
--
-- > thread (withC s) (\\slot -> spread slot c_strncmp)
-- >   = \\arg1 -> thread … (\\b -> c_strncmp b arg1)              -- step
-- >   = \\arg1 -> \\arg2 -> thread … (\\b -> c_strncmp b arg1 arg2) -- step
-- >   = \\arg1 -> \\arg2 -> withC s (\\b -> c_strncmp b arg1 arg2)  -- base
class Thread b g where
  thread :: (forall r. (b -> IO r) -> IO r) -> (b -> g) -> g

instance Thread b (IO r) where
  thread br f = br f

instance Thread b rest => Thread b (arg -> rest) where
  thread br f = \arg -> thread br (\b -> f b arg)

{-------------------------------------------------------------------------------
  HighLevel: lift a low-level binding to a user-chosen high-level signature.
-------------------------------------------------------------------------------}

-- | Lift a low-level binding @lo@ to a high-level signature @hi@.
-- The method 'hl' is the user-facing combinator:
--
-- > hsStrlen :: String -> IO Int
-- > hsStrlen = hl strlen
class HighLevel hi lo where
  hl :: lo -> hi

-- | Base case: convert the C-side return value via 'FromC'.
instance FromC c hs => HighLevel (IO hs) (IO c) where
  hl mc = fromC <$> mc

-- | Argument step: 'Spread' consumes the slot(s); 'Thread' defers
-- @x@'s bracket until the innermost 'IO'; recurse on @rest@.
instance
     ( ToC a
     , Spread (CSlot a) lo loSpread
     , Thread (CSlot a) loSpread
     , HighLevel rest loSpread
     )
  => HighLevel (a -> rest) lo
  where
    hl loFn = \x ->
      hl @rest (thread (withC x) (\slot -> spread slot loFn))
