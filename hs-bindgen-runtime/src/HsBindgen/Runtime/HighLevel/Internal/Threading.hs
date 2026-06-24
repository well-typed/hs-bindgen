{-# LANGUAGE TypeFamilyDependencies #-}

-- | Bracket threading and result flattening behind @input@, @output@, and
-- @scratch@ in "HsBindgen.Runtime.HighLevel". Internal; in their own module
-- because @auto@ in "HsBindgen.Runtime.HighLevel.Defaults" needs 'ThreadIn' in a
-- constraint.
module HsBindgen.Runtime.HighLevel.Internal.Threading (
    ThreadIn (..)
  , ThreadOut (..)
  ) where

import Data.Kind (Constraint)
import GHC.TypeLits (TypeError)

import HsBindgen.Runtime.HighLevel.Internal.Errors (TooManyResults)

-- | Keep an input or scratch bracket (e.g. 'Foreign.C.String.withCString') open
-- while the wrapper's later arguments are applied and the C call runs.
--
-- Read @threadIn br f@ as: introduce a lambda for each remaining argument of the
-- wrapper, then fire the bracket @br@ at the innermost 'IO' with all of them in
-- scope. For @hi = x -> y -> IO z@ it is @\\x y -> br (\\lo -> f lo x y)@.
class ThreadIn lo hi where
  threadIn :: (forall r. (lo -> IO r) -> IO r) -> (lo -> hi) -> hi

instance ThreadIn lo (IO r) where
  threadIn br f = br f
  {-# INLINE threadIn #-}

instance ThreadIn lo rest => ThreadIn lo (arg -> rest) where
  threadIn br f = \arg -> threadIn br (\l -> f l arg)
  {-# INLINE threadIn #-}

-- | Prepend @hs@ onto the result @t@, flattening so a chain of outputs builds a
-- /flat/ tuple: @Cons a (b, c) = (a, b, c)@, and @Cons a b = (a, b)@ when @t@ is not
-- a tuple. Injective (@r -> hs t@), which lets the inverse 'ThreadOut' fundep recover
-- @hs@ and @t@. Tops out at an 8-tuple (the value-level side enumerates one instance
-- per width); a ninth component is a type error via 'ResultFits', not a silent nest.
type family Cons hs t = r | r -> hs t where
  Cons hs (a, b, c, d, e, f, g) = (hs, a, b, c, d, e, f, g)
  Cons hs (a, b, c, d, e, f)    = (hs, a, b, c, d, e, f)
  Cons hs (a, b, c, d, e)       = (hs, a, b, c, d, e)
  Cons hs (a, b, c, d)          = (hs, a, b, c, d)
  Cons hs (a, b, c)             = (hs, a, b, c)
  Cons hs (a, b)                = (hs, a, b)
  Cons hs t                     = (hs, t)

-- | Value-level 'Cons': term-level dispatch on @t@'s width, mirroring each 'Cons'
-- clause (the @Cons hs t@ return type keeps the two in sync).
class ConsV hs t where
  consV :: hs -> t -> Cons hs t
instance ConsV hs (a, b, c, d, e, f, g) where
  consV h (a, b, c, d, e, f, g) = (h, a, b, c, d, e, f, g)
  {-# INLINE consV #-}
instance ConsV hs (a, b, c, d, e, f) where
  consV h (a, b, c, d, e, f) = (h, a, b, c, d, e, f)
  {-# INLINE consV #-}
instance ConsV hs (a, b, c, d, e) where
  consV h (a, b, c, d, e) = (h, a, b, c, d, e)
  {-# INLINE consV #-}
instance ConsV hs (a, b, c, d) where
  consV h (a, b, c, d) = (h, a, b, c, d)
  {-# INLINE consV #-}
instance ConsV hs (a, b, c) where
  consV h (a, b, c) = (h, a, b, c)
  {-# INLINE consV #-}
instance ConsV hs (a, b) where
  consV h (a, b) = (h, a, b)
  {-# INLINE consV #-}
instance {-# OVERLAPPABLE #-} (Cons hs t ~ (hs, t)) => ConsV hs t where
  consV h t = (h, t)
  {-# INLINE consV #-}

-- | Reject a result wider than the eight-component flat tuple. An @output@
-- prepends one component with 'Cons'; when the result so far is already an
-- eight-tuple, one more would nest silently, so this fires 'TooManyResults' in the
-- 'ThreadOut' base instance instead.
type family ResultFits t :: Constraint where
  ResultFits (a, b, c, d, e, f, g, h) = TypeError TooManyResults
  ResultFits _                        = ()

-- | The output counterpart of 'ThreadIn': thread the output's allocation past the
-- wrapper's later arguments (exactly as 'ThreadIn' does), then prepend its peeked
-- value @hs@ to the result, turning @hi@ into @hi'@. So @hs = a@ with @hi = IO (b,
-- c)@ gives @hi' = IO (a, b, c)@ (the flattening is 'Cons').
--
-- The two fundeps run inference both ways: forward (@c hs hi -> hi'@) builds the
-- result type, inverse (@c hi' -> hs hi@) recovers @hs@ and @hi@ from the declared
-- return. The inverse is what lets a trailing @auto@ follow an output and pins a
-- typed hole in an output slot.
class ThreadOut c hs hi hi' | c hs hi -> hi', c hi' -> hs hi where
  threadOut :: (forall r. (c -> IO r) -> IO (hs, r)) -> (c -> hi) -> hi'

instance (ConsV hs t, flat ~ Cons hs t, ResultFits t)
      => ThreadOut c hs (IO t) (IO flat) where
  threadOut br f = do (hs, t) <- br f; pure (consV hs t)
  {-# INLINE threadOut #-}

instance ThreadOut c hs rest rest'
       => ThreadOut c hs (arg -> rest) (arg -> rest') where
  threadOut br f = \arg -> threadOut br (\s -> f s arg)
  {-# INLINE threadOut #-}
