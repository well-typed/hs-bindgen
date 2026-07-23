{-# LANGUAGE TypeFamilyDependencies #-}

-- | Bracket threading and result flattening behind @input@, @output@, and
-- @scratch@ in "HsBindgen.HighLevel". Internal; in their own module
-- because @auto@ in "HsBindgen.HighLevel.Defaults" needs 'ThreadIn' in a
-- constraint.
module HsBindgen.HighLevel.Internal.Threading (
    ThreadIn (..)
  , ThreadOut (..)
  , StripUnit (..)
  , DropUnit (..)
  ) where

import Data.Kind (Constraint, Type)
import GHC.Exts (RuntimeRep, TYPE)
import GHC.TypeLits (TypeError)

import HsBindgen.HighLevel.Internal.Errors (TooManyResults)

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
--
-- @c@ is representation-polymorphic, so an output can be a lifted @'Foreign.Ptr.Ptr'@
-- or an unlifted by-value @R@ \/ @W@ struct payload (see 'output' \/ 'outputUnlifted'
-- in "HsBindgen.HighLevel"). Only the base case is polymorphic in @c@; the
-- recursive case must bind the C value and so stays lifted (see below).
type ThreadOut ::
     forall (rep :: RuntimeRep). TYPE rep -> Type -> Type -> Type -> Constraint
class ThreadOut c hs hi hi' | c hs hi -> hi', c hi' -> hs hi where
  threadOut :: (forall r. (c -> IO r) -> IO (hs, r)) -> (c -> hi) -> hi'

-- Base case. Representation-polymorphic in @c@: it never binds a @c@-value (it only
-- passes @f@ into @br@), so it covers a lifted @Ptr@ output and an unlifted @R@ \/
-- @W@ by-value output alike, and the inverse fundep fires for both.
instance (ConsV hs t, flat ~ Cons hs t, ResultFits t)
      => ThreadOut (c :: TYPE rep) hs (IO t) (IO flat) where
  threadOut br f = do (hs, t) <- br f; pure (consV hs t)
  {-# INLINE threadOut #-}

-- Recursive case: threads the output past a later wrapper argument. It binds
-- @s :: c@ (in @\\s -> f s arg@), and a representation-polymorphic binder is
-- illegal, so this stays fixed to the lifted representation. An unlifted output is
-- therefore supported only in tail position, where nothing binds its @c@.
instance ThreadOut c hs rest rest'
       => ThreadOut (c :: Type) hs (arg -> rest) (arg -> rest') where
  threadOut br f = \arg -> threadOut br (\s -> f s arg)
  {-# INLINE threadOut #-}

-- | Drop a trailing @()@ tuple component from a flat result, one instance per
-- width (2- through 8-tuple, matching 'Cons'). Purely a value-and-type rewrite;
-- 'HsBindgen.HighLevel.dropTrailingUnit' threads it under any remaining
-- wrapper arguments.
--
-- This is deliberately __forward-only__ (@t -> t'@ with no inverse). A void closer
-- ('HsBindgen.HighLevel.discardResult' \/ 'throwOnNonZero') contributes a
-- trailing @()@, and that @()@ is load-bearing __during construction__: it is what
-- lets 'ThreadOut'\'s inverse fundep tell "one output plus a void closer" from "no
-- outputs plus a value closer", and so pins a typed hole in an output slot.
-- Collapsing it inside 'Cons' would make that inference ambiguous; collapsing it
-- here, as a separate pass after the spec is built, cannot.
class StripUnit t t' | t -> t' where
  stripUnit :: t -> t'
instance StripUnit (a, ())                   a                     where stripUnit (a, ())                   = a
instance StripUnit (a, b, ())                (a, b)                where stripUnit (a, b, ())                = (a, b)
instance StripUnit (a, b, c, ())             (a, b, c)             where stripUnit (a, b, c, ())             = (a, b, c)
instance StripUnit (a, b, c, d, ())          (a, b, c, d)          where stripUnit (a, b, c, d, ())          = (a, b, c, d)
instance StripUnit (a, b, c, d, e, ())       (a, b, c, d, e)       where stripUnit (a, b, c, d, e, ())       = (a, b, c, d, e)
instance StripUnit (a, b, c, d, e, f, ())    (a, b, c, d, e, f)    where stripUnit (a, b, c, d, e, f, ())    = (a, b, c, d, e, f)
instance StripUnit (a, b, c, d, e, f, g, ()) (a, b, c, d, e, f, g) where stripUnit (a, b, c, d, e, f, g, ()) = (a, b, c, d, e, f, g)

-- | Thread 'stripUnit' under the wrapper's remaining arguments down to the final
-- @IO@ (the outer counterpart of 'ThreadIn' \/ 'ThreadOut'), so a trailing @()@ is
-- dropped whether or not the wrapper still takes arguments.
class DropUnit hi hi' | hi -> hi' where
  dropUnit :: hi -> hi'
instance StripUnit t t' => DropUnit (IO t) (IO t') where
  dropUnit = fmap stripUnit
  {-# INLINE dropUnit #-}
instance DropUnit rest rest' => DropUnit (arg -> rest) (arg -> rest') where
  dropUnit f = dropUnit . f
  {-# INLINE dropUnit #-}
