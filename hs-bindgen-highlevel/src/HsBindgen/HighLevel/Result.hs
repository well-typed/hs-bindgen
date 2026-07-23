-- | Expose a wrapper over a deterministic C call (a crypto primitive, a hash) as a
-- pure function with 'assertPure': @encrypt = assertPure raw@.
--
module HsBindgen.HighLevel.Result (
    Purify
  , Purifiable (..)
  , assertPure
  ) where

import System.IO.Unsafe (unsafePerformIO)

-- | The type 'assertPure' gives a wrapper: its own type with the @IO@ stripped off
-- the result. A wrapper @a -> b -> IO c@ purifies to @a -> b -> c@.
type family Purify hi where
  Purify (x -> r) = x -> Purify r
  Purify (IO t)   = t

-- | Walk under a wrapper's arguments to the @IO@ at the end and strip it. Nothing
-- about the returned value is pure: the leaf instance forces it with
-- 'unsafePerformIO', so correctness rests entirely on the caller's assertion that
-- the call is deterministic (see 'assertPure').
class Purifiable hi where
  -- | Usually reached through 'assertPure', the same function under a name that
  -- flags the proof obligation.
  purify :: hi -> Purify hi
instance Purifiable r => Purifiable (x -> r) where
  purify f = \x -> purify (f x)
  {-# INLINE purify #-}
instance Purifiable (IO t) where
  purify = unsafePerformIO
  {-# INLINE purify #-}

-- | Expose a wrapper over a deterministic C call as a pure function.
--
-- > encrypt :: Key -> Nonce -> ByteString -> ByteString
-- > encrypt = assertPure (toHighLevel spec crypto_secretbox_easy)
--
-- The caller asserts the call is a pure function of its inputs: deterministic, no
-- side effects, no global state, no randomness. Pair it with a non-throwing closer so
-- the result is total.
assertPure :: Purifiable hi => hi -> Purify hi
assertPure = purify
{-# INLINE assertPure #-}
