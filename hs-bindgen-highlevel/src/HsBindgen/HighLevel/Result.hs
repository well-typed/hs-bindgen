{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Exposing a deterministic C call as a pure function.
--
-- Some C calls really are functions: a hash, a cipher, a numeric kernel. Their
-- bindings still come out in 'IO', because nothing in the FFI can know otherwise.
-- 'toHighLevelPure' runs a spec the way 'HsBindgen.HighLevel.toHighLevel' does but hands
-- it back with the 'IO' taken off, so the binding's signature is the one you would
-- write for an ordinary Haskell function:
--
-- > encrypt :: Key -> Nonce -> ByteString -> ByteString
-- > encrypt = toHighLevelPure crypto_secretbox_easy spec
--
-- The name is the contract: /you/ assert that the call is a pure function of its
-- inputs, and nothing here checks it.
module HsBindgen.HighLevel.Result (
    -- * Closing a spec as a pure function
    toHighLevelPure
  , Unpurify
  , Purifiable
    -- * Machinery
    --
    -- | What 'Purifiable' abbreviates. A binding never names these.
  , IsFunction
  , PurifyAt (..)
  ) where

import Data.Kind (Constraint, Type)
import System.IO.Unsafe (unsafePerformIO)

import HsBindgen.HighLevel.Internal.Spec (ToHighLevel, toHighLevel)

{-------------------------------------------------------------------------------
  Relating the two signatures

  A spec produces a function ending in IO; 'toHighLevelPure' produces the pure
  function. Something has to say those are the same binding, and it can only be said
  in one direction: stripping the IO is a function on types, putting it back is not,
  because @a -> b@ is what both @a -> IO b@ and @IO (a -> b)@ strip to.

  It goes in the direction 'auto' needs. 'auto' reads argument types off the
  signature the binding was given, and here that signature is the pure one, so the
  high-level type has to be computed *from* it.
-------------------------------------------------------------------------------}

-- | The high-level type a pure signature stands for: every argument kept, the result
-- moved into 'IO'.
--
-- > Unpurify (Key -> Nonce -> ByteString ->    ByteString)
-- >   =       Key -> Nonce -> ByteString -> IO ByteString
--
-- This is the type a spec has to build for 'toHighLevelPure' to accept it, and
-- writing it as a family of the pure signature rather than the other way round is
-- what lets 'HsBindgen.HighLevel.Auto.auto' work under 'toHighLevelPure'.
--
-- The first equation wins on an arrow, so a signature whose /result/ is itself a
-- function cannot be reached: @Unpurify (a -> b)@ is @a -> IO b@, never
-- @IO (a -> b)@. Close that one with 'HsBindgen.HighLevel.toHighLevel' and strip the
-- 'IO' yourself.
type Unpurify :: Type -> Type
type family Unpurify hs where
  Unpurify (x -> r) = x -> Unpurify r
  Unpurify t        = IO t

-- | Does the pure signature still take an argument?
--
-- Stuck rather than @\'False@ on a signature that is not yet known, so a binding
-- without one reports its ambiguity instead of committing to a result type.
type IsFunction :: Type -> Bool
type family IsFunction hs where
  IsFunction (x -> r) = 'True
  IsFunction t        = 'False

-- | The constraint 'toHighLevelPure' carries: @hs@ is a signature it can strip the
-- 'IO' out of. Every signature is, so this is only ever discharged, never written.
type Purifiable :: Type -> Constraint
type Purifiable hs = PurifyAt (IsFunction hs) hs

-- | The recursion behind 'Purifiable', indexed by whether the pure signature still
-- takes an argument.
--
-- Indexed on the signature @hs@ rather than on the high-level type, so that the walk is
-- driven by the type the binding was given. The high-level type it consumes is
-- @'Unpurify' hs@ at every step.
class PurifyAt (isFunction :: Bool) hs where
  -- | Invoked through 'toHighLevelPure'.
  purifyAt :: Unpurify hs -> hs

-- One more argument: pass it along and carry on under it.
instance forall x r hs.
         (hs ~ (x -> r), PurifyAt (IsFunction r) r)
      => PurifyAt 'True hs where
  purifyAt f = \x -> purifyAt @(IsFunction r) (f x)
  {-# INLINE purifyAt #-}

-- No more arguments, so what is left is the 'IO' the caller asserts away. Nothing
-- about the result is guaranteed pure: it is produced with 'unsafePerformIO', and
-- correctness rests entirely on that assertion.
instance (Unpurify hs ~ IO hs) => PurifyAt 'False hs where
  purifyAt = unsafePerformIO
  {-# INLINE purifyAt #-}

{-------------------------------------------------------------------------------
  Closing a spec as a pure function
-------------------------------------------------------------------------------}

-- | Run a finished spec against a low-level callable and expose the binding as a
-- pure function. 'HsBindgen.HighLevel.toHighLevel' with the 'IO' taken off the
-- result, and with the same argument order: the callable first, so the spec chains
-- onto it with @($)@.
--
-- > encrypt :: Key -> Nonce -> ByteString -> ByteString
-- > encrypt = toHighLevelPure crypto_secretbox_easy spec
--
-- The caller asserts the call is a pure function of its inputs: deterministic, no
-- side effects, no global state, no randomness. Nothing here checks that, and the
-- value is produced with 'unsafePerformIO'. Pair it with a non-throwing closer so
-- the result is total.
--
-- @hs@ is the signature written on the binding, so the spec is checked against
-- @'Unpurify' hs@, that signature with the 'IO' put back. That is the whole of what
-- the type says, and it is what lets 'HsBindgen.HighLevel.Auto.auto' read the
-- argument types off a /pure/ signature:
--
-- > hsHypot :: Double -> Double -> Double
-- > hsHypot = toHighLevelPure c_hypot auto
toHighLevelPure ::
     forall hs lo. Purifiable hs
  => lo
  -> ToHighLevel '[] lo (Unpurify hs)
  -> hs
toHighLevelPure lo = purifyAt @(IsFunction hs) . toHighLevel lo
{-# INLINE toHighLevelPure #-}
