-- The "no default" fallback instance below omits its associated 'DefInArrow'
-- equation: a general row would conflict with every concrete one, and its
-- 'TypeError' fires before the family is consulted. That omission trips
-- -Wmissing-methods, silenced here; every real method is still defined.
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Default marshallers for 'ToHighLevel': one canonical conversion per C type,
-- dropped into a wrapper as @'input' 'defaultIn'@, @'output' 'defaultOut'@, and
-- @defaultRes@ (the closer, used directly).
--
-- This module is policy: it fixes one canonical Haskell type per C type (e.g.
-- @CInt@ ↔ @Int@, @const char *@ ↔ 'String'). For a different representation, use
-- an explicit marshaller: a constructor from
-- "HsBindgen.Runtime.HighLevel.ToHighLevel" or a ready-made one from
-- "HsBindgen.Runtime.HighLevel.ToHighLevel.Marshallers". Import this module only if
-- you want the defaults.
--
-- The Haskell type determines the C representation: 'DefaultIn' maps @hs@ to the C
-- argument(s) it fills (via the associated 'DefInArrow' family), 'DefaultOut' maps
-- @hs@ to its out-pointer, 'DefaultRes' maps the C return @c@ to @hs@.
--
-- __Warning: silent numeric conversion.__ The scalar defaults coerce with
-- 'fromIntegral' / 'realToFrac', which are lossy and silent: @Int -> CInt@
-- truncates a 64-bit 'Int' to 32 bits, and @CSize -> Int@ can wrap a large
-- @size_t@ to a negative 'Int'. An explicit @'pureIn' 'fromIntegral'@ makes the
-- choice visible at the call site; through 'defaultIn' / 'defaultRes' it is not.
-- If a binding can carry values outside the target type's range, use an explicit
-- marshaller with a checked conversion.
--
module HsBindgen.Runtime.HighLevel.ToHighLevel.Defaults (
    -- * Default marshallers
    --
    -- | Use as @'input' 'defaultIn'@, @'output' 'defaultOut'@, and @defaultRes@.
    --
    -- | 'DefaultIn' carries its C-argument shape in the associated open type family
    -- 'DefInArrow'; the @(..)@ export brings 'DefInArrow' and 'defaultIn' into scope.
    DefaultIn (..)
  , DefaultOut (..)
  , DefaultRes (..)
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Foreign.C.Types (CBool, CChar, CDouble, CFloat, CInt, CSize, CUInt)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import GHC.TypeLits (TypeError)

import HsBindgen.Runtime.CBool qualified as CBool
import HsBindgen.Runtime.HighLevel.ToHighLevel
import HsBindgen.Runtime.HighLevel.ToHighLevel.Errors (NoDefault)
import HsBindgen.Runtime.HighLevel.ToHighLevel.Marshallers (useAsByteStringLenIn,
                                                            withCStringIn,
                                                            withConstIncompleteArrayIn)
import HsBindgen.Runtime.IncompleteArray (IncompleteArray)
import HsBindgen.Runtime.PtrConst (PtrConst)

{-------------------------------------------------------------------------------
  Default marshaller classes
-------------------------------------------------------------------------------}

-- | The canonical input marshaller for a Haskell type @hs@, with the C argument(s)
-- it fills.
--
-- The associated open family 'DefInArrow' fixes those C arguments, prepended onto
-- the rest of the callable @lo@ (a scalar fills one, a 'ByteString' fills the
-- @(const char *, size_t)@ pair). It lets @'input' 'defaultIn'@ infer the C side
-- with no annotation, where a bare @'pureIn' 'fromIntegral'@ cannot.
--
-- A binding gives its own type a default with one instance block: a 'DefInArrow'
-- row and a 'defaultIn'. For @newtype Handle = Handle CInt@:
--
-- > instance DefaultIn Handle where
-- >   type DefInArrow Handle lo = CInt -> lo
-- >   defaultIn = pureIn (\(Handle h) -> h)
class DefaultIn hs where
  type DefInArrow hs lo :: Type
  defaultIn :: InMarshaller (DefInArrow hs lo) lo hs

-- | The canonical output marshaller for a Haskell type @hs@, coercing a C scalar
-- to a Haskell scalar. Buffer-sized outputs (strings, arrays) have no canonical
-- length, so they have no default and stay explicit.
--
class DefaultOut hs c | hs -> c where
  defaultOut :: OutMarshaller c hs

-- | The canonical result closer for a C return type @c@: drop it straight into a
-- spec in the closing position, as @... $ defaultRes@.
--
class DefaultRes c hs | c -> hs where
  defaultRes :: ToHighLevel (IO c) (IO hs)

{-------------------------------------------------------------------------------
  DefaultIn instances
-------------------------------------------------------------------------------}

instance DefaultIn Int where
  type DefInArrow Int lo = CInt -> lo
  defaultIn = pureIn fromIntegral
instance DefaultIn Word where
  type DefInArrow Word lo = CUInt -> lo
  defaultIn = pureIn fromIntegral
instance DefaultIn Double where
  type DefInArrow Double lo = CDouble -> lo
  defaultIn = pureIn realToFrac
instance DefaultIn Float where
  type DefInArrow Float lo = CFloat -> lo
  defaultIn = pureIn realToFrac
instance DefaultIn Bool where
  type DefInArrow Bool lo = CBool -> lo
  defaultIn = pureIn CBool.fromBool
instance DefaultIn String where
  type DefInArrow String lo = PtrConst CChar -> lo
  defaultIn = withCStringIn
instance DefaultIn ByteString where
  type DefInArrow ByteString lo = PtrConst CChar -> CSize -> lo
  defaultIn = useAsByteStringLenIn

instance Storable a => DefaultIn (IncompleteArray a) where
  type DefInArrow (IncompleteArray a) lo = PtrConst a -> lo
  defaultIn = withConstIncompleteArrayIn

{-------------------------------------------------------------------------------
  DefaultOut instances (coerce policy: Ptr CInt -> Int, etc.)
-------------------------------------------------------------------------------}

instance DefaultOut Int    (Ptr CInt)    where defaultOut = peekOutPure fromIntegral
instance DefaultOut Word   (Ptr CUInt)   where defaultOut = peekOutPure fromIntegral
instance DefaultOut Double (Ptr CDouble) where defaultOut = peekOutPure realToFrac
instance DefaultOut Float  (Ptr CFloat)  where defaultOut = peekOutPure realToFrac
instance DefaultOut Bool   (Ptr CBool)   where defaultOut = peekOutPure CBool.toBool

{-------------------------------------------------------------------------------
  DefaultRes instances
-------------------------------------------------------------------------------}

instance DefaultRes CInt    Int    where defaultRes = resultPure fromIntegral
instance DefaultRes CUInt   Word   where defaultRes = resultPure fromIntegral
instance DefaultRes CSize   Int    where defaultRes = resultPure fromIntegral
instance DefaultRes CDouble Double where defaultRes = resultPure realToFrac
instance DefaultRes CFloat  Float  where defaultRes = resultPure realToFrac
instance DefaultRes CBool   Bool   where defaultRes = resultPure CBool.toBool
instance DefaultRes ()      ()     where defaultRes = resultPure id

{-------------------------------------------------------------------------------
  "No default" fallbacks

  When a wrapper reaches for a default that does not exist (via 'input' 'defaultIn'
  / 'output' 'defaultOut' / 'defaultRes', the type inferred from the wrapper
  signature), these overlappable instances turn the cryptic ambiguity error into a
  clear message. The 'TypeError' lives in the instance context, so it fires only
  when the instance is selected.
-------------------------------------------------------------------------------}

-- Uninhabited placeholders so the 'DefaultOut'/'DefaultRes' fallbacks satisfy
-- their functional-dependency coverage condition; never reached at runtime.
data NoCArg
data NoHs

-- Omits the 'DefInArrow' equation on purpose (see the -Wno-missing-methods note
-- at the top of the module): the 'TypeError' fires before the family is read.
instance {-# OVERLAPPABLE #-} TypeError (NoDefault "input" hs) => DefaultIn hs where
  defaultIn = errorWithoutStackTrace "ToHighLevel.Defaults: unreachable"

instance {-# OVERLAPPABLE #-} (c ~ NoCArg, TypeError (NoDefault "output" hs))
      => DefaultOut hs c where
  defaultOut = errorWithoutStackTrace "ToHighLevel.Defaults: unreachable"

instance {-# OVERLAPPABLE #-} (hs ~ NoHs, TypeError (NoDefault "result" c))
      => DefaultRes c hs where
  defaultRes = errorWithoutStackTrace "ToHighLevel.Defaults: unreachable"
