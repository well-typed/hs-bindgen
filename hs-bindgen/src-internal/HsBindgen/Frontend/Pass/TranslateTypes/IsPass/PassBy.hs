module HsBindgen.Frontend.Pass.TranslateTypes.IsPass.PassBy (
    PassResBy (..)
  , PassArgBy (..)
  , byAddressFFIType
  , passArgByFFIType
  ) where

import Data.Proxy (Proxy (Proxy))

import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass (CoercePass (coercePass),
                          CoercePassTypeFunArg (coercePassTypeFunArg), IsPass,
                          PassTypes (TypeFunArg))
import HsBindgen.IR.Pass.Types (CoercePassTypes (coercePassTypes),
                                PassTypes (Types))

-- | How a function result is passed from C to Haskell
--
-- Union, struct, and complex results cannot be handled by the Haskell FFI
-- directly; they are passed by address.
--
-- See also 'HsBindgen.Backend.Hs.Translation.Function.functionDecs'.
data PassResBy p =
    PassResVoid
  | PassResByValue Hs.FFIType
    -- | Type of the pointer to the result, as it appears in the C wrapper and
    -- in the foreign import of the C wrapper
  | PassResByAddress (Types p)

deriving stock instance IsPass p => Show (PassResBy p)
deriving stock instance IsPass p => Eq (PassResBy p)

instance (
      CoercePassTypes p p'
    ) => CoercePass PassResBy p p' where
  coercePass = \case
      PassResVoid         -> PassResVoid
      PassResByValue   t  -> PassResByValue t
      PassResByAddress ty ->
        PassResByAddress $ coercePassTypes (Proxy @'(p, p')) ty

-- | How a function argument is passed from Haskell to C
--
-- Union, struct, and complex arguments cannot be handled by the Haskell FFI
-- directly; they are passed by address.
--
-- See also 'HsBindgen.Backend.Hs.Translation.Function.functionDecs'.
data PassArgBy p =
    PassArgByValue Hs.FFIType
    -- | Type of the pointer to the argument, as it appears in the C wrapper
    -- and in the foreign import of the C wrapper
  | PassArgByAddress (TypeFunArg p)

deriving stock instance IsPass p => Show (PassArgBy p)
deriving stock instance IsPass p => Eq (PassArgBy p)

instance (
      CoercePassTypeFunArg p p'
    ) => CoercePass PassArgBy p p' where
  coercePass = \case
      PassArgByValue   t  -> PassArgByValue t
      PassArgByAddress ty ->
        PassArgByAddress $ coercePassTypeFunArg (Proxy @'(p, p')) ty

-- | FFI type of a value passed by address
byAddressFFIType :: Hs.FFIType
byAddressFFIType = Hs.FFIPtrVoid

-- | FFI type of an argument in the foreign import of the C wrapper
passArgByFFIType :: PassArgBy p -> Hs.FFIType
passArgByFFIType = \case
    PassArgByValue   t -> t
    PassArgByAddress _ -> byAddressFFIType
