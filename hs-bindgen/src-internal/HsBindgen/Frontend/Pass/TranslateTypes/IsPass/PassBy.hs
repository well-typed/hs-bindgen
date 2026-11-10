module HsBindgen.Frontend.Pass.TranslateTypes.IsPass.PassBy (
    PassBy (..)
  , PassResBy (..)
  , byAddressFFIType
  , passByFFIType
  ) where

import Data.Proxy (Proxy (Proxy))

import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass (CoercePass (coercePass), IsPass)
import HsBindgen.IR.Pass.Types (CoercePassTypes (coercePassTypes),
                                PassTypes (Types))

-- | How a function argument or a non-@void@ result crosses the Haskell FFI
--
-- Union, struct, and complex values cannot be handled by the Haskell FFI
-- directly; they are passed by address. An argument is passed as a pointer to
-- it; a result becomes an additional, last argument pointing to where the C
-- wrapper stores it.
--
-- See also 'HsBindgen.Backend.Hs.Translation.Function.functionDecs'.
data PassBy p =
    PassByValue Hs.FFIType
    -- | Type of the pointer to the value, as it appears in the C wrapper and in
    -- the foreign import of the C wrapper
  | PassByAddress (Types p)

deriving stock instance IsPass p => Show (PassBy p)
deriving stock instance IsPass p => Eq (PassBy p)

instance (
      CoercePassTypes p p'
    ) => CoercePass PassBy p p' where
  coercePass = \case
      PassByValue   t  -> PassByValue t
      PassByAddress ty ->
        PassByAddress $ coercePassTypes (Proxy @'(p, p')) ty

-- | How a function result is passed from C to Haskell
data PassResBy p =
    PassResVoid
  | PassResNonVoid (PassBy p)

deriving stock instance IsPass p => Show (PassResBy p)
deriving stock instance IsPass p => Eq (PassResBy p)

instance (
      CoercePassTypes p p'
    ) => CoercePass PassResBy p p' where
  coercePass = \case
      PassResVoid      -> PassResVoid
      PassResNonVoid x -> PassResNonVoid (coercePass x)

-- | FFI type of a value passed by address
byAddressFFIType :: Hs.FFIType
byAddressFFIType = Hs.FFIPtrVoid

-- | FFI type of a value in the foreign import of the C wrapper
passByFFIType :: PassBy p -> Hs.FFIType
passByFFIType = \case
    PassByValue   t -> t
    PassByAddress _ -> byAddressFFIType
