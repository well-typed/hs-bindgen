module HsBindgen.Frontend.Pass.TranslateTypes.IsPass.PassingMethod (
    PassingMethod (..)
  , ResPassingMethod (..)
  , isSerialized
  , isResSerialized
  , serializedFFIType
  , passingMethodFFIType
  ) where

import Data.Proxy (Proxy (Proxy))

import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass (CoercePass (coercePass), IsPass)
import HsBindgen.IR.Pass.Types (CoercePassTypes (coercePassTypes),
                                PassTypes (Types))

-- | How a function argument or a non-@void@ result crosses the Haskell FFI
--
-- The Haskell FFI passes values of its built-in types directly. It cannot pass
-- union, struct, and complex values; we serialise them to or from a buffer and
-- pass them by address. An argument is passed as a pointer to it; a result
-- becomes an additional, last argument pointing to where the C wrapper stores
-- it.
--
-- See also 'HsBindgen.Backend.Hs.Translation.Function.functionDecs'.
data PassingMethod p =
    PassDirect Hs.FFIType
    -- | Type of the pointer to the value, as it appears in the C wrapper and in
    -- the foreign import of the C wrapper
  | PassSerialized (Types p)

deriving stock instance IsPass p => Show (PassingMethod p)
deriving stock instance IsPass p => Eq (PassingMethod p)

instance (
      CoercePassTypes p p'
    ) => CoercePass PassingMethod p p' where
  coercePass = \case
      PassDirect     t  -> PassDirect t
      PassSerialized ty ->
        PassSerialized $ coercePassTypes (Proxy @'(p, p')) ty

-- | How a function result is passed from C to Haskell
data ResPassingMethod p =
    PassResVoid
  | PassResNonVoid (PassingMethod p)

deriving stock instance IsPass p => Show (ResPassingMethod p)
deriving stock instance IsPass p => Eq (ResPassingMethod p)

instance (
      CoercePassTypes p p'
    ) => CoercePass ResPassingMethod p p' where
  coercePass = \case
      PassResVoid      -> PassResVoid
      PassResNonVoid x -> PassResNonVoid (coercePass x)

-- | Is the value serialised?
isSerialized :: PassingMethod p -> Bool
isSerialized = \case
    PassDirect{}     -> False
    PassSerialized{} -> True

-- | Is the result serialised?
isResSerialized :: ResPassingMethod p -> Bool
isResSerialized = \case
    PassResVoid      -> False
    PassResNonVoid x -> isSerialized x

-- | FFI type of a serialised value
serializedFFIType :: Hs.FFIType
serializedFFIType = Hs.FFIPtrVoid

-- | FFI type of a value in the foreign import of the C wrapper
passingMethodFFIType :: PassingMethod p -> Hs.FFIType
passingMethodFFIType = \case
    PassDirect     t -> t
    PassSerialized _ -> serializedFFIType
