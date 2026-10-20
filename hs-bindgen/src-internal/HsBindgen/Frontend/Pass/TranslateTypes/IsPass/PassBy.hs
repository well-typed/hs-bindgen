module HsBindgen.Frontend.Pass.TranslateTypes.IsPass.PassBy (
    PassResBy (..)
  , PassArgBy (..)
  ) where

import Data.Proxy (Proxy (Proxy))

import HsBindgen.IR.Pass (CoercePass (coercePass),
                          CoercePassTypeFunArg (coercePassTypeFunArg), IsPass,
                          PassTypes (TypeFunArg))
import HsBindgen.IR.Pass.Types (CoercePassTypes (coercePassTypes),
                                PassTypes (Types))

-- | Classification of the type of a function result: it is either passed by
-- value or by address.
--
-- See also 'HsBindgen.Backend.Hs.Translation.Function.functionDecs'.
data PassResBy p = PassResBy {
    -- | Types as they appear in the C wrapper and foreign import of the C wrapper
    --
    -- If a result is passed by value, then these types will match the
    -- /original/ types of the function result. If a result is passed by
    -- address, then these types types will be pointers to the /original/ types
    -- of the function result.
    wrapperType :: Types p
    -- | Whether the type is passed by address. If not, it is passed by value.
    --
    -- Types passed by value: ordinary, "primitive" types which can be handled
    -- by Haskell FFI directly.
    --
    -- Types passed by address: union, struct, and complex, which can not be
    -- handled by the Haskell FFI directly.
  , byAddress :: Bool
  }

deriving stock instance IsPass p => Show (PassResBy p)
deriving stock instance IsPass p => Eq (PassResBy p)

instance (
      CoercePassTypes p p'
    ) => CoercePass PassResBy p p' where
  coercePass x = PassResBy {
        wrapperType = coercePassTypes (Proxy @'(p, p')) x.wrapperType
      , byAddress = x.byAddress
      }

-- | Classification of the type of a function argument: it is either passed by
-- value or by address.
--
-- See also 'HsBindgen.Backend.Hs.Translation.Function.functionDecs'.
data PassArgBy p = PassArgBy {
    -- | Types as they appear in the C wrapper and foreign import of the C wrapper
    --
    -- If an argument is passed by value, then these types will match the
    -- /original/ types of the function argument. If an argument is passed by
    -- address, then these types types will be pointers to the /original/ types
    -- of the function argument.
    wrapperType :: TypeFunArg p
    -- | Whether the type is passed by address. If not, it is passed by value.
    --
    -- Types passed by value: ordinary, "primitive" types which can be handled
    -- by Haskell FFI directly.
    --
    -- Types passed by address: union, struct, and complex, which can not be
    -- handled by the Haskell FFI directly.
  , byAddress :: Bool
  }

deriving stock instance IsPass p => Show (PassArgBy p)
deriving stock instance IsPass p => Eq (PassArgBy p)

instance (
      CoercePassTypeFunArg p p'
    ) => CoercePass PassArgBy p p' where
  coercePass x = PassArgBy {
        wrapperType = coercePassTypeFunArg (Proxy @'(p, p')) x.wrapperType
      , byAddress = x.byAddress
      }
