module Test.Util.FunPtr (
    castFunPtrCoercible
  ) where

import Data.Coerce (Coercible, coerce)
import Foreign.Ptr (FunPtr, castFunPtr)

castFunPtrCoercible :: forall a b. Coercible a b => FunPtr a -> FunPtr b
castFunPtrCoercible = castFunPtr
  where
    _unused :: a -> b
    _unused = coerce
