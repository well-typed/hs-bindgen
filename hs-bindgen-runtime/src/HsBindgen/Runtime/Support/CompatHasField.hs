{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Prelude required by generated bindings.
--
-- This is a sub-mobule of "HsBindgen.Runtime.Support" that only exports
-- definitions from the @record-hasfield@ package. We do this because the
-- 'HasField' name is not unique. The name is used for two separate classes the
-- "GHC.Records" and "GHC.Records.Compat" modules, and we can't export the same
-- name from the "HsBindgen.Runtime.Support" twice.
module HsBindgen.Runtime.Support.CompatHasField (
    -- * 'HasField'
    HasField(hasField)
  , getField
  , setField
  , modifyField
  ) where

import GHC.Records.Compat (HasField (..), getField, setField)

-- | Modify a field in a record.
modifyField :: forall x r a. (HasField x r a) => r -> (a -> a) -> r
modifyField r f = gen $ f val
  where
    (gen, val) = hasField @x r
