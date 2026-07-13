{-# OPTIONS_HADDOCK hide #-}

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
  ) where

import GHC.Records.Compat (HasField (..), getField, setField)
