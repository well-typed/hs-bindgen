-- | Class for C arrays (of known or unknown size)
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgen.Runtime.IsArray qualified as IsA
module HsBindgen.Runtime.IsArray (
    IsArray (..)
  ) where

import Data.Kind (Type)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)

class IsArray a where
  type Elem a :: Type
  withElemPtr :: Storable (Elem a) => a -> (Ptr (Elem a) -> IO r) -> IO r
