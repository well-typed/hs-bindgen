-- | Class for C arrays (of known or unknown size)
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgen.Runtime.Array.Class qualified as AC
module HsBindgen.Runtime.Array.Class (
    IsArray (..)
  ) where

import Data.Kind (Type)
import Foreign.Ptr (Ptr)

class IsArray a where
  type Elem a :: Type
  withElemPtr :: a -> (Ptr (Elem a) -> IO r) -> IO r
