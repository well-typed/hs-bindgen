module HsBindgen.Frontend.OmittedDecls (
    -- * Type
    OmittedDecls(..)
    -- * API
  , empty
  , insert
  , lookup
  ) where

import Data.Map.Strict qualified as Map
import Prelude hiding (lookup)

import Clang.Paths (SourcePath)
import HsBindgen.Imports
import HsBindgen.Language.C.Name (CName, Namespace)

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

newtype OmittedDecls = OmittedDecls {
      unOmittedDecls :: Map (CName, Namespace) SourcePath
    }
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

empty :: OmittedDecls
empty = OmittedDecls Map.empty

insert :: (CName, Namespace) -> SourcePath -> OmittedDecls -> OmittedDecls
insert k v = OmittedDecls . Map.insert k v . unOmittedDecls

lookup :: (CName, Namespace) -> OmittedDecls -> Maybe SourcePath
lookup k = Map.lookup k . unOmittedDecls
