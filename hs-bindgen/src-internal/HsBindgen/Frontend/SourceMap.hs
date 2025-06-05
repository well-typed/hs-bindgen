module HsBindgen.Frontend.SourceMap (
    -- * Type
    SourceMap(..)
    -- * API
  , empty
  , insert
  ) where

import Data.Map.Strict qualified as Map

import Clang.Paths (SourcePath)
import HsBindgen.Frontend.AST.Internal (QualId)
import HsBindgen.Frontend.Pass (Id)
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

newtype SourceMap p = SourceMap {
      unSourceMap :: Map (QualId p) SourcePath
    }

deriving instance Show (Id p) => Show (SourceMap p)
deriving instance Eq   (Id p) => Eq   (SourceMap p)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

empty :: SourceMap p
empty = SourceMap Map.empty

insert :: Ord (Id p) => QualId p -> SourcePath -> SourceMap p -> SourceMap p
insert k v = SourceMap . Map.insert k v . unSourceMap
