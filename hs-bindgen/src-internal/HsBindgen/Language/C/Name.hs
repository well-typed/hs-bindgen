module HsBindgen.Language.C.Name (
    -- * Name and namespaces
    CName(..)
  , Namespace(..)
  ) where

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Names and namespaces
-------------------------------------------------------------------------------}

newtype CName = CName {
      getCName :: Text
    }
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)
  deriving stock (Generic)

data Namespace =
    NamespaceTypedef
  | NamespaceStruct
  | NamespaceUnion
  | NamespaceEnum
  | NamespaceMacro
  | NamespaceFunction
  deriving stock (Show, Eq, Ord)

