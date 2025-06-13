module HsBindgen.Language.C.Name (
    -- * Name and namespaces
    CName(..)
  , Namespace(..)
  ) where

import Data.Text (unpack)

import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyTrace (prettyTrace))

{-------------------------------------------------------------------------------
  Names and namespaces
-------------------------------------------------------------------------------}

newtype CName = CName {
      getCName :: Text
    }
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)
  deriving stock (Generic)

instance PrettyTrace CName where
  prettyTrace (CName name) = unpack name

data Namespace =
    NamespaceTypedef
  | NamespaceStruct
  | NamespaceUnion
  | NamespaceEnum
  | NamespaceMacro
  | NamespaceFunction
  deriving stock (Show, Eq, Ord, Generic)

instance PrettyTrace Namespace where
  prettyTrace = \case
    NamespaceTypedef  -> "typedef"
    NamespaceStruct   -> "struct"
    NamespaceUnion    -> "union"
    NamespaceEnum     -> "enum"
    -- I made these two up.
    NamespaceMacro    -> "macro"
    NamespaceFunction -> "function"
