-- | C header AST
--
-- We omit a lot of detail from the full AST, including only information that
-- is relevant for our purposes.
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.AST qualified as C
module HsBindgen.C.AST (
    -- * Top-level
    Header(..)
  , Decl(..)
    -- * Names
  , CName(..)
    -- * Types
  , Typ(..)
    -- ** Primitive types
  , PrimType(..)
  , PrimSign(..)
    -- ** Structs
  , Struct(..)
  , StructField(..)
    -- ** Enums
  , Enu(..)
  , EnumValue(..)
    -- ** Typedefs
  , Typedef(..)
    -- * Macros
  , Macro(..)
    -- ** Expressions
  , MExpr(..)
  , MTerm(..)
  , Literal(..)
    -- ** Attributes
  , Attribute(..)
    -- ** Classification
  , isIncludeGuard
    -- ** Unrecognized
  , ReparseError(..)
  , Token(..)
  , TokenSpelling(..)
    -- * Source locations
  , SourcePath(..)
  , SingleLoc(..)
  , MultiLoc(..)
  , Range(..)
  ) where

import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal(..))

import HsBindgen.C.AST.Literal
import HsBindgen.C.AST.Macro
import HsBindgen.C.AST.Name
import HsBindgen.C.AST.Type
import HsBindgen.C.Reparse.Infra (ReparseError(..))
import HsBindgen.Clang.Util.SourceLoc.Type
import HsBindgen.Clang.Util.Tokens

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | C header
data Header = Header {
      headerDecls :: [Decl]
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

-- | Top-level declaration
data Decl =
    DeclStruct Struct
  | DeclTypedef Typedef
  | DeclEnum Enu
  | DeclMacro (Either ReparseError Macro)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

