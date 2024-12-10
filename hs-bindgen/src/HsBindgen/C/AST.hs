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
  , Type(..)
    -- ** Primitive types
  , PrimType(..)
  , PrimIntType(..)
  , PrimFloatType(..)
  , PrimSign(..)
    -- ** Structs
  , Struct(..)
  , StructField(..)
  , OpaqueStruct(..)
    -- ** Enums
  , Enu(..)
  , EnumValue(..)
  , OpaqueEnum(..)
    -- ** Typedefs
  , Typedef(..)
    -- ** Functions
  , Function(..)
    -- * Macros
  , Macro(..)
  , MacroDecl(..)
    -- ** Expressions
  , MExpr(..)
  , MFun(..)
  , MTerm(..)
  , IntegerLiteral(..)
  , FloatingLiteral(..)
    -- ** Attributes
  , Attribute(..)
    -- ** Classification
  , isIncludeGuard
    -- ** Unrecognized
  , ReparseError(..)
  , Token(..)
  , TokenSpelling(..)
    -- * Macro type
  , QuantTy
  , TcMacroError(..)
  , pprTcMacroError
    -- * DeclPath
  , DeclPath(..)
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
import HsBindgen.C.Tc.Macro
  ( QuantTy, TcMacroError(..), pprTcMacroError )
import HsBindgen.Clang.HighLevel.Types

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
  | DeclOpaqueStruct OpaqueStruct
  | DeclTypedef Typedef
  | DeclEnum Enu
  | DeclOpaqueEnum OpaqueEnum
  | DeclMacro MacroDecl
  | DeclFunction Function
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data MacroDecl
  = MacroReparseError ReparseError
  | MacroTcError { macroTcErrorMacro :: Macro, macroTcError :: TcMacroError }
  | MacroDecl {
        macroDeclMacro     :: Macro
      , macroDeclMacroTy   :: QuantTy
      , macroDeclSourceLoc :: SingleLoc
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data Function = Function
    { functionName :: CName
    , functionType :: Type

    -- TODO: we might not need functionHeader field,
    -- https://github.com/well-typed/hs-bindgen/issues/333
    , functionHeader :: FilePath
    , functionSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)
