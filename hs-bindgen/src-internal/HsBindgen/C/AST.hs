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
  , TypeName(..)
  , typeNameType
  , Size(..)
    -- ** Primitive types
  , PrimType(..)
  , PrimIntType(..)
  , PrimFloatType(..)
  , PrimSign(..)
  , PrimSignChar(..)
    -- ** Structs
  , Struct(..)
  , StructField(..)
  , OpaqueStruct(..)
    -- ** Unions
  , Union(..)
  , UnionField(..)
    -- ** Enums
  , Enu(..)
  , EnumValue(..)
  , OpaqueEnum(..)
    -- ** Typedefs
  , Typedef(..)
    -- ** Functions
  , Function(..)
    -- * Declarations
  , DeclaratorType(..)
  , Declarator(..), DirectDeclarator(..)
  , DeclName(..)
  , DeclarationSpecifier(..)
  , ArrayDeclarator(..), FunctionDeclarator(..)
  , ArraySize(..)
  , Pointers(..)
  , TypeSpecifier(..)
  , TypeQualifier(..)
  , TypeSpecifierQualifier(..)
  , AttributeSpecifier(..)
  , Attribute(..)
  , AttributeToken(..)
  , BalancedToken(..)
  , AlignmentSpecifier(..)
  , StorageClassSpecifier(..)
  , FunctionSpecifier(..)
  , StructOrUnionSpecifier(..), StructOrUnion(..)
  , EnumSpecifier(..)
  , SizeExpression(..), litSizeExpression
  , Parameter(..)
  , ParameterDeclarator(..)
    -- * Macros
  , Macro(..)
  , MacroDecl(..)
  , MacroBody(..)
  , Pass(..)
  , XVar(..), XApp(..)
    -- ** Expressions
  , MExpr(..)
  , MFun(..)
  , MTerm(..)
  , IntegerLiteral(..)
  , FloatingLiteral(..)
  , CharLiteral(..)
  , StringLiteral(..)
  , fromBytes
    -- ** Classification
  , isIncludeGuard
    -- ** Unrecognized
  , ReparseError(..)
  , Token(..)
  , TokenSpelling(..)
    -- * Macro type
  , Macro.Quant(..)
  , TcMacroError(..)
  , pprTcMacroError
    -- * DeclPath
  , DeclPath(..)
  , DeclPathCtxt(..)
  , declPathName
    -- * Source locations
  , SingleLoc(..)
  , MultiLoc(..)
  , Range(..)
  ) where

import GHC.Generics (Generic)

import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.C.AST.Literal
import HsBindgen.C.AST.Macro
import HsBindgen.C.AST.Name
import HsBindgen.C.AST.Type
import HsBindgen.C.Reparse.Decl
import HsBindgen.C.Reparse.Infra (ReparseError(..))
import HsBindgen.C.Tc.Macro
  ( TcMacroError(..), pprTcMacroError )
import HsBindgen.C.Tc.Macro qualified as Macro


{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | C header
data Header = Header {
      headerDecls :: [Decl]
    }
  deriving stock (Show, Eq, Generic)

-- | Top-level declaration
data Decl =
    DeclStruct Struct
  | DeclOpaqueStruct OpaqueStruct
  | DeclUnion Union
  | DeclTypedef Typedef
  | DeclEnum Enu
  | DeclOpaqueEnum OpaqueEnum
  | DeclMacro MacroDecl
  | DeclFunction Function
  deriving stock (Show, Eq, Generic)

data MacroDecl
  = MacroReparseError {
        macroReparseError          :: ReparseError
      , macroReparseErrorSourceLoc :: SingleLoc
      }
  | MacroTcError {
        macroTcErrorMacro     :: Macro Ps
      , macroTcError          :: Macro.TcMacroError
      , macroTcErrorSourceLoc :: SingleLoc
      }
  | MacroDecl {
        macroDeclMacro     :: Macro Ps
      , macroDeclMacroTy   :: Macro.Quant ( Macro.Type Macro.Ty )
      , macroDeclSourceLoc :: SingleLoc
      }
  deriving stock (Show, Eq, Generic)

data Function = Function
    { functionName      :: CName
    , functionArgs      :: [Type]
    , functionRes       :: Type
    , functionHeader    :: CHeaderIncludePath
    , functionSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)
