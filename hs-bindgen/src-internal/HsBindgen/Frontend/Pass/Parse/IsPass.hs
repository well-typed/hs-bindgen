module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
    -- * Macros
  , ParsedMacro(..)
  , ReparseInfo(..)
    -- * Fields
  , FieldOrigin(..)
  , ExplicitFieldOrigin(..)
  , ImplicitFieldOrigin(..)
    -- * IsAnon
  , IsAnon(..)
  ) where

import C.Expr.Syntax qualified as CExpr

import Clang.HighLevel.Types
import Clang.LowLevel.Core (CXType)

import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Naming (CScopedName)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Imports
import HsBindgen.Util.Tracer (WithCallStack)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Parse :: Pass
data Parse a

type family AnnParse (ix :: Symbol) :: Star where
  AnnParse "Struct"      = IsAnon
  AnnParse "StructField" = (ReparseInfo, FieldOrigin)
  AnnParse "Union"       = IsAnon
  AnnParse "UnionField"  = (ReparseInfo, FieldOrigin)
  AnnParse "Typedef"     = ReparseInfo
  AnnParse "Function"    = ReparseInfo
  AnnParse "Global"      = ReparseInfo
  AnnParse _             = NoAnn

instance IsPass Parse where
  type Id          Parse = PrelimDeclId
  type MacroBody   Parse = ParsedMacro
  type ExtBinding  Parse = Void
  type Ann ix      Parse = AnnParse ix
  type Msg         Parse = WithCallStack (WithLocationInfo ImmediateParseMsg)
  type CommentDecl Parse = ()

  idNameKind     _ = PrelimDeclId.nameKind
  idSourceName   _ = PrelimDeclId.sourceName
  idLocationInfo _ = prelimDeclIdLocationInfo

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

-- | Syntactic parse result from @c-expr-dsl@.
--
-- A type macro (e.g. @#define FOO int@) has 'CExpr.macroBody' equal to
-- @'CExpr.MTerm' ('CExpr.MType' …)@. An expression macro has any
-- other 'CExpr.macroBody'. Type conversion and expression typechecking
-- happen later in "HsBindgen.Frontend.Pass.TypecheckMacros".
newtype ParsedMacro = ParsedMacro {
      parsedMacro :: CExpr.Macro
    }

deriving stock instance Show ParsedMacro
deriving stock instance Eq   ParsedMacro

data ReparseInfo =
    -- | We need to reparse this declaration (to deal with macros)
    --
    -- We do not use this for macro declarations _themselves_ (see
    -- 'ParsedMacro').
    ReparseNeeded
      [Token TokenSpelling]
      -- ^ Original tokens of declaration without macro expansions
      (Set Text)
      -- ^ Names of expanded macros

    -- | This declaration does not use macros, so no need to reparse
  | ReparseNotNeeded
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Fields
-------------------------------------------------------------------------------}

-- | How a struct or union field came to be
data FieldOrigin =
    ExplicitParsed ExplicitFieldOrigin
  | ImplicitGenerated ImplicitFieldOrigin
  deriving stock (Show, Eq, Ord)

-- | An explicit struct or union field is parsed directly from a C header file
--
-- Named fields are always parsed directly from a C header file.
data ExplicitFieldOrigin = ExplicitFieldOrigin
  deriving stock (Show, Eq, Ord)

-- | An implicit struct or union field is generated for nested anonymous structs
-- and unions
--
-- We track this information because @libclang@ does not expose information
-- about implicit fields, so we have to derive the information ourselves using a
-- custom algorithm. See the "HsBindgen.Frontend.Pass.Parse.Decl.ImplicitFields"
-- module for the algorithm.
data ImplicitFieldOrigin = ImplicitFieldOrigin {
    -- | The type of the enclosing object
    enclosing :: CXType
    -- | The name of the first field of the anonymous object
    --
    -- The offset from the enclosing object to an anonymous struct or union is
    -- equal to the offset from the enclosing object to the first field of the
    -- anonymous struct or union. The first field can also be an indirect field
    -- if there are multiple levels of nested anonymous structs/unions.
  , field :: CScopedName
  }
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  IsAnon
-------------------------------------------------------------------------------}

-- | A struct or union is anonymous if it is untagged and if it is referenced by
-- a single unnamed field.
newtype IsAnon = IsAnon { isAnon :: Bool }
  deriving stock (Show, Eq, Ord)
