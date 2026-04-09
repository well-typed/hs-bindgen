module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
    -- * Macros
  , UnparsedMacro(..)
  , ReparseInfo(..)
    -- * Fields
  , FieldOrigin(..)
  , ExplicitFieldOrigin(..)
  , ImplicitFieldOrigin(..)
    -- * IsAnon
  , IsAnon(..)
  ) where

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
  AnnParse _             = NoAnn

instance IsPass Parse where
  type Id         Parse = PrelimDeclId
  type MacroBody  Parse = UnparsedMacro
  type ExtBinding Parse = Void
  type Ann ix     Parse = AnnParse ix
  type Msg        Parse = WithCallStack (WithLocationInfo ImmediateParseMsg)

  idNameKind     _ = PrelimDeclId.nameKind
  idSourceName   _ = PrelimDeclId.sourceName
  idLocationInfo _ = prelimDeclIdLocationInfo

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

data UnparsedMacro = UnparsedMacro {
      tokens :: [Token TokenSpelling]
    }
  deriving stock (Show, Eq, Ord)

data ReparseInfo =
    -- | We need to reparse this declaration (to deal with macros)
    --
    -- NOTE: We do not use this for macros /themselves/ (see 'UnparsedMacro').
    ReparseNeeded [Token TokenSpelling]

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
