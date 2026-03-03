module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
    -- * Macros
  , UnparsedMacro(..)
  , ReparseInfo(..)
  ) where

import Clang.HighLevel.Types

import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Parse :: Pass
data Parse a

type family AnnParse (ix :: Symbol) :: Star where
  AnnParse "StructField" = ReparseInfo
  AnnParse "UnionField"  = ReparseInfo
  AnnParse "Typedef"     = ReparseInfo
  AnnParse "Function"    = ReparseInfo
  AnnParse _             = NoAnn

instance IsPass Parse where
  type Id         Parse = PrelimDeclId
  type MacroBody  Parse = UnparsedMacro
  type ExtBinding Parse = Void
  type Ann ix     Parse = AnnParse ix
  type Msg        Parse = WithLocationInfo ImmediateParseMsg

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
