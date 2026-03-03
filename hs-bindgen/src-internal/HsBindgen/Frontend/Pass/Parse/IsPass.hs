module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
    -- * Macros
  , UnparsedMacro(..)
  , ReparseInfo(..)
    -- * Trace messages
  , RequiredForScoping(..)
  , ImmediateParseMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types

import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Imports
import HsBindgen.Util.Tracer

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

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}


-- | We always need to parse declarations required for scoping
data RequiredForScoping =
    RequiredForScoping
  | NotRequiredForScoping
  -- TODO-D: UnknownRequiredForScoping may not be required.
  | UnknownRequiredForScoping
  deriving stock (Show, Eq, Ord)

-- | Parse messages that we emit immediately
--
-- Reason for reporting immediately:
--
-- - The user may not care (they might not even select the declaration), but we
--   care (i.e., a bug or an unsupported feature)
--
-- - The declarations may affect other declarations
data ImmediateParseMsg =
    -- | We failed to parse a declaration that is required for scoping.
    ParseOfDeclarationRequiredForScopingFailed ParseTypeException
  deriving stock (Show)

instance PrettyForTrace ImmediateParseMsg where
  prettyForTrace = \case
      ParseOfDeclarationRequiredForScopingFailed err ->
        PP.hang "parse of declaration required for scoping failed:" 2 $
          prettyForTrace err

instance IsTrace Level ImmediateParseMsg where
  getDefaultLogLevel = \case
      ParseOfDeclarationRequiredForScopingFailed{} -> Info
  getSource  = const HsBindgen
  getTraceId = const "parse-immediate"

