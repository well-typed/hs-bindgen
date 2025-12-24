module HsBindgen.Frontend.Pass.Parse.IsPass (
    Parse
    -- * Macros
  , UnparsedMacro(..)
  , ReparseInfo(..)
  , getUnparsedMacro
    -- * Trace messages
  , RequiredForScoping(..)
  , ImmediateParseMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Msg
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
  type Id         Parse = C.PrelimDeclId
  type MacroBody  Parse = UnparsedMacro
  type ExtBinding Parse = Void
  type Ann ix     Parse = AnnParse ix
  type Msg        Parse = WithLocationInfo ImmediateParseMsg

  idNameKind     _ = C.prelimDeclIdNameKind
  idSourceName   _ = C.prelimDeclIdSourceName
  idLocationInfo _ = prelimDeclIdLocationInfo

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

newtype UnparsedMacro = UnparsedMacro {
      unparsedTokens :: [Token TokenSpelling]
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

getUnparsedMacro ::
     (MonadIO m, HasCallStack)
  => CXTranslationUnit -> CXCursor -> m UnparsedMacro
getUnparsedMacro unit curr = do
    range  <- HighLevel.clang_getCursorExtent curr
    tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
    return $ UnparsedMacro tokens

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- | We always need to parse declarations required for scoping
data RequiredForScoping = RequiredForScoping | NotRequiredForScoping
  deriving stock (Show, Eq)

-- | Parse messages that we emit immediately
--
-- For example, if we can not attach messages to declarations, we emit them
-- directly while parsing.
data ImmediateParseMsg =
    -- | Declaration availability can not be determined.
    --
    -- That is 'Clang.LowLevel.Core.clang_getCursorAvailability' does not
    -- provide a valid 'Clang.LowLevel.Core.CXAvailabilityKind'.
    ParseUnknownCursorAvailability (SimpleEnum CXAvailabilityKind)
    -- | We failed to parse a declaration that is required for scoping.
  | ParseOfDeclarationRequiredForScopingFailed ParseTypeException
  deriving stock (Show)

instance PrettyForTrace ImmediateParseMsg where
  prettyForTrace = \case
      ParseUnknownCursorAvailability simpleKind -> PP.hsep [
          "unknown declaration availability:"
        , PP.showToCtxDoc simpleKind
        ]
      ParseOfDeclarationRequiredForScopingFailed err ->
        PP.hang "parse of declaration required for scoping failed:" 2 $
          prettyForTrace err

instance IsTrace Level ImmediateParseMsg where
  getDefaultLogLevel = \case
      ParseUnknownCursorAvailability{}             -> Notice
      ParseOfDeclarationRequiredForScopingFailed{} -> Info
  getSource  = const HsBindgen
  getTraceId = const "parse-immediate"

