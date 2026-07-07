-- | Trace messages for the @ReparseMacroExpansions@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass.Msg (
    -- * DelayedMsg
    DelayedReparseMacroExpansionsMsg (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.LanguageC.Error qualified as LanC
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Zip.Error
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  DelayedMsg
-------------------------------------------------------------------------------}

data DelayedReparseMacroExpansionsMsg =
    -- | We could not reparse a fragment of C (to recover macro use sites)
    ReparseMacroExpansionsLanC LanC.Error

    -- | We could not zip the C AST before reparsing with the C AST after
    -- reparsing
  | ReparseMacroExpansionsZip ZipError

    -- | While reparsing a declaration with a macro expansion, we do not know
    --   the type of an expanded macro.
  | ReparseMacroExpansionUnknownType Text
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedReparseMacroExpansionsMsg where
  prettyForTrace = \case
      ReparseMacroExpansionsLanC x -> PP.hsep [
          "Failed to parse tokens using language-c:"
        , prettyForTrace x
        ]
      ReparseMacroExpansionsZip err -> PP.hsep [
          "Failed to zip the C AST before reparsing with"
        , "the C AST after reparsing:"
        , prettyForTrace err
        ]
      ReparseMacroExpansionUnknownType x -> PP.hsep [
          "Unknown type of expanded macro"
        , PP.text x
        ]

instance IsTrace Level DelayedReparseMacroExpansionsMsg where
  getDefaultLogLevel = \case
      ReparseMacroExpansionsLanC x       -> getDefaultLogLevel x
      ReparseMacroExpansionsZip x        -> getDefaultLogLevel x
      ReparseMacroExpansionUnknownType{} -> Info
  getSource          = const HsBindgen
  getTraceId         = const "reparse-macro-expansions"
