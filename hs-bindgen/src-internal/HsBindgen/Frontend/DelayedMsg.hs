-- | Delayed messages
--
-- A pass that has something to say about a declaration does not say it
-- immediately, but attaches a delayed message to the declaration; @Select@
-- emits it if the declaration is selected (and always if it is a bug).
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.DelayedMsg (
    DelayedMsg(..)
  ) where

import GHC.Generics (Generic)

import HsBindgen.Frontend.Pass.MangleNames.IsPass.Msg (DelayedMangleNamesMsg)
import HsBindgen.Frontend.Pass.Parse.Msg (DelayedParseMsg)
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass.Msg (DelayedPrepareReparseMsg)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass.Msg (DelayedReparseMacroExpansionsMsg)
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg (DelayedTranslateTypesMsg)
import HsBindgen.Util.Tracer

-- | A delayed message, by the pass that issued it
data DelayedMsg =
    DelayedParse                  DelayedParseMsg
  | DelayedPrepareReparse         DelayedPrepareReparseMsg
  | DelayedReparseMacroExpansions DelayedReparseMacroExpansionsMsg
  | DelayedMangleNames            DelayedMangleNamesMsg
  | DelayedTranslateTypes         DelayedTranslateTypesMsg
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedMsg where
  prettyForTrace = \case
    DelayedParse                  x -> prettyForTrace x
    DelayedPrepareReparse         x -> prettyForTrace x
    DelayedReparseMacroExpansions x -> prettyForTrace x
    DelayedMangleNames            x -> prettyForTrace x
    DelayedTranslateTypes         x -> prettyForTrace x

instance IsTrace Level DelayedMsg where
  getDefaultLogLevel = \case
    DelayedParse                  x -> getDefaultLogLevel x
    DelayedPrepareReparse         x -> getDefaultLogLevel x
    DelayedReparseMacroExpansions x -> getDefaultLogLevel x
    DelayedMangleNames            x -> getDefaultLogLevel x
    DelayedTranslateTypes         x -> getDefaultLogLevel x
  getSource  = const HsBindgen
  getTraceId = \case
    DelayedParse                  x -> getTraceId x
    DelayedPrepareReparse         x -> getTraceId x
    DelayedReparseMacroExpansions x -> getTraceId x
    DelayedMangleNames            x -> getTraceId x
    DelayedTranslateTypes         x -> getTraceId x
