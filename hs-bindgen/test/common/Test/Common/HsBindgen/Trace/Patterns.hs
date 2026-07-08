-- | Pattern synonyms for working with traces
module Test.Common.HsBindgen.Trace.Patterns (
    -- * Clang
    pattern MatchDiagnosticOption
  , pattern MatchDiagnosticCategory
  , matchDiagnosticSpelling
    -- * Parse
  , pattern MatchImmediate
  , pattern MatchDelayed
  , pattern MatchDelayedImplicitField
    -- * PrepareReparse
  , pattern MatchImmediatePrepareReparse
  , pattern MatchDelayedPrepareReparse
    -- * ReparseMacroExpansions
  , pattern MatchImmediateReparseMacroExpansions
  , pattern MatchDelayedReparseMacroExpansions
    -- * ResolveBindingSpecs
  , pattern MatchBindingSpec
  , pattern MatchResolveBindingSpecs
    -- * Select
  , pattern MatchNoDeclarations
  , pattern MatchSelect
  , pattern MatchTransMissing
  , pattern MatchTransNotSelected
  , pattern MatchTransUnusable
    -- * MangleNames
  , pattern MatchMangle
    -- * Doxygen
  , pattern MatchDoxygen
  ) where

import Data.Text qualified as Text

import HsBindgen.Doxygen (DoxygenMsg)
import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Pass.Parse.Msg (ParseImplicitFieldsMsg)
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass.Msg (DelayedPrepareReparseMsg,
                                                          PrepareReparseMsg)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (ReparseMacroExpansions)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass.Msg (DelayedReparseMacroExpansionsMsg)
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass.Msg (PassMsg (Msg))
import HsBindgen.TraceMsg

{-------------------------------------------------------------------------------
  Clang
-------------------------------------------------------------------------------}

pattern MatchDiagnosticOption :: Text -> TraceMsg
pattern MatchDiagnosticOption x <- TraceFrontend (
      FrontendClang (
          ClangDiagnostic Diagnostic{diagnosticOption = Just x}
        )
    )

pattern MatchDiagnosticCategory :: Text -> TraceMsg
pattern MatchDiagnosticCategory x <- TraceFrontend (
      FrontendClang (
          ClangDiagnostic (diagnosticCategoryText -> x)
        )
    )

matchDiagnosticSpelling :: Text -> TraceMsg -> Maybe Text
matchDiagnosticSpelling text = \case
    TraceFrontend (
        FrontendClang (
            ClangDiagnostic diag
          )
      ) ->
      if text `Text.isInfixOf` diag.diagnosticSpelling
        then Just diag.diagnosticSpelling
        else Nothing
    _otherwise ->
      Nothing

{-------------------------------------------------------------------------------
  Parse
-------------------------------------------------------------------------------}

pattern MatchImmediate :: C.DeclName -> ImmediateParseMsg -> TraceMsg
pattern MatchImmediate name x <- TraceFrontend (
      FrontendParse C.WithLocationInfo{
          loc = C.locationInfoName -> Just name
        , msg = x
        }
    )

pattern MatchDelayed :: C.DeclName -> DelayedParseMsg -> TraceMsg
pattern MatchDelayed name x <- MatchSelect name (matchDelayed -> Just x)

pattern MatchDelayedImplicitField ::
     C.DeclName
  -> ParseImplicitFieldsMsg
  -> TraceMsg
pattern MatchDelayedImplicitField name x <- MatchDelayed name (ParseImplicitFieldFailed x)

{-------------------------------------------------------------------------------
  PrepareReparse
-------------------------------------------------------------------------------}

pattern MatchImmediatePrepareReparse :: PrepareReparseMsg -> TraceMsg
pattern MatchImmediatePrepareReparse x <- TraceFrontend (
      FrontendPrepareReparse x
    )

pattern MatchDelayedPrepareReparse ::
     C.DeclName
  -> DelayedPrepareReparseMsg
  -> TraceMsg
pattern MatchDelayedPrepareReparse name x <- MatchSelect name (matchDelayedPrepareReparse -> Just x)

matchDelayedPrepareReparse :: SelectMsg -> Maybe DelayedPrepareReparseMsg
matchDelayedPrepareReparse = \case
    SelectDelayedPrepareReparseMsg x -> Just x
    _otherwise -> Nothing

{-------------------------------------------------------------------------------
  ReparseMacroExpansions
-------------------------------------------------------------------------------}

pattern MatchImmediateReparseMacroExpansions :: Msg ReparseMacroExpansions -> TraceMsg
pattern MatchImmediateReparseMacroExpansions x <- TraceFrontend (
      FrontendReparseMacroExpansions x
    )

pattern MatchDelayedReparseMacroExpansions ::
     C.DeclName
  -> DelayedReparseMacroExpansionsMsg
  -> TraceMsg
pattern MatchDelayedReparseMacroExpansions name x <- MatchSelect name (matchDelayedReparseMacroExpansions -> Just x)

matchDelayedReparseMacroExpansions :: SelectMsg -> Maybe DelayedReparseMacroExpansionsMsg
matchDelayedReparseMacroExpansions = \case
    SelectDelayedReparseMacroExpansionsMsg x -> Just x
    _otherwise -> Nothing

{-------------------------------------------------------------------------------
  ResolveBindingSpecs
-------------------------------------------------------------------------------}

pattern MatchBindingSpec :: BindingSpecMsg -> TraceMsg
pattern MatchBindingSpec x <- TraceBoot (
      BootBindingSpec x
    )

pattern MatchResolveBindingSpecs :: ResolveBindingSpecsMsg -> TraceMsg
pattern MatchResolveBindingSpecs x <- TraceFrontend (
      FrontendResolveBindingSpecs x
    )

{-------------------------------------------------------------------------------
  Select
-------------------------------------------------------------------------------}

pattern MatchNoDeclarations :: TraceMsg
pattern MatchNoDeclarations <- TraceFrontend (
      FrontendSelect C.WithLocationInfo{
          msg = SelectNoDeclarationsMatched
        }
    )

pattern MatchSelect :: C.DeclName -> SelectMsg -> TraceMsg
pattern MatchSelect name x <- TraceFrontend (
      FrontendSelect C.WithLocationInfo{
          loc = C.locationInfoName -> Just name
        , msg = x
        }
    )

-- | Transitive dependencies of a declaration are missing
pattern MatchTransMissing :: [TransitiveDependencyMissing] -> SelectMsg
pattern MatchTransMissing xs <- TransitiveDependenciesMissing _ xs

-- | A single transitive dependency of a declaration was not selected
pattern MatchTransNotSelected :: TransitiveDependencyMissing
pattern MatchTransNotSelected <- TransitiveDependencyNotSelected _ _

-- | A single transitive dependency of a declaration is unusable
pattern MatchTransUnusable :: Unusable -> TransitiveDependencyMissing
pattern MatchTransUnusable x <- TransitiveDependencyUnusable _ x

{-------------------------------------------------------------------------------
  MangleNames
-------------------------------------------------------------------------------}

pattern MatchMangle :: C.DeclName -> MangleNamesMsg -> TraceMsg
pattern MatchMangle name x <- TraceFrontend (
      FrontendMangleNames C.WithLocationInfo{
           loc = C.locationInfoName -> Just name
         , msg = x
        }
    )

{-------------------------------------------------------------------------------
  Doxygen
-------------------------------------------------------------------------------}

pattern MatchDoxygen :: DoxygenMsg -> TraceMsg
pattern MatchDoxygen x <- TraceFrontend (FrontendDoxygen x)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

matchDelayed :: SelectMsg -> Maybe DelayedParseMsg
matchDelayed = \case
    SelectDelayedParseMsg x -> Just x
    SelectParseFailure x -> Just x
    _otherwise -> Nothing
