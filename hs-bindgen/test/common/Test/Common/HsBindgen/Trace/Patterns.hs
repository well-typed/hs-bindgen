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
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Parse.Msg (ParseImplicitFieldsMsg)
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer (WithCallStack (..))

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

pattern MatchImmediate :: CDeclName -> ImmediateParseMsg -> TraceMsg
pattern MatchImmediate name x <- TraceFrontend (
      FrontendParse WithCallStack{traceMsg = WithLocationInfo{
          loc = locationInfoName -> Just name
        , msg = x
        }}
    )

pattern MatchDelayed :: CDeclName -> DelayedParseMsg -> TraceMsg
pattern MatchDelayed name x <- MatchSelect name (matchDelayed -> Just x)

pattern MatchDelayedImplicitField :: CDeclName -> ParseImplicitFieldsMsg -> TraceMsg
pattern MatchDelayedImplicitField name x <- MatchDelayed name (ParseImplicitFieldFailed x)

{-------------------------------------------------------------------------------
  ResolveBindingSpecs
-------------------------------------------------------------------------------}

pattern MatchBindingSpec :: BindingSpecMsg -> TraceMsg
pattern MatchBindingSpec x <- TraceBoot (
      BootBindingSpec x
    )

pattern MatchResolveBindingSpecs :: ResolveBindingSpecsMsg -> TraceMsg
pattern MatchResolveBindingSpecs x <- TraceFrontend (
      FrontendResolveBindingSpecs WithCallStack{traceMsg = x}
    )

{-------------------------------------------------------------------------------
  Select
-------------------------------------------------------------------------------}

pattern MatchNoDeclarations :: TraceMsg
pattern MatchNoDeclarations <- TraceFrontend (
      FrontendSelect WithCallStack{traceMsg = WithLocationInfo{
          msg = SelectNoDeclarationsMatched
        }}
    )

pattern MatchSelect :: CDeclName -> SelectMsg -> TraceMsg
pattern MatchSelect name x <- TraceFrontend (
      FrontendSelect WithCallStack{traceMsg = WithLocationInfo{
          loc = locationInfoName -> Just name
        , msg = x
        }}
    )

-- | Transitive dependencies of a declaration are missing
pattern MatchTransMissing :: [TransitiveDependencyMissing] -> SelectMsg
pattern MatchTransMissing xs <- TransitiveDependenciesMissing _ xs

-- | A single transitive dependency of a declaration was not selected
pattern MatchTransNotSelected :: TransitiveDependencyMissing
pattern MatchTransNotSelected <- TransitiveDependencyNotSelected _ _

-- | A single transitive dependency of a declaration is unusable
pattern MatchTransUnusable :: Unusable -> TransitiveDependencyMissing
pattern MatchTransUnusable x <- TransitiveDependencyUnusable _ x _

{-------------------------------------------------------------------------------
  MangleNames
-------------------------------------------------------------------------------}

pattern MatchMangle :: CDeclName -> MangleNamesMsg -> TraceMsg
pattern MatchMangle name x <- TraceFrontend (
      FrontendMangleNames WithCallStack{traceMsg = WithLocationInfo{
           loc = locationInfoName -> Just name
         , msg = x
        }}
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
