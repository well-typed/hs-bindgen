-- | Pattern synonyms for working with traces
module Test.Common.HsBindgen.Trace.Patterns (
    -- * Clang
    pattern MatchDiagnosticOption
  , pattern MatchDiagnosticCategory
  , matchDiagnosticSpelling
    -- * Parse
  , pattern MatchParse
  , pattern MatchDelayed
  , pattern MatchUnknownStorageClass
    -- * ResolveBindingSpecs
  , pattern MatchBindingSpec
  , pattern MatchResolveBindingSpecs
    -- * Select
  , pattern MatchNoDeclarations
  , pattern MatchSelect
  , pattern MatchTransMissing
    -- * MangleNames
  , pattern MatchMangle
  ) where

import Data.Text qualified as Text

import Clang.Enum.Simple
import Clang.LowLevel.Core

import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
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

pattern MatchParse :: C.DeclName -> ImmediateParseMsg -> TraceMsg
pattern MatchParse name x <- TraceFrontend (
      FrontendParse WithLocationInfo{
          loc = locationInfoName -> Just name
        , msg = x
        }
    )

pattern MatchDelayed :: C.DeclName -> DelayedParseMsg -> TraceMsg
pattern MatchDelayed name x <- MatchSelect name (matchDelayed -> Just x)

pattern MatchUnknownStorageClass :: CX_StorageClass -> DelayedParseMsg
pattern MatchUnknownStorageClass x <- ParseUnknownStorageClass (
      fromSimpleEnum -> Right x
    )

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
      FrontendSelect WithLocationInfo{
          msg = SelectNoDeclarationsMatched
        }
    )

pattern MatchSelect :: C.DeclName -> SelectMsg -> TraceMsg
pattern MatchSelect name x <- TraceFrontend (
      FrontendSelect WithLocationInfo{
          loc = locationInfoName -> Just name
        , msg = x
        }
    )

-- | Transitive dependency of a declaration is missing
--
-- The transitive dependency is unusable or simply not selected ('Nothing').
pattern MatchTransMissing :: Maybe Unusable -> SelectMsg
pattern MatchTransMissing x <- (matchTransMissing -> Just x)

{-------------------------------------------------------------------------------
  MangleNames
-------------------------------------------------------------------------------}

pattern MatchMangle :: C.DeclName -> MangleNamesMsg -> TraceMsg
pattern MatchMangle name x <- TraceFrontend (
      FrontendMangleNames WithLocationInfo{
           loc = locationInfoName -> Just name
         , msg = x
        }
    )

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

matchDelayed :: SelectMsg -> Maybe DelayedParseMsg
matchDelayed = \case
    SelectParseSuccess x -> Just x
    SelectParseFailure (ParseFailure x) -> Just x
    _otherwise -> Nothing

matchTransMissing :: SelectMsg -> Maybe (Maybe Unusable)
matchTransMissing = \case
    TransitiveDependencyOfDeclarationUnusable _ _ unusable _ ->
      Just $ Just unusable
    TransitiveDependencyOfDeclarationNotSelected{} ->
      Just $ Nothing
    _otherwise ->
      Nothing
