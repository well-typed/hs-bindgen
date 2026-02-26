-- | Pattern synonyms for working with traces
module Test.Common.HsBindgen.Trace.Patterns (
    -- * Clang
    pattern MatchDiagnosticOption
  , pattern MatchDiagnosticCategory
  , matchDiagnosticSpelling
    -- * Parse
  , pattern MatchParse
  , pattern MatchParseTypeException
  , pattern MatchParseDeclException
  , pattern MatchParseMsg
    -- * HandleMacros
  , pattern MatchHandleMacros
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
  ) where

import Data.Text qualified as Text

import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports
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

pattern MatchParse :: CDeclName -> ImmediateParseMsg -> TraceMsg
pattern MatchParse name x <- TraceFrontend (
      FrontendParse WithLocationInfo{
          loc = locationInfoName -> Just name
        , msg = x
        }
    )

pattern MatchParseTypeException :: CDeclName -> ParseTypeException -> TraceMsg
pattern MatchParseTypeException name x <-
  MatchSelect name (matchParseTypeException -> Just x)

pattern MatchParseDeclException :: CDeclName -> ParseDeclException -> TraceMsg
pattern MatchParseDeclException name x <-
  MatchSelect name (matchParseDeclException -> Just x)

pattern MatchParseMsg :: CDeclName -> ParseMsg -> TraceMsg
pattern MatchParseMsg name x <- MatchSelect name (matchParseMsg -> Just x)

pattern MatchUnknownStorageClass :: CX_StorageClass -> ParseMsg
pattern MatchUnknownStorageClass x <- ParseUnknownStorageClass (
      fromSimpleEnum -> Right x
    )

{-------------------------------------------------------------------------------
  HandleMacros
-------------------------------------------------------------------------------}

pattern MatchHandleMacros :: HandleMacrosReparseMsg -> TraceMsg
pattern MatchHandleMacros x <- TraceFrontend (
      FrontendHandleMacros x
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

pattern MatchSelect :: CDeclName -> SelectMsg -> TraceMsg
pattern MatchSelect name x <- TraceFrontend (
      FrontendSelect WithLocationInfo{
          loc = locationInfoName -> Just name
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
pattern MatchTransUnusable x <- TransitiveDependencyUnusable _ x _

{-------------------------------------------------------------------------------
  MangleNames
-------------------------------------------------------------------------------}

pattern MatchMangle :: CDeclName -> MangleNamesMsg -> TraceMsg
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

matchParseTypeException :: SelectMsg -> Maybe ParseTypeException
matchParseTypeException x =  case matchDelayed x of
  Just (ParseTypeException m) -> Just m
  _otherwise        -> Nothing

matchParseDeclException :: SelectMsg -> Maybe ParseDeclException
matchParseDeclException x =  case matchDelayed x of
  Just (ParseDeclException m) -> Just m
  _otherwise        -> Nothing

matchParseMsg :: SelectMsg -> Maybe ParseMsg
matchParseMsg x = case matchDelayed x of
  Just (ParseMsg m) -> Just m
  _otherwise        -> Nothing
