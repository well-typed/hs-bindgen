-- | Frontend dump targets, for use in the 'HsBindgen.Artefact.Artefact' GADT
-- and CLI @internal@ commands.
--
-- Each constructor corresponds to a frontend pass, with its result type.
module HsBindgen.FrontendDump (
    FrontendDump(..)
  , frontendDumpName
    -- * Existential wrapper
  , SomeFrontendDump(..)
  , parseFrontendDumpName
  ) where

import Data.List (intercalate)

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass (AdjustTypes)
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass (AssignAnonIds)
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (ConstructTranslationUnit)
import HsBindgen.Frontend.Pass.HandleMacros.IsPass (HandleMacros)
import HsBindgen.Frontend.Pass.MangleNames.IsPass (MangleNames)
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Parse.Result (ParseResult)
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (ResolveBindingSpecs)
import HsBindgen.Frontend.Pass.Select.IsPass (Select)
import HsBindgen.Frontend.Pass.SimplifyAST.IsPass (SimplifyAST)
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Frontend dumps
--
-- Each constructor names a frontend pass and carries the result type of running
-- that pass. See "HsBindgen.Frontend" for the pass ordering and descriptions.
data FrontendDump (result :: Star) where
  DumpParse
    :: FrontendDump [ParseResult Parse]
  DumpSimplifyAST
    :: FrontendDump [ParseResult SimplifyAST]
  DumpAssignAnonIds
    :: FrontendDump [ParseResult AssignAnonIds]
  DumpConstructTranslationUnit
    :: FrontendDump (C.TranslationUnit ConstructTranslationUnit)
  DumpHandleMacros
    :: FrontendDump (C.TranslationUnit HandleMacros)
  DumpResolveBindingSpecs
    :: FrontendDump (C.TranslationUnit ResolveBindingSpecs)
  DumpMangleNames
    :: FrontendDump (C.TranslationUnit MangleNames)
  DumpSelect
    :: FrontendDump (C.TranslationUnit Select)
  DumpAdjustTypes
    :: FrontendDump (C.TranslationUnit AdjustTypes)

-- | Human-readable name of a frontend dump target (for CLI and traces)
frontendDumpName :: FrontendDump result -> String
frontendDumpName = \case
  DumpParse                    -> "parse"
  DumpSimplifyAST              -> "simplify-ast"
  DumpAssignAnonIds            -> "assign-anon-ids"
  DumpConstructTranslationUnit -> "construct-translation-unit"
  DumpHandleMacros             -> "handle-macros"
  DumpResolveBindingSpecs      -> "resolve-binding-specs"
  DumpMangleNames              -> "mangle-names"
  DumpSelect                   -> "select"
  DumpAdjustTypes              -> "adjust-types"

{-------------------------------------------------------------------------------
  Existential wrapper
-------------------------------------------------------------------------------}

-- | Existential wrapper pairing 'FrontendDump' with a 'Show' constraint.
data SomeFrontendDump where
  SomeFrontendDump :: Show result => FrontendDump result -> SomeFrontendDump

-- | Parse a frontend dump name (inverse of 'frontendDumpName').
--
-- Returns 'Left' with an error message listing valid names on failure.
parseFrontendDumpName :: String -> Either String SomeFrontendDump
parseFrontendDumpName s = case lookup s knownPasses of
    Just d  -> Right d
    Nothing -> Left $
      "unknown pass " ++ show s ++ "; valid passes: "
        ++ intercalate ", " (map fst knownPasses)
  where
    knownPasses :: [(String, SomeFrontendDump)]
    knownPasses = [
          mk DumpParse
        , mk DumpSimplifyAST
        , mk DumpAssignAnonIds
        , mk DumpConstructTranslationUnit
        , mk DumpHandleMacros
        , mk DumpResolveBindingSpecs
        , mk DumpMangleNames
        , mk DumpSelect
        , mk DumpAdjustTypes
        ]

    mk :: Show result => FrontendDump result -> (String, SomeFrontendDump)
    mk d = (frontendDumpName d, SomeFrontendDump d)
