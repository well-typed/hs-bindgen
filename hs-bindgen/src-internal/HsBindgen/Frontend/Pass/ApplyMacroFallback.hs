module HsBindgen.Frontend.Pass.ApplyMacroFallback (
    applyMacroFallback
  ) where

import Data.Either (partitionEithers)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.MacroFallback
import HsBindgen.Frontend.Pass.ApplyMacroFallback.IsPass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (ConstructTranslationUnit)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass

type In  = TypecheckMacros
type Out = ApplyMacroFallback

-- | Decide whether to drop or fall back macros the macro language gave up on
--
-- A macro matching the predicate becomes a fallback macro ('MacroRaw'). Any
-- other is removed from the declarations and registered as unusable, with the
-- reason of the stage that gave up. Either way, the macro loses its
-- dependencies.
applyMacroFallback ::
     forall l.
     IsMainHeader
  -> IsInMainHeaderDir
  -> MacroFallback
  -> C.TranslationUnit l In
  -> C.TranslationUnit l Out
applyMacroFallback isMainHeader isInMainHeaderDir config unit =
    C.TranslationUnit{
        decls        = decls'
      , includeGraph = unit.includeGraph
      , meta         = DeclMeta{
            declDB       = declDB'
          , useDeclGraph = UseDeclGraph.fromDeclUseGraph declUseGraph'
          , declUseGraph = declUseGraph'
          }
      }
  where
    declDB' :: DeclDB l ConstructTranslationUnit
    declDB' = DeclDB.deactivate (Map.fromList rejected) unit.meta.declDB

    -- The edges into a rejected macro stay: its dependents use an unusable
    -- macro.
    declUseGraph' :: DeclUseGraph
    declUseGraph' = DeclUseGraph.deleteDeps unhandled unit.meta.declUseGraph

    unhandled :: Set C.DeclId
    unhandled = Set.fromList [
          decl.id
        | decl <- unit.decls
        , C.DeclMacro PendingMacro{handling = Unhandled _} <- [decl.kind]
        ]

    rejected :: [(C.DeclId, DeclDB.Deactivation)]
    decls'   :: [C.Decl l Out]
    (rejected, decls') = partitionEithers $ map decide unit.decls

    decide ::
         C.Decl l In
      -> Either (C.DeclId, DeclDB.Deactivation) (C.Decl l Out)
    decide decl = case decl.kind of
        C.DeclMacro macro -> case macro.handling of
          Handled (CheckedType  x) -> keep $ C.DeclMacro $ MacroType  $ coercePass x
          Handled (CheckedValue x) -> keep $ C.DeclMacro $ MacroValue $ coercePass x
          Unhandled failure
            | fallsBack decl ->
                keep $ C.DeclMacro $ MacroRaw FallbackMacro{
                    raw     = macro.raw
                  , failure = failure
                  }
            | otherwise ->
                Left (decl.id, DeclDB.Unusable $ DeclDB.UnusableMacro failure)
        C.DeclTypedef              x -> keep $ C.DeclTypedef              $ coercePass x
        C.DeclStruct               x -> keep $ C.DeclStruct               $ coercePass x
        C.DeclUnion                x -> keep $ C.DeclUnion                $ coercePass x
        C.DeclEnum                 x -> keep $ C.DeclEnum                 $ coercePass x
        C.DeclUntaggedEnumConstant x -> keep $ C.DeclUntaggedEnumConstant $ coercePass x
        C.DeclOpaque               x -> keep $ C.DeclOpaque                 x
        C.DeclFunction             x -> keep $ C.DeclFunction             $ coercePass x
        C.DeclGlobal               x -> keep $ C.DeclGlobal               $ coercePass x
      where
        keep :: C.DeclKind l Out -> Either a (C.Decl l Out)
        keep kind' = Right $ C.coercePassDeclWith kind' decl.ann decl

    fallsBack :: C.Decl l In -> Bool
    fallsBack decl =
      matchSelect
        isMainHeader
        isInMainHeaderDir
        (singleLocPath decl.info.loc)
        decl.id.name
        decl.info.availability
        config.predicate
