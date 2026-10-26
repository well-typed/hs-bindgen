module HsBindgen.Frontend.Pass.ApplyMacroFallback (
    applyMacroFallback
  ) where

import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Frontend.DeclDB (DeclDB)
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.MacroFallback
import HsBindgen.Frontend.Pass.ApplyMacroFallback.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
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
    unit{C.db = db'}
  where
    -- The edges into a rejected macro stay: its dependents use an unusable
    -- macro.
    db' :: DeclDB l Out
    db' =
        DeclDB.deleteDeps unhandled
      . runIdentity
      . DeclDB.traverseActive DeclDB.AnyOrder (pure . decide)
      $ unit.db

    unhandled :: Set C.DeclId
    unhandled = Set.fromList [
          decl.id
        | decl <- DeclDB.active DeclDB.AnyOrder unit.db
        , C.DeclMacro PendingMacro{handling = Unhandled _} <- [decl.kind]
        ]

    decide :: C.Decl l In -> DeclDB.PassResult l Out
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
                DeclDB.Deactivate $ DeclDB.Unusable $ DeclDB.UnusableMacro failure
        C.DeclTypedef              x -> keep $ C.DeclTypedef              $ coercePass x
        C.DeclStruct               x -> keep $ C.DeclStruct               $ coercePass x
        C.DeclUnion                x -> keep $ C.DeclUnion                $ coercePass x
        C.DeclEnum                 x -> keep $ C.DeclEnum                 $ coercePass x
        C.DeclUntaggedEnumConstant x -> keep $ C.DeclUntaggedEnumConstant $ coercePass x
        C.DeclOpaque               x -> keep $ C.DeclOpaque                 x
        C.DeclFunction             x -> keep $ C.DeclFunction             $ coercePass x
        C.DeclGlobal               x -> keep $ C.DeclGlobal               $ coercePass x
      where
        keep :: C.DeclKind l Out -> DeclDB.PassResult l Out
        keep kind' = DeclDB.Advanced $ C.coercePassDeclWith kind' decl.ann decl

    fallsBack :: C.Decl l In -> Bool
    fallsBack decl =
      matchSelect
        isMainHeader
        isInMainHeaderDir
        (singleLocPath decl.info.loc)
        decl.id.name
        decl.info.deprecation
        config.predicate
