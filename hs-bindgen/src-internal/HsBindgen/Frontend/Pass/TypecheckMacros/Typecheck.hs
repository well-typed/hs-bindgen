module HsBindgen.Frontend.Pass.TypecheckMacros.Typecheck (
    FailedMacro
  , typecheckDecls
  ) where

import Data.Either
import Data.Map qualified as Map
import Data.Set qualified as Set

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Error
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro

type In    = ConstructTranslationUnit
type Out   = TypecheckMacros

type FailedMacro = (C.DeclInfo Out, MacroTypecheckError)

{-------------------------------------------------------------------------------
  Typecheck macros
-------------------------------------------------------------------------------}

-- | Typecheck all macro declarations, threading non-macro declarations through
-- unchanged.
--
-- Returns the typechecked declarations, the set of names of type-like macros
-- (used by the reparse environment), and the macros that failed to typecheck.
typecheckDecls ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> [C.Decl l In]
  -> ([C.Decl l Out], Set Text, [FailedMacro])
typecheckDecls macroLang decls =
    (successes, getKnownMacroTypes successes, failures)
  where
    tcResults :: Map Text (Macro.TypecheckResult l)
    tcResults = macroLang.typecheck (mapMaybe getResolvedMacro decls)

    failures  :: [FailedMacro]
    successes :: [C.Decl l Out]
    (failures, successes) = partitionEithers (map (toOutcome tcResults) decls)

getResolvedMacro :: C.Decl l In -> Maybe (Macro.Resolved l)
getResolvedMacro decl = case decl.kind of
    C.DeclMacro macro -> Just macro
    _otherwise        -> Nothing

{-------------------------------------------------------------------------------
  Process a single declaration
-------------------------------------------------------------------------------}

-- | The outcome of processing a single declaration: a macro that failed to
-- typecheck (dropped from the output), or a declaration to keep (a non-macro,
-- or a successfully typechecked macro).
type Outcome l = Either FailedMacro (C.Decl l Out)

-- | Coerce a non-macro declaration to the output pass, or assemble a macro from
-- its typecheck result.
toOutcome ::
     forall l. Macro.HasTypes l
  => Map Text (Macro.TypecheckResult l)
  -> C.Decl l In
  -> Outcome l
toOutcome tcResults decl = case decl.kind of
    C.DeclMacro            x -> assembleMacro tcResults info' decl.ann x
    C.DeclTypedef          x -> nonMacro $ C.DeclTypedef          $ coercePass x
    C.DeclStruct           x -> nonMacro $ C.DeclStruct           $ coercePass x
    C.DeclUnion            x -> nonMacro $ C.DeclUnion            $ coercePass x
    C.DeclEnum             x -> nonMacro $ C.DeclEnum             $ coercePass x
    C.DeclAnonEnumConstant x -> nonMacro $ C.DeclAnonEnumConstant $ coercePass x
    C.DeclOpaque           x -> nonMacro $ C.DeclOpaque             x
    C.DeclFunction         x -> nonMacro $ C.DeclFunction         $ coercePass x
    C.DeclGlobal           x -> nonMacro $ C.DeclGlobal           $ coercePass x
  where
    info' :: C.DeclInfo Out
    info' = coercePass decl.info

    nonMacro :: C.DeclKind l Out -> Outcome l
    nonMacro kind' = Right $ C.Decl info' kind' decl.ann

-- | Assemble a macro declaration from its (pre-computed) batch typecheck result,
-- reporting failure if typechecking did not succeed.
assembleMacro ::
     Macro.HasTypes l
  => Map Text (Macro.TypecheckResult l)
  -> C.DeclInfo Out
  -> Ann "Decl" Out
  -> Macro.Resolved l
  -> Outcome l
assembleMacro tcResults info ann resolved =
    case Map.lookup info.id.name.text tcResults of
      Nothing ->
        panicPure $
          "assembleMacro: typechecked macro unavailable: "
            <> show info.id.name.text
      Just tcRes -> case fromTcResult resolved.deps tcRes of
        Left  err  -> Left (info, err)
        Right body -> Right (C.Decl info (C.DeclMacro body) ann)

-- | Build the typechecked macro body from its typecheck result and (previously
-- resolved) dependencies.
fromTcResult ::
     Macro.HasTypes l
  => [(C.DeclId, Dependency)]
  -> Macro.TypecheckResult l
  -> Either MacroTypecheckError (TypecheckedMacro TypecheckMacros l)
fromTcResult deps = \case
    Macro.TypecheckType x ->
      Right $ MacroType $ TypecheckedMacroType (fmap MacroTypeBodyVar x) deps NoAnn
    Macro.TypecheckValue x ->
      Right $ MacroValue $ TypecheckedMacroValue x deps
    Macro.TypecheckError err ->
      Left err

{-------------------------------------------------------------------------------
  Known macro types
-------------------------------------------------------------------------------}

-- | Names of the type-like macros among the given declarations.
--
-- The reparse environment uses these names to recognise macro-defined type
-- names. Special case: @#define bool _Bool@ from stdbool.h is normalised so
-- that @bool@ renders identically to @_Bool@ regardless of @language-c@
-- version.
getKnownMacroTypes :: [C.Decl l Out] -> Set Text
getKnownMacroTypes decls = Set.fromList
    [ decl.info.id.name.text
    | decl <- decls
    , C.DeclMacro (MacroType _) <- [decl.kind]
    ]
