module HsBindgen.Frontend.Pass.TypecheckMacros.Typecheck (
    typecheckDecls
  ) where

import Data.Map qualified as Map
import Data.Set qualified as Set

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis
import HsBindgen.Frontend.MacroFallback
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

{-------------------------------------------------------------------------------
  Typecheck macros
-------------------------------------------------------------------------------}

-- | Typecheck all macro declarations, threading non-macro declarations through
-- unchanged.
--
-- Returns the typechecked declarations and the set of names of type-like macros
-- (used by the reparse environment). A macro that fails to typecheck stays a
-- declaration, recording the failure (see "HsBindgen.Frontend.MacroFallback").
typecheckDecls ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> [C.Decl l In]
  -> ([C.Decl l Out], Set Text)
typecheckDecls macroLang decls =
    (decls', getKnownMacroTypes decls')
  where
    -- Needs dependency order: a macro is typechecked in the environment of the
    -- macros before it.
    tcResults :: Map Text (Macro.TypecheckResult l)
    tcResults = macroLang.typecheck (mapMaybe getResolvedMacro decls)

    decls' :: [C.Decl l Out]
    decls' = map (typecheckDecl tcResults) decls

getResolvedMacro :: C.Decl l In -> Maybe (Macro.Resolved l)
getResolvedMacro decl = case decl.kind of
    C.DeclMacro PendingMacro{handling = Handled macro} -> Just macro
    _otherwise                                         -> Nothing

{-------------------------------------------------------------------------------
  Process a single declaration
-------------------------------------------------------------------------------}

-- | Coerce a non-macro declaration to the output pass, or assemble a macro from
-- its typecheck result.
typecheckDecl ::
     forall l. Macro.HasTypes l
  => Map Text (Macro.TypecheckResult l)
  -> C.Decl l In
  -> C.Decl l Out
typecheckDecl tcResults decl = C.coercePassDeclWith kind' decl.ann decl
  where
    kind' :: C.DeclKind l Out
    kind' = case decl.kind of
      C.DeclMacro                x -> C.DeclMacro                $ assembleMacro tcResults decl.id x
      C.DeclTypedef              x -> C.DeclTypedef              $ coercePass x
      C.DeclStruct               x -> C.DeclStruct               $ coercePass x
      C.DeclUnion                x -> C.DeclUnion                $ coercePass x
      C.DeclEnum                 x -> C.DeclEnum                 $ coercePass x
      C.DeclUntaggedEnumConstant x -> C.DeclUntaggedEnumConstant $ coercePass x
      C.DeclOpaque               x -> C.DeclOpaque                 x
      C.DeclFunction             x -> C.DeclFunction             $ coercePass x
      C.DeclGlobal               x -> C.DeclGlobal               $ coercePass x

-- | Assemble a macro declaration from its (pre-computed) batch typecheck result
assembleMacro ::
     forall l. Macro.HasTypes l
  => Map Text (Macro.TypecheckResult l)
  -> C.DeclId
  -> MacroBody In l
  -> MacroBody Out l
assembleMacro tcResults declId macro = PendingMacro{
      raw      = macro.raw
    , handling = nextStage FailedTypecheck typecheck macro.handling
    }
  where
    typecheck ::
         Macro.Resolved l
      -> Either MacroTypecheckError (CheckedMacro Out l)
    typecheck resolved = case Map.lookup declId.name.text tcResults of
      Nothing ->
        panicPure $
          "assembleMacro: typechecked macro unavailable: "
            <> show declId.name.text
      Just tcRes ->
        fromTcResult resolved.deps tcRes

-- | Build the typechecked macro body from its typecheck result and (previously
-- resolved) dependencies.
fromTcResult ::
     Macro.HasTypes l
  => [(C.DeclId, Dependency)]
  -> Macro.TypecheckResult l
  -> Either MacroTypecheckError (CheckedMacro TypecheckMacros l)
fromTcResult deps = \case
    Macro.TypecheckType x ->
      Right $ CheckedType $ TypecheckedMacroType (fmap MacroTypeBodyVar x) deps NoAnn
    Macro.TypecheckValue x ->
      Right $ CheckedValue $ TypecheckedMacroValue x deps
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
    [ decl.id.name.text
    | decl <- decls
    , C.DeclMacro PendingMacro{handling = Handled CheckedType{}} <- [decl.kind]
    ]
