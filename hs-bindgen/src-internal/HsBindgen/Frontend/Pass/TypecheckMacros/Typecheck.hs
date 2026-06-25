module HsBindgen.Frontend.Pass.TypecheckMacros.Typecheck (
    FailedMacro
  , typecheckDecls
  ) where

import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set

import Clang.HighLevel.Types

import HsBindgen.Errors (panicPure)
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

type FailedMacro = (C.DeclId, SingleLoc, MacroTypecheckError)

{-------------------------------------------------------------------------------
  Traversal 2: Typecheck macros
-------------------------------------------------------------------------------}

-- | Typecheck all macro declarations, threading non-macro declarations through
-- unchanged.
--
-- Returns the per-declaration results together with @resolvedMacroTypes@, the
-- mapping from macro names to their underlying C types (used by the reparse
-- environment).
typecheckDecls ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> [C.Decl l In]
  -> ([Either FailedMacro (C.Decl l Out)], Set Text)
typecheckDecls macroLang decls =
    merge typecheckedMacros decls
  where
    parsedMacros :: [Macro.Resolved l]
    parsedMacros = mapMaybe getParsedMacro decls

    typecheckedMacros :: Map Text (Macro.TypecheckResult l)
    typecheckedMacros =
      macroLang.typecheck parsedMacros

{-------------------------------------------------------------------------------
  Interleaving: thread typechecked macros back into declaration order
-------------------------------------------------------------------------------}

getParsedMacro :: C.Decl l In -> Maybe (Macro.Resolved l)
getParsedMacro decl = case decl.kind of
    C.DeclMacro macro -> Just macro
    _otherwise        -> Nothing

data CoerceResult l =
    NonMacro (C.Decl l Out)
  | Macro    (C.DeclInfo Out)

coerceDecl ::
     C.Decl l In
  -> CoerceResult l
coerceDecl decl = case decl.kind of
    C.DeclMacro            _ -> Macro info'
    C.DeclTypedef          k -> nonMacro $ C.DeclTypedef          $ coercePass k
    C.DeclStruct           k -> nonMacro $ C.DeclStruct           $ coercePass k
    C.DeclUnion            k -> nonMacro $ C.DeclUnion            $ coercePass k
    C.DeclEnum             k -> nonMacro $ C.DeclEnum             $ coercePass k
    C.DeclAnonEnumConstant k -> nonMacro $ C.DeclAnonEnumConstant $ coercePass k
    C.DeclOpaque mSize       -> nonMacro $ C.DeclOpaque mSize
    C.DeclFunction         k -> nonMacro $ C.DeclFunction         $ coercePass k
    C.DeclGlobal           k -> nonMacro $ C.DeclGlobal           $ coercePass k
  where
    info' :: C.DeclInfo Out
    info' = coercePass decl.info

    nonMacro :: C.DeclKind l Out -> CoerceResult l
    nonMacro kind' = NonMacro (C.Decl info' kind' decl.ann)

-- | Merge typechecked macro results back into the original declarations
merge ::
     forall l. Macro.HasTypes l
  => Map Text (Macro.TypecheckResult l)
  -> [C.Decl l In]
  -> ([Either FailedMacro (C.Decl l Out)], Set Text)
merge checkedMacros =
    first reverse . Foldable.foldl' step ([], Set.empty)
  where
    step ::
         ([Either FailedMacro (C.Decl l Out)], Set Text)
      -> C.Decl l In
      -> ([Either FailedMacro (C.Decl l Out)], Set Text)
    step (eDecls, resolvedMacroTypes) decl = case coerceDecl decl of
      NonMacro decl' -> (Right decl' : eDecls, resolvedMacroTypes)
      Macro    info' ->
        let macroName = info'.id.name.text
        in  case Map.lookup macroName checkedMacros of
              Nothing ->
                panicPure $ "merge: typechecked macro unavailable: "
                          <> show macroName
              Just checkedMacro ->
                ( fromMacroTcResult info' checkedMacro : eDecls
                , addKnownMacro
                    macroName
                    checkedMacro
                    resolvedMacroTypes
                )

fromMacroTcResult ::
     Macro.HasTypes l
  => C.DeclInfo Out
  -> Macro.TypecheckResult l
  -> Either FailedMacro (C.Decl l Out)
fromMacroTcResult info = \case
    Macro.TypecheckType  x ->
      Right $ toDecl $
        MacroType $ TypecheckedMacroType (fmap MacroTypeBodyVar x) NoAnn
    Macro.TypecheckValue x ->
      Right $ toDecl $
        MacroValue $ TypecheckedMacroValue x
    Macro.TypecheckError err ->
      Left (info.id, info.loc, err)
  where
    toDecl :: TypecheckedMacro Out l -> C.Decl l Out
    toDecl checked = C.Decl{ info = info, kind = C.DeclMacro checked, ann = NoAnn }

{-------------------------------------------------------------------------------
  Build resolvedMacroTypes
-------------------------------------------------------------------------------}

-- | Add a macro's underlying C type to the accumulator.
--
-- Special case: @#define bool _Bool@ from stdbool.h is normalised so that
-- @bool@ renders identically to @_Bool@ regardless of @language-c@ version.
addKnownMacro ::
     Text
  -> Macro.TypecheckResult l
  -> Set Text
  -> Set Text
addKnownMacro macroName tcRes knownMacros =
    case tcRes of
      Macro.TypecheckType _body -> Set.insert macroName knownMacros
      _                         -> knownMacros
