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
import HsBindgen.Macro.Interface
import HsBindgen.Macro.Type

type In    = ConstructTranslationUnit
type Out   = TypecheckMacros
type CType = C.Type Out

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
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1952>
--
-- We should only resolve the declaration IDs of macro dependencies once. Now
-- we resolve them twice: when we get the dependencies of parsed macros, and
-- when we typecheck macros.
typecheckDecls ::
     forall l. HasMacroTypes l
  => MacroLang l
  -> Map C.DeclId CType
  -> [C.Decl l In]
  -> ([Either FailedMacro (C.Decl l Out)], Set Text)
typecheckDecls macroLang knownTypes decls =
    merge typecheckedMacros decls
  where
    parsedMacros :: [ParsedMacroBody l]
    parsedMacros = mapMaybe getParsedMacro decls

    typecheckedMacros :: Map Text (MacroTypecheckResult l)
    typecheckedMacros =
      macroLang.typecheckMacroBodies (Map.keysSet knownTypes) parsedMacros

{-------------------------------------------------------------------------------
  Interleaving: thread typechecked macros back into declaration order
-------------------------------------------------------------------------------}

getParsedMacro :: C.Decl l In -> Maybe (ParsedMacroBody l)
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
    C.DeclOpaque             -> nonMacro $ C.DeclOpaque
    C.DeclFunction         k -> nonMacro $ C.DeclFunction         $ coercePass k
    C.DeclGlobal           k -> nonMacro $ C.DeclGlobal           $ coercePass k
  where
    info' :: C.DeclInfo Out
    info' = coercePass decl.info

    nonMacro :: C.DeclKind l Out -> CoerceResult l
    nonMacro kind' = NonMacro (C.Decl info' kind' decl.ann)

-- | Merge typechecked macro results back into the original declarations
merge ::
     forall l. HasMacroTypes l
  => Map Text (MacroTypecheckResult l)
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
     HasMacroTypes l
  => C.DeclInfo Out
  -> MacroTypecheckResult l
  -> Either FailedMacro (C.Decl l Out)
fromMacroTcResult info = \case
    MacroTypecheckType  x ->
      Right $ toDecl $
        MacroType $ TypecheckedMacroType (fmap MacroTypeBodyVar x) NoAnn
    MacroTypecheckValue x ->
      Right $ toDecl $
        MacroValue $ TypecheckedMacroValue x
    MacroTypecheckError err ->
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
  -> MacroTypecheckResult l
  -> Set Text
  -> Set Text
addKnownMacro macroName tcRes knownMacros =
    case tcRes of
      MacroTypecheckType _body -> Set.insert macroName knownMacros
      _                        -> knownMacros
