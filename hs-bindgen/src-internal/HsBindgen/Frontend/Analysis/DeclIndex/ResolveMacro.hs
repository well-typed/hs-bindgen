module HsBindgen.Frontend.Analysis.DeclIndex.ResolveMacro (
    resolveMacroWith
  ) where


import HsBindgen.Frontend.MacroFallback
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Error
import HsBindgen.Macro.Interface qualified as Macro

type In  = EnrichComments
type Out = ConstructTranslationUnit

-- | Resolve the names in a macro declaration
--
-- A macro whose names the macro language cannot resolve stays a declaration:
-- it may still fall back (see @ApplyMacroFallback@).
resolveMacroWith ::
     forall l.
     Macro.Lang l
  -> Set C.DeclId
  -> C.Decl l In
  -> C.Decl l Out
resolveMacroWith macroLang allDeclIds decl = C.coercePassDeclWith kind' decl.ann decl
  where
    kind' :: C.DeclKind l Out
    kind' = case decl.kind of
      C.DeclStruct               x -> C.DeclStruct               $ coercePass x
      C.DeclUnion                x -> C.DeclUnion                $ coercePass x
      C.DeclTypedef              x -> C.DeclTypedef              $ coercePass x
      C.DeclEnum                 x -> C.DeclEnum                 $ coercePass x
      C.DeclUntaggedEnumConstant x -> C.DeclUntaggedEnumConstant $ coercePass x
      C.DeclOpaque               x -> C.DeclOpaque               $ x
      C.DeclMacro                x -> C.DeclMacro                $ resolveMacro macroLang allDeclIds x
      C.DeclFunction             x -> C.DeclFunction             $ coercePass x
      C.DeclGlobal               x -> C.DeclGlobal               $ coercePass x

resolveMacro ::
     Macro.Lang l
  -> Set C.DeclId
  -> MacroBody In l
  -> MacroBody Out l
resolveMacro macroLang allDeclIds macro = PendingMacro{
      raw      = macro.raw
    , handling =
        nextStage FailedResolve (macroLang.resolve allDeclIds) macro.handling
    }
