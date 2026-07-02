module HsBindgen.Frontend.Analysis.DeclIndex.ResolveMacro (
    resolveMacroWith
  ) where


import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Error
import HsBindgen.Macro.Interface qualified as Macro

type In  = EnrichComments
type Out = ConstructTranslationUnit

resolveMacroWith ::
     Macro.Lang l
  -> Set C.DeclId
  -> C.Decl l In
  -> Either MacroResolutionError (C.Decl l Out)
resolveMacroWith macroLang allDeclIds decl = case decl.kind of
    C.DeclStruct           x -> Right $ aux $ C.DeclStruct           $ coercePass x
    C.DeclUnion            x -> Right $ aux $ C.DeclUnion            $ coercePass x
    C.DeclTypedef          x -> Right $ aux $ C.DeclTypedef          $ coercePass x
    C.DeclEnum             x -> Right $ aux $ C.DeclEnum             $ coercePass x
    C.DeclAnonEnumConstant x -> Right $ aux $ C.DeclAnonEnumConstant $ coercePass x
    C.DeclOpaque           x -> Right $ aux $ C.DeclOpaque           $ x
    C.DeclMacro            x -> resolveMacro macroLang allDeclIds info' decl.ann x
    C.DeclFunction         x -> Right $ aux $ C.DeclFunction         $ coercePass x
    C.DeclGlobal           x -> Right $ aux $ C.DeclGlobal           $ coercePass x
  where
    info' :: C.DeclInfo Out
    info' = coercePass decl.info

    aux :: C.DeclKind l Out -> C.Decl l Out
    aux x = C.Decl info' x decl.ann

resolveMacro ::
     Macro.Lang l
  -> Set C.DeclId
  -> C.DeclInfo Out
  -> Ann "Decl" Out
  -> MacroBody In l
  -> Either MacroResolutionError (C.Decl l Out)
resolveMacro macroLang allDeclIds info' ann unresolvedMacro = do
    resolvedMacro <- macroLang.resolve allDeclIds unresolvedMacro
    pure $ C.Decl info' (C.DeclMacro resolvedMacro) ann
