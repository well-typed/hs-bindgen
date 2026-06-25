module HsBindgen.Macro.Resolution (
    resolveMacro
  ) where

import Data.Set qualified as Set

import C.Expr.Syntax qualified as CExpr

import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.CExpr (CExpr)
import HsBindgen.Macro.CExpr qualified as Macro
import HsBindgen.Macro.Error
import HsBindgen.Macro.Type qualified as Macro

resolveMacro ::
     Set C.DeclId
  -> Macro.Unresolved CExpr
  -> Either MacroResolutionError (Macro.Resolved CExpr)
resolveMacro declIds (Macro.Unresolved (Macro.ParsedCExpr macro)) =
    Macro.Resolved . Macro.ParsedCExpr <$> CExpr.annotateMacro resolve macro
  where
    resolve :: CExpr.Name -> () -> Either MacroResolutionError C.DeclId
    resolve name _ = case name of
      CExpr.NameOrdinary nm ->
        resolveBare nm.getIdentifier
      CExpr.NameTagged nm tag ->
        resolveTagged nm.getIdentifier (Macro.convertTagKind tag)

    -- | Resolves a bare name found in a parsed macro body
    --
    -- Result:
    --
    -- - @'Just' 'CNameKind'@: This bare name refers to another declaration that is in
    --   the 'DeclIndex'. For example, a macro, or a @typedef@.
    --
    -- - @'Nothing'@: This bare name is not in the set of known declarations. It
    --   may simply be non-existent, or refer to a built-in type or a system
    --   @typedef@ not present in the 'DeclIndex'. In this case, we /do not
    --   generate/ a dependency.
    resolveBare :: Text -> Either MacroResolutionError C.DeclId
    resolveBare nm = case (isMacro, isTypedef) of
        -- TODO <https://github.com/well-typed/hs-bindgen/issues/2097>
        --
        -- This is separate from user-selection or resolution of conflicts in
        -- the Select pass [issue-1553], but instead we'd need to know which
        -- definition C chooses to resolve to.
        --
        -- Crucially, this depends on the order of declarations (i.e., sequence
        -- order); and we currently do not have this order available. Once our
        -- lower limit on LLVM is 20.1, we can ensure sequence order and resolve
        -- to the name as C does.
        --
        -- [issue-1553] https://github.com/well-typed/hs-bindgen/issues/1553
        (True,  True)  -> Left $
          MacroResolutionError $
            "conflict detected: " <> show nm <> " is both a type and a macro"
        (True,  False) -> Right macroId
        (False, True)  -> Right typedefId
        (False, False) -> Left $
          MacroResolutionError $
            "bare identifier " <> show nm <> " not found"
      where
        macroId, typedefId :: C.DeclId
        macroId   = C.DeclId (C.DeclName nm C.NameKindMacro)    False
        typedefId = C.DeclId (C.DeclName nm C.NameKindOrdinary) False
        isMacro   = macroId   `Set.member` declIds
        isTypedef = typedefId `Set.member` declIds

    resolveTagged :: Text -> C.TagKind -> Either MacroResolutionError C.DeclId
    resolveTagged nm tag
      | taggedId `Set.member` declIds = Right taggedId
      | otherwise =
        Left $
          MacroResolutionError $
            "tagged identifier "
            <> show (C.tagKindPrefix tag)
            <> " "
            <> show nm
            <> " not found"
      where
        taggedId :: C.DeclId
        taggedId = C.DeclId (C.DeclName nm $ C.NameKindTagged tag) False
