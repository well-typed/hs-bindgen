module HsBindgen.Macro.Raw.Lang (
    raw -- opaque
  ) where

import Data.Map qualified as Map
import Data.Text qualified as Text

import HsBindgen.Runtime.Macro qualified as Runtime.Macro

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST.Expr
import HsBindgen.Backend.SHs.AST.Expr qualified as SHs
import HsBindgen.Backend.SHs.AST.Type qualified as SHs
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Raw.Parse

resolveRaw :: Macro.Unresolved Raw -> Macro.Resolved Raw
resolveRaw m = Macro.Resolved{
      macro = coerceMacro m.unwrap
    , deps = []
    }

typecheckRaw :: [Macro.Resolved Raw] -> Map Text (Macro.TypecheckResult Raw)
typecheckRaw xs =
    Map.fromList [
        (parsedMacro.unwrap.name, Macro.TypecheckValue parsedMacro)
      | resolvedMacro <- xs
      , let parsedMacro = resolvedMacro.macro
      ]

translateRaw ::
     Hs.Name Hs.NsVar
  -> ParsedMacro a
  -> Maybe HsDoc.Comment
  -> Binding
translateRaw name parsedMacro mDoc = Binding{
      name       = Hs.ExportedName name
    , parameters = []
    , result     = Result rawMacroT Nothing
    , body       = rawMacroE
    , pragmas    = []
    , comment    = mDoc
    }
  where
    macro :: Runtime.Macro.Raw Text
    macro = parsedMacro.unwrap

    -- @Macro.Raw Text@
    rawMacroT :: SHs.ClosedType
    rawMacroT =
        SHs.TApp
          (SHs.TGlobal $ bindgenGlobalType Macro_Raw_type)
          (SHs.TGlobal $ bindgenGlobalType String_type)

    rawMacroE :: SHs.ClosedExpr
    rawMacroE = case macro.params of
        Runtime.Macro.NoParams ->
          applyTo Macro_objectLike [nameE, bodyE]
        Runtime.Macro.Params names Runtime.Macro.NotVariadic ->
          applyTo Macro_functionLike [nameE, stringsE names, bodyE]
        Runtime.Macro.Params names Runtime.Macro.Ellipsis ->
          applyTo Macro_variadicFunctionLike [nameE, stringsE names, bodyE]
        Runtime.Macro.Params names (Runtime.Macro.NamedEllipsis ellipsisName) ->
          applyTo Macro_namedVariadicFunctionLike
            [nameE, stringsE names, stringE ellipsisName, bodyE]

    nameE, bodyE :: SHs.ClosedExpr
    nameE = stringE macro.name
    bodyE = stringsE macro.body

    stringE :: Text -> SHs.ClosedExpr
    stringE = SHs.EString . Text.unpack

    stringsE :: [Text] -> SHs.ClosedExpr
    stringsE = SHs.EList . map stringE

    applyTo :: BindgenGlobalTerm -> [SHs.ClosedExpr] -> SHs.ClosedExpr
    applyTo f = foldl' SHs.EApp (SHs.EGlobal $ bindgenGlobalTerm f)

raw :: Macro.Lang Raw
raw = Macro.Lang{
    parse          = parseRaw
  , resolve        = \_ -> Right . resolveRaw
  , typecheck      = typecheckRaw
  , translateType  = absurdVoidMacro
  , translateValue = translateRaw
  }
