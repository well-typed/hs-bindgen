-- | The 'Raw' macro language.
--
-- Every macro is a value-like macro.
--
-- Translate all macros to their token spellings, as a
-- 'HsBindgen.Runtime.Macro.Raw'.
--
-- Intended for unqualified import.
module HsBindgen.Macro.Raw (
    Raw -- opaque
  , raw -- opaque
  ) where

import Data.Map qualified as Map
import Data.Text qualified as Text

import Clang.HighLevel.Types

import HsBindgen.Runtime.Macro qualified as RawMacro

import HsBindgen.Backend.Global (BindgenGlobalTerm (Macro_functionLike, Macro_objectLike, Macro_variadicFunctionLike),
                                 BindgenGlobalType (Macro_Raw_type, Text_type),
                                 bindgenGlobalTerm, bindgenGlobalType)
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST.Expr
import HsBindgen.Backend.SHs.AST.Expr qualified as SHs
import HsBindgen.Backend.SHs.AST.Type qualified as SHs
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Error (MacroParseError)
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Parse (spelling, splitMacro)
import HsBindgen.Macro.Type qualified as Macro

data Raw

data VoidMacro a
  deriving stock (Functor, Foldable, Traversable, Show, Eq)

absurdVoidMacro :: VoidMacro a -> b
absurdVoidMacro m = case m of {}

-- | A macro definition, untyped
--
-- 'Macro.Parsed' is indexed by the /annotation/, whereas 'RawMacro.Raw' is
-- indexed by the /token representation/; this wrapper bridges the two. The
-- annotation is a phantom: there is nothing in a raw macro to resolve.
newtype ParsedMacro ann = ParsedMacro {
      unwrap :: RawMacro.Raw Text
    }
  deriving stock (Functor, Foldable, Traversable, Show, Eq)

coerceMacro :: ParsedMacro a -> ParsedMacro b
coerceMacro = ParsedMacro . (.unwrap)

instance Macro.HasTypes Raw where
  type Parsed           Raw = ParsedMacro
  type TypecheckedType  Raw = VoidMacro
  type TypecheckedValue Raw = ParsedMacro

parseRaw ::
     [Token TokenSpelling]
  -> Either MacroParseError (Macro.Unresolved Raw)
parseRaw =
      fmap (Macro.Unresolved . ParsedMacro . fmap spelling)
    . splitMacro

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
    macro :: RawMacro.Raw Text
    macro = parsedMacro.unwrap

    -- @Macro.Raw Text@
    rawMacroT :: SHs.ClosedType
    rawMacroT =
        SHs.TApp
          (SHs.TGlobal $ bindgenGlobalType Macro_Raw_type)
          (SHs.TGlobal $ bindgenGlobalType Text_type)

    rawMacroE :: SHs.ClosedExpr
    rawMacroE = case macro.params of
        RawMacro.NoParams ->
          applyTo Macro_objectLike [nameE, bodyE]
        RawMacro.Params params False ->
          applyTo Macro_functionLike [nameE, stringsE params, bodyE]
        RawMacro.Params params True ->
          applyTo Macro_variadicFunctionLike [nameE, stringsE params, bodyE]

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
