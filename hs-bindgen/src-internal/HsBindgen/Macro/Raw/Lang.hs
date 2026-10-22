module HsBindgen.Macro.Raw.Lang (
    raw -- opaque
  ) where

import Data.Map qualified as Map

import HsBindgen.Runtime.Macro qualified as Runtime.Macro (Raw (name))

import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.SHs.AST.Expr
import HsBindgen.Backend.SHs.Translation.RawMacro
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
translateRaw name parsedMacro = rawMacroBinding name parsedMacro.unwrap

raw :: Macro.Lang Raw
raw = Macro.Lang{
    parse          = parseRaw
  , resolve        = \_ -> Right . resolveRaw
  , typecheck      = typecheckRaw
  , translateType  = absurdVoidMacro
  , translateValue = translateRaw
  }
