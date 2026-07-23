-- | The 'Raw' macro language.
--
-- Every macro is a value-like macro.
--
-- Translate all macros to their list of tokens.
--
-- Intended for unqualified import.
module HsBindgen.Macro.Raw (
    Raw -- opaque
  , raw -- opaque
  ) where

import Data.Map qualified as Map
import Data.Text qualified as Text

import Clang.HighLevel.Types

import HsBindgen.Backend.Global (BindgenGlobalType (String_type),
                                 bindgenGlobalType)
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST.Expr
import HsBindgen.Backend.SHs.AST.Expr qualified as SHs
import HsBindgen.Backend.SHs.AST.Type qualified as SHs
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Error (MacroParseError (MacroParseError))
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro

data Raw

data VoidMacro a
  deriving stock (Functor, Foldable, Traversable, Show, Eq)

absurdVoidMacro :: VoidMacro a -> b
absurdVoidMacro m = case m of {}

data RawMacro a = RawMacro {
    name   :: Text
  , tokens :: [Token TokenSpelling]
  }
  deriving stock (Functor, Foldable, Traversable, Show, Eq)

coerceMacro :: RawMacro a -> RawMacro b
coerceMacro m = RawMacro{
      name   = m.name
    , tokens = m.tokens
    }

instance Macro.HasTypes Raw where
  type Parsed           Raw = RawMacro
  type TypecheckedType  Raw = VoidMacro
  type TypecheckedValue Raw = RawMacro

parseRaw :: [Token TokenSpelling] -> Either MacroParseError (Macro.Unresolved Raw)
parseRaw []     = Left  $ MacroParseError "parseRaw: empty macro"
parseRaw (t:ts) = Right $ Macro.Unresolved $ RawMacro{
      name   = t.tokenSpelling.getTokenSpelling
    , tokens = ts
    }

resolveRaw :: Macro.Unresolved Raw -> Macro.Resolved Raw
resolveRaw m = Macro.Resolved{
      macro = coerceMacro m.unwrap
    , deps = []
    }

typecheckRaw :: [Macro.Resolved Raw] -> Map Text (Macro.TypecheckResult Raw)
typecheckRaw xs =
    Map.fromList [
        (rawMacro.name, Macro.TypecheckValue rawMacro)
      | resolvedMacro <- xs
      , let rawMacro = resolvedMacro.macro
      ]

translateRaw ::
     Hs.Name Hs.NsVar
  -> RawMacro a
  -> Maybe HsDoc.Comment
  -> Binding
translateRaw name rawMacro mDoc = Binding{
      name       = Hs.ExportedName name
    , parameters = []
    , result     = Result listOfStringsT Nothing
    , body       = SHs.EList $ map tokenToStringE rawMacro.tokens
    , pragmas    = []
    , comment    = mDoc
    }
  where
    listOfStringsT :: SHs.ClosedType
    listOfStringsT = SHs.TList $ SHs.TGlobal $ bindgenGlobalType String_type

    tokenToStringE :: Token TokenSpelling -> SHs.ClosedExpr
    tokenToStringE t = SHs.EString $ Text.unpack t.tokenSpelling.getTokenSpelling

raw :: Macro.Lang Raw
raw = Macro.Lang{
    parse          = parseRaw
  , resolve        = \_ -> Right . resolveRaw
  , typecheck      = typecheckRaw
  , translateType  = absurdVoidMacro
  , translateValue = translateRaw
  }
