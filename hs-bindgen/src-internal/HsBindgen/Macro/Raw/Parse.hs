module HsBindgen.Macro.Raw.Parse (
    Raw -- opaque
  , VoidMacro
  , absurdVoidMacro
  , ParsedMacro(..)
  , coerceMacro
  , parseRaw
  ) where

import Data.Text (Text)

import Clang.HighLevel.Types

import HsBindgen.Runtime.Macro qualified as RawMacro

import HsBindgen.Macro.Error
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Parse
import HsBindgen.Macro.Syntax
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
