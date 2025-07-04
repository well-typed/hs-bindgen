module HsBindgen.Frontend.Pass.NameAnon.IsPass (
    NameAnon
  , Msg (..)
  ) where

import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C (CName)
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type NameAnon :: Pass
data NameAnon a deriving anyclass ValidPass

type family AnnNameAnon ix where
  AnnNameAnon "TranslationUnit" = DeclMeta
  AnnNameAnon _                 = NoAnn

instance IsPass NameAnon where
  type Id         NameAnon = CName
  type FieldName  NameAnon = CName
  type TypedefRef NameAnon = CName
  type MacroBody  NameAnon = C.CheckedMacro NameAnon
  type ExtBinding NameAnon = Void
  type Ann ix     NameAnon = AnnNameAnon ix
  data Msg        NameAnon =
      -- | Skipped unused anonymous declaration entirely
      --
      -- @clang@ will produce a warning for this ("declaration does not declare
      -- anything"); we issue a separate message here in case we skip over
      -- something that we shouldn't.
      NameAnonSkipped (C.DeclInfo Parse)
    deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

instance PrettyForTrace (Msg NameAnon) where
  prettyForTrace = \case
      NameAnonSkipped info -> hsep [
          "Skipped unused anonynous declaration"
        , prettyForTrace info
        ]

instance HasDefaultLogLevel (Msg NameAnon) where
  getDefaultLogLevel = \case
      NameAnonSkipped{} -> Debug -- clang already warned
