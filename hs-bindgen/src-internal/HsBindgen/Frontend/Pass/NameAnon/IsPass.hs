module HsBindgen.Frontend.Pass.NameAnon.IsPass (
    NameAnon
  , NameAnonMsg(..)
  ) where

import Text.SimplePrettyPrint

import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type NameAnon :: Pass
data NameAnon a deriving anyclass ValidPass

type family AnnNameAnon ix where
  AnnNameAnon "TranslationUnit" = DeclMeta
  AnnNameAnon _                 = NoAnn

instance IsPass NameAnon where
  type Id           NameAnon = C.DeclId NameAnon
  type FieldName    NameAnon = C.Name
  type ArgumentName NameAnon = Maybe C.Name
  type TypedefRef   NameAnon = OrigTypedefRef NameAnon
  type MacroBody    NameAnon = C.CheckedMacro NameAnon
  type ExtBinding   NameAnon = Void
  type Ann ix       NameAnon = AnnNameAnon ix
  type Msg          NameAnon = NameAnonMsg

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data NameAnonMsg =
    -- | Skipped unused anonymous declaration
    --
    -- @clang@ will produce a warning for this ("declaration does not declare
    -- anything"); we issue a separate message here in case we skip over
    -- something that we shouldn't.
    NameAnonSkipped (C.DeclInfo HandleMacros)
  deriving stock (Show)

instance PrettyForTrace NameAnonMsg where
  prettyForTrace = \case
      NameAnonSkipped info -> hsep [
          "Skipped unused anonynous declaration"
        , prettyForTrace info
        ]

instance IsTrace Level NameAnonMsg where
  getDefaultLogLevel = \case
      NameAnonSkipped{} -> Debug -- clang already warned
  getSource  = const HsBindgen
  getTraceId = const "name-anon"
