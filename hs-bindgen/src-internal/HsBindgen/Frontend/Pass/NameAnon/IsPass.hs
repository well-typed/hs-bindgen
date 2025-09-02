module HsBindgen.Frontend.Pass.NameAnon.IsPass (
    NameAnon
  , NameAnonParseStatus(..)
  , NameAnonDeclMeta(..)
  , NameAnonMsg(..)
  ) where

import Text.SimplePrettyPrint

import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph
import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type NameAnon :: Pass
data NameAnon a deriving anyclass ValidPass

type family AnnNameAnon ix where
  AnnNameAnon "TranslationUnit" = NameAnonDeclMeta
  AnnNameAnon _                 = NoAnn

instance IsPass NameAnon where
  type Id           NameAnon = C.DeclId
  type FieldName    NameAnon = C.Name
  type ArgumentName NameAnon = Maybe C.Name
  type TypedefRef   NameAnon = C.Name
  type MacroBody    NameAnon = C.CheckedMacro NameAnon
  type ExtBinding   NameAnon = Void
  type Ann ix       NameAnon = AnnNameAnon ix
  type Msg          NameAnon = NameAnonMsg

{-------------------------------------------------------------------------------
  Information about the declarations
-------------------------------------------------------------------------------}

newtype NameAnonParseStatus = NameAnonParseStatus {
      unNameAnonParseStatus :: Map QualName (NameOrigin, ParseStatusValue)
    }
  deriving stock (Show)

data NameAnonDeclMeta = NameAnonDeclMeta {
      declIndex       :: DeclIndex
    , declUseDecl     :: UseDeclGraph
    , declDeclUse     :: DeclUseGraph
    , declParseStatus :: NameAnonParseStatus
    }
  deriving stock (Show, Generic)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data NameAnonMsg =
    -- | Skipped unused anonymous declaration
    --
    -- @clang@ will produce a warning for this ("declaration does not declare
    -- anything"); we issue a separate message here in case we skip over
    -- something that we shouldn't.
    NameAnonSkipped (C.DeclInfo Parse)
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
