module HsBindgen.Frontend.Pass.FillUnnamedIds.IsPass (
    FillUnnamedIds
  , ImmediateFillUnnamedIdsMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type FillUnnamedIds :: Pass
data FillUnnamedIds a

-- We preserve the annotations from the @Parse@ pass
type family AnnFillUnnamedIds ix where
  AnnFillUnnamedIds "ExplicitField" = ReparseInfo Tokens
  AnnFillUnnamedIds "Function"      = ReparseInfo Tokens
  AnnFillUnnamedIds "Global"        = ReparseInfo Tokens
  AnnFillUnnamedIds "IndirectField" = ReparseInfo Tokens
  AnnFillUnnamedIds "Typedef"       = ReparseInfo Tokens
  AnnFillUnnamedIds _               = NoAnn

instance IsPass FillUnnamedIds

instance PassId FillUnnamedIds

instance PassScopedName FillUnnamedIds

instance PassMacro FillUnnamedIds where
  type MacroBody FillUnnamedIds = Macro.Unresolved

instance PassExtBinding FillUnnamedIds

instance PassCommentDecl FillUnnamedIds

instance PassAnn FillUnnamedIds where
  type Ann ix FillUnnamedIds = AnnFillUnnamedIds ix

instance PassMsg FillUnnamedIds where
  type Msg FillUnnamedIds = ImmediateFillUnnamedIdsMsg

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ImmediateFillUnnamedIdsMsg =
    -- | Skipped unused unnamed declaration
    --
    -- @clang@ will produce a warning for this ("declaration does not declare
    -- anything"); we issue a separate message here in case we skip over
    -- something that we shouldn't.
    FillUnnamedIdsSkippedDecl C.UnnamedId
  deriving stock (Show, Generic)

instance PrettyForTrace ImmediateFillUnnamedIdsMsg where
  prettyForTrace = \case
      FillUnnamedIdsSkippedDecl unnamedId -> PP.hsep [
          "Skipped unused or unusable unnamed declaration"
        , prettyForTrace unnamedId
        ]

instance IsTrace Level ImmediateFillUnnamedIdsMsg where
  getDefaultLogLevel = \case
      FillUnnamedIdsSkippedDecl{} -> Debug -- clang already warned

  getSource  = const HsBindgen
  getTraceId = const "fill-unnamed-ids"
