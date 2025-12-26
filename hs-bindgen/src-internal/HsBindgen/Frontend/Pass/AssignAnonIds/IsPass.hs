module HsBindgen.Frontend.Pass.AssignAnonIds.IsPass (
    AssignAnonIds
  , ImmediateAssignAnonIdsMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (AnonId)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type AssignAnonIds :: Pass
data AssignAnonIds a

-- We preserve the annotations from the @Parse@ pass
type family AnnAssignAnonIds ix where
  AnnAssignAnonIds "StructField" = ReparseInfo
  AnnAssignAnonIds "UnionField"  = ReparseInfo
  AnnAssignAnonIds "Typedef"     = ReparseInfo
  AnnAssignAnonIds "Function"    = ReparseInfo
  AnnAssignAnonIds _             = NoAnn

instance IsPass AssignAnonIds where
  type MacroBody  AssignAnonIds = UnparsedMacro
  type ExtBinding AssignAnonIds = Void
  type Ann ix     AssignAnonIds = AnnAssignAnonIds ix
  type Msg        AssignAnonIds = ImmediateAssignAnonIdsMsg

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ImmediateAssignAnonIdsMsg =
    -- | Skipped unused anonymous declaration
    --
    -- @clang@ will produce a warning for this ("declaration does not declare
    -- anything"); we issue a separate message here in case we skip over
    -- something that we shouldn't.
    AssignAnonIdsSkippedDecl AnonId
  deriving stock (Show, Generic)

instance PrettyForTrace ImmediateAssignAnonIdsMsg where
  prettyForTrace = \case
      AssignAnonIdsSkippedDecl anonId -> PP.hsep [
          "Skipped unused or unusable anonynous declaration"
        , prettyForTrace anonId
        ]

instance IsTrace Level ImmediateAssignAnonIdsMsg where
  getDefaultLogLevel = \case
      AssignAnonIdsSkippedDecl{} -> Debug -- clang already warned

  getSource  = const HsBindgen
  getTraceId = const "assign-anon-ids"
