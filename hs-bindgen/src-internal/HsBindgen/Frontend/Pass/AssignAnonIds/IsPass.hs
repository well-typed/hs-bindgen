module HsBindgen.Frontend.Pass.AssignAnonIds.IsPass (
    AssignAnonIds
  , ImmediateAssignAnonIdsMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type AssignAnonIds :: Pass
data AssignAnonIds a

-- We preserve the annotations from the @Parse@ pass
type family AnnAssignAnonIds ix where
  AnnAssignAnonIds "Function"    = ReparseInfo Tokens
  AnnAssignAnonIds "Global"      = ReparseInfo Tokens
  AnnAssignAnonIds "StructField" = ReparseInfo Tokens
  AnnAssignAnonIds "Typedef"     = ReparseInfo Tokens
  AnnAssignAnonIds "UnionField"  = ReparseInfo Tokens
  AnnAssignAnonIds _             = NoAnn

instance IsPass AssignAnonIds

instance PassId AssignAnonIds

instance PassScopedName AssignAnonIds

instance PassMacro AssignAnonIds where
  type MacroBody AssignAnonIds = Macro.Unresolved

instance PassExtBinding AssignAnonIds

instance PassCommentDecl AssignAnonIds

instance PassAnn AssignAnonIds where
  type Ann ix AssignAnonIds = AnnAssignAnonIds ix

instance PassMsg AssignAnonIds where
  type Msg AssignAnonIds = ImmediateAssignAnonIdsMsg

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ImmediateAssignAnonIdsMsg =
    -- | Skipped unused anonymous declaration
    --
    -- @clang@ will produce a warning for this ("declaration does not declare
    -- anything"); we issue a separate message here in case we skip over
    -- something that we shouldn't.
    AssignAnonIdsSkippedDecl C.AnonId
  deriving stock (Show, Generic)

instance PrettyForTrace ImmediateAssignAnonIdsMsg where
  prettyForTrace = \case
      AssignAnonIdsSkippedDecl anonId -> PP.hsep [
          "Skipped unused or unusable anonymous declaration"
        , prettyForTrace anonId
        ]

instance IsTrace Level ImmediateAssignAnonIdsMsg where
  getDefaultLogLevel = \case
      AssignAnonIdsSkippedDecl{} -> Debug -- clang already warned

  getSource  = const HsBindgen
  getTraceId = const "assign-anon-ids"
