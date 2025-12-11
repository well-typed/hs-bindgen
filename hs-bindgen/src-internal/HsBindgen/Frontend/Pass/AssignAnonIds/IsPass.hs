module HsBindgen.Frontend.Pass.AssignAnonIds.IsPass (
    AssignAnonIds
  , AssignAnonIdsMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.Tracer

type AssignAnonIds :: Pass
data AssignAnonIds a deriving anyclass ValidPass

type family AnnAssignAnonIds ix where
   AnnAssignAnonIds _ = NoAnn

instance IsPass AssignAnonIds where
  type Id           AssignAnonIds = C.DeclId AssignAnonIds
  type FieldName    AssignAnonIds = C.ScopedName
  type ArgumentName AssignAnonIds = Maybe C.ScopedName
  type MacroBody    AssignAnonIds = CheckedMacro AssignAnonIds
  type ExtBinding   AssignAnonIds = Void
  type Ann ix       AssignAnonIds = AnnAssignAnonIds ix
  type Msg          AssignAnonIds = AssignAnonIdsMsg

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data AssignAnonIdsMsg =
    -- | Skipped unused anonymous declaration
    --
    -- @clang@ will produce a warning for this ("declaration does not declare
    -- anything"); we issue a separate message here in case we skip over
    -- something that we shouldn't.
    AssignAnonIdsSkipped (DeclInfo Parse)
  deriving stock (Show)

instance PrettyForTrace AssignAnonIdsMsg where
  prettyForTrace = \case
      AssignAnonIdsSkipped info -> PP.hsep [
          "Skipped unused or unusable anonynous declaration"
        , prettyForTrace info
        ]

instance IsTrace Level AssignAnonIdsMsg where
  getDefaultLogLevel = \case
      AssignAnonIdsSkipped{} -> Debug -- clang already warned
  getSource  = const HsBindgen
  getTraceId = const "name-anon"

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassHaskellId Parse AssignAnonIds where
  coercePassHaskellId _ = id
