-- | Delayed trace messages for the @MangleNames@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.MangleNames.IsPass.Msg (
    DelayedMangleNamesMsg(..)
  ) where

import GHC.Generics (Generic)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  DelayedMsg
-------------------------------------------------------------------------------}

data DelayedMangleNamesMsg =
    -- | The Haskell name is not mangled from the C name: it has a suffix, or
    --   it is the name of the squashed typedef
    MangleNamesAssignedName Hs.SomeName
    -- | The Haskell name is the one a binding specification gives to the
    --   squashed typedef
  | MangleNamesReusedAssignedName Hs.SomeName
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedMangleNamesMsg where
  prettyForTrace = \case
      MangleNamesAssignedName newName -> PP.hsep [
          "Assigned name"
        , PP.text newName.text
        ]
      MangleNamesReusedAssignedName newName -> PP.hsep [
          "Reused assigned name"
        , PP.text newName.text
        ]

instance IsTrace Level DelayedMangleNamesMsg where
  getDefaultLogLevel = \case
      MangleNamesAssignedName{}       -> Info
      MangleNamesReusedAssignedName{} -> Info
  getSource  = const HsBindgen
  getTraceId = const "mangle-names"
