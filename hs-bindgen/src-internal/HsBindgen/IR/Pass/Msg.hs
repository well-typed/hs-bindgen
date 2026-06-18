-- | Trace messages
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.Pass" should be used.
--
-- Intended for unqualified import.
--
-- > import HsBindgen.IR.Pass.Msg
module HsBindgen.IR.Pass.Msg (
    -- * Associated type families
    PassMsg(..)
  , AMsg
    -- * Defaults
  , NoMsg
  ) where

import HsBindgen.Imports
import HsBindgen.IR.Pass.Definition
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Associated type families
-------------------------------------------------------------------------------}

-- | Trace messages vary across passes
class PassMsg (p :: Pass) where

  -- | Trace messages possibly emitted by a pass
  type Msg p :: Star
  type Msg p = NoMsg Level

-- | Trace messages with call-stack annotations
type AMsg :: Pass -> Star
type AMsg p = WithCallStack (Msg p)

{-------------------------------------------------------------------------------
  Defaults
-------------------------------------------------------------------------------}

data NoMsg lvl
  deriving stock (Eq, Ord, Show)

instance IsTrace lvl (NoMsg lvl) where
  getDefaultLogLevel msg = case msg of {}
  getSource          msg = case msg of {}
  getTraceId         msg = case msg of {}

instance PrettyForTrace (NoMsg lvl) where
  prettyForTrace msg = case msg of {}
