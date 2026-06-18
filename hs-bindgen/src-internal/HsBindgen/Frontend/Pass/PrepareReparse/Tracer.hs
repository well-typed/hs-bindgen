-- | Tracing
--
-- This module is intended to be imported unqualified. It is also intended to
-- only be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse"
-- module hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.Tracer
--
module HsBindgen.Frontend.Pass.PrepareReparse.Tracer (
    traceImmediate
  , traceBelated
  ) where

import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (PrepareReparse)
import HsBindgen.IR.Pass (AMsg, PassMsg (Msg))
import HsBindgen.Util.Tracer (Tracer, traceWith, withCallStack)

traceImmediate :: Tracer (Msg PrepareReparse) -> Msg PrepareReparse -> IO ()
traceImmediate tracer msg = traceWith tracer $ withCallStack msg

traceBelated :: Tracer (Msg PrepareReparse) -> AMsg PrepareReparse -> IO ()
traceBelated tracer amsg = traceWith tracer amsg
