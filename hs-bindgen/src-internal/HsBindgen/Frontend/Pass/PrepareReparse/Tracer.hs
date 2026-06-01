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
  ) where

import HsBindgen.Frontend.Pass (IsPass (Msg))
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (PrepareReparse,
                                                      PrepareReparseMsg)
import HsBindgen.Util.Tracer (Contravariant (contramap), Tracer, traceWith,
                              withCallStack)

traceImmediate :: Tracer (Msg PrepareReparse) -> PrepareReparseMsg -> IO ()
traceImmediate tracer msg = traceWith (contramap withCallStack tracer) $
    withCallStack msg
