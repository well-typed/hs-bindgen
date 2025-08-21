module Test.Common.HsBindgen.Trace (
  reportTrace
  ) where

import HsBindgen.Util.Tracer

import Text.SimplePrettyPrint (CtxDoc, (><))
import Text.SimplePrettyPrint qualified as PP

-- Seeing both, the pretty trace and the 'Show' instance greatly simplifies test
-- design and debugging.
reportTrace :: forall l a. (IsTrace l a, Show a) => a -> CtxDoc
reportTrace trace = PP.vcat $
              [ "[" >< ppTraceId >< "][pretty] " >< prettyForTrace trace
              , "[" >< ppTraceId >< "][show  ] " >< PP.showToCtxDoc trace
              ]
  where
    ppTraceId = PP.string $ unTraceId $ getTraceId trace
