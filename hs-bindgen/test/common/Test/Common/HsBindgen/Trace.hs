module Test.Common.HsBindgen.Trace (
  reportTrace
  ) where

import Text.SimplePrettyPrint (CtxDoc, (><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Util.Tracer

-- Seeing both, the pretty trace and the 'Show' instance greatly simplifies test
-- design and debugging.
reportTrace :: forall a l. (IsTrace l a, Show a) => a -> CtxDoc
reportTrace trace = PP.vcat $
              [ "[" >< ppTraceId >< "][pretty] " >< prettyForTrace trace
              , "[" >< ppTraceId >< "][show  ] " >< PP.show trace
              , ""
              ]
  where
    ppTraceId = PP.string $ unTraceId $ getTraceId trace
