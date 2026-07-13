{-# LANGUAGE ExplicitForAll #-}

module Example
    ( Example.oUTER_A
    , Example.iNNER_A
    )
  where

import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro OUTER_A@

    __defined at:__ @macros\/parse\/simple.h 7:9@

    __exported by:__ @macros\/parse\/simple.h@
-}
oUTER_A :: BG.CInt
oUTER_A = (1 :: BG.CInt)

{-| __C declaration:__ @macro INNER_A@

    __defined at:__ @simple_inner.h 1:9@

    __exported by:__ @macros\/parse\/simple.h@
-}
iNNER_A :: BG.CInt
iNNER_A = oUTER_A
