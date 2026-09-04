module Example
    ( Example.iNNER_A
    , Example.oUTER_A
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro INNER_A@

    __defined at:__ @simple_inner.h 1:9@

    __exported by:__ @macros\/parse\/simple.h@
-}
iNNER_A :: Macro.Raw BG.Text
iNNER_A = Macro.objectLike "INNER_A" ["OUTER_A"]

{-| __C declaration:__ @macro OUTER_A@

    __defined at:__ @macros\/parse\/simple.h 7:9@

    __exported by:__ @macros\/parse\/simple.h@
-}
oUTER_A :: Macro.Raw BG.Text
oUTER_A = Macro.objectLike "OUTER_A" ["1"]
