{-# LANGUAGE ExplicitForAll #-}

module Example
    ( Example.g
    )
  where

import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro G@

    __defined at:__ @noguard_inner.h 2:9@

    __exported by:__ @macros\/redeclaration\/noguard.h@
-}
g :: BG.CInt
g = (9 :: BG.CInt)
