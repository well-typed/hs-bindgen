{-# LANGUAGE ExplicitForAll #-}

module Example
    ( Example.v
    )
  where

import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro V@

    __defined at:__ @macros\/redeclaration\/main_header.h 5:9@

    __exported by:__ @macros\/redeclaration\/main_header.h@
-}
v :: BG.CInt
v = (3 :: BG.CInt)
