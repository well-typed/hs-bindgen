{-# LANGUAGE ExplicitForAll #-}

module Example
    ( Example.a
    )
  where

import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro A@

    __defined at:__ @macros\/redeclaration\/identical_value.h 3:9@

    __exported by:__ @macros\/redeclaration\/identical_value.h@
-}
a :: BG.CInt
a = (5 :: BG.CInt)
