{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign.C as FC

{-| __C declaration:__ @A@

    __defined at:__ @redeclaration_identical.h:3:9@

    __exported by:__ @redeclaration_identical.h@
-}
a :: FC.CInt
a = (5 :: FC.CInt)
