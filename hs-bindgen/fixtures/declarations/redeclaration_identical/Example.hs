{-# LANGUAGE ExplicitForAll #-}

module Example where

import qualified HsBindgen.Runtime.Internal.Prelude as RIP

{-| __C declaration:__ @A@

    __defined at:__ @declarations\/redeclaration_identical.h 3:9@

    __exported by:__ @declarations\/redeclaration_identical.h@
-}
a :: RIP.CInt
a = (5 :: RIP.CInt)
