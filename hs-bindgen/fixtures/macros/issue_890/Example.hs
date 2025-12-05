{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified C.Expr.HostPlatform as C
import qualified Foreign.C as FC

{-| __C declaration:__ @A@

    __defined at:__ @macros\/issue_890.h:1:9@

    __exported by:__ @macros\/issue_890.h@
-}
a :: FC.CInt
a = (0 :: FC.CInt)

{-| __C declaration:__ @B@

    __defined at:__ @macros\/issue_890.h:2:9@

    __exported by:__ @macros\/issue_890.h@
-}
b :: forall a0. (C.Add a0) FC.CInt => a0 -> (C.AddRes a0) FC.CInt
b = \x0 -> (C.+) x0 (1 :: FC.CInt)

{-| __C declaration:__ @C@

    __defined at:__ @macros\/issue_890.h:3:9@

    __exported by:__ @macros\/issue_890.h@
-}
c :: FC.CInt
c = b (0 :: FC.CInt)

{-| __C declaration:__ @D@

    __defined at:__ @macros\/issue_890.h:4:9@

    __exported by:__ @macros\/issue_890.h@
-}
d :: FC.CInt
d = b a

{-| __C declaration:__ @E@

    __defined at:__ @macros\/issue_890.h:5:9@

    __exported by:__ @macros\/issue_890.h@
-}
e :: FC.CInt
e = b (1 :: FC.CInt)
