{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}

module Example
    ( Example.a
    , Example.b
    , Example.c
    , Example.d
    , Example.e
    )
  where

import qualified C.Expr.HostPlatform
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

{-| __C declaration:__ @macro A@

    __defined at:__ @macros\/issue_890.h 1:9@

    __exported by:__ @macros\/issue_890.h@
-}
a :: RIP.CInt
a = (0 :: RIP.CInt)

{-| __C declaration:__ @macro B@

    __defined at:__ @macros\/issue_890.h 2:9@

    __exported by:__ @macros\/issue_890.h@
-}
b :: forall a0. (C.Expr.HostPlatform.Add a0) RIP.CInt => a0 -> (C.Expr.HostPlatform.AddRes a0) RIP.CInt
b = \x0 -> (C.Expr.HostPlatform.+) x0 (1 :: RIP.CInt)

{-| __C declaration:__ @macro C@

    __defined at:__ @macros\/issue_890.h 3:9@

    __exported by:__ @macros\/issue_890.h@
-}
c :: RIP.CInt
c = b (0 :: RIP.CInt)

{-| __C declaration:__ @macro D@

    __defined at:__ @macros\/issue_890.h 4:9@

    __exported by:__ @macros\/issue_890.h@
-}
d :: RIP.CInt
d = b a

{-| __C declaration:__ @macro E@

    __defined at:__ @macros\/issue_890.h 5:9@

    __exported by:__ @macros\/issue_890.h@
-}
e :: RIP.CInt
e = b (1 :: RIP.CInt)
