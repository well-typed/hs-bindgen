{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}

module Example
    ( Example.iD_INT
    , Example.aDD_SIZEOF
    , Example.fST_CONST
    )
  where

import qualified C.Expr.HostPlatform

{-| __C declaration:__ @macro ID_INT@

    __defined at:__ @macros\/keyword_params.h 9:9@

    __exported by:__ @macros\/keyword_params.h@
-}
iD_INT :: forall a0. a0 -> a0
iD_INT = \int0 -> int0

{-| __C declaration:__ @macro ADD_SIZEOF@

    __defined at:__ @macros\/keyword_params.h 10:9@

    __exported by:__ @macros\/keyword_params.h@
-}
aDD_SIZEOF :: forall a0 b1. C.Expr.HostPlatform.Add a0 b1 => a0 -> b1 -> C.Expr.HostPlatform.AddRes a0 b1
aDD_SIZEOF =
  \sizeof0 -> \x1 -> (C.Expr.HostPlatform.+) sizeof0 x1

{-| __C declaration:__ @macro FST_CONST@

    __defined at:__ @macros\/keyword_params.h 11:9@

    __exported by:__ @macros\/keyword_params.h@
-}
fST_CONST :: forall a0 b1. a0 -> b1 -> a0
fST_CONST = \const0 -> \volatile1 -> const0
