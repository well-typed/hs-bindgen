{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}

module Example
    ( Example.f
    )
  where

import qualified C.Expr.HostPlatform

{-| __C declaration:__ @macro F@

    __defined at:__ @macros\/object_like_as_function_like.h 10:9@

    __exported by:__ @macros\/object_like_as_function_like.h@
-}
f :: forall a0 b1. C.Expr.HostPlatform.Add a0 b1 => a0 -> b1 -> C.Expr.HostPlatform.AddRes a0 b1
f = \x0 -> \y1 -> (C.Expr.HostPlatform.+) x0 y1
