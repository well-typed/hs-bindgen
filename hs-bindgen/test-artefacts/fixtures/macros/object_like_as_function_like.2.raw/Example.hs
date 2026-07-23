module Example
    ( Example.f
    , Example.g
    )
  where

{-| __C declaration:__ @macro F@

    __defined at:__ @macros\/object_like_as_function_like.h 10:9@

    __exported by:__ @macros\/object_like_as_function_like.h@
-}
f :: [String]
f = ["(", "x", ",", "y", ")", "x", "+", "y"]

{-| __C declaration:__ @macro G@

    __defined at:__ @macros\/object_like_as_function_like.h 11:9@

    __exported by:__ @macros\/object_like_as_function_like.h@
-}
g :: [String]
g = ["(", "x", ",", "y", ")", "x", "+", "y"]
