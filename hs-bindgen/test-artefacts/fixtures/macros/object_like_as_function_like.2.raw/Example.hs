module Example
    ( Example.f
    , Example.g
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro

{-| __C declaration:__ @macro F@

    __defined at:__ @macros\/object_like_as_function_like.h 10:9@

    __exported by:__ @macros\/object_like_as_function_like.h@
-}
f :: Macro.Raw String
f = Macro.functionLike "F" ["x", "y"] ["x", "+", "y"]

{-| __C declaration:__ @macro G@

    __defined at:__ @macros\/object_like_as_function_like.h 11:9@

    __exported by:__ @macros\/object_like_as_function_like.h@
-}
g :: Macro.Raw String
g =
  Macro.objectLike "G" ["(", "x", ",", "y", ")", "x", "+", "y"]
