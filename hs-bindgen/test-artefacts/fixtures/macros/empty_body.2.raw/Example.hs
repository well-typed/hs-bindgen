module Example
    ( Example.eMPTY_OBJECT
    , Example.eMPTY_FUNCTION
    , Example.eMPTY_FUNCTION_PARAMS
    , Example.eMPTY_FUNCTION_VARIADIC
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro

{-| __C declaration:__ @macro EMPTY_OBJECT@

    __defined at:__ @macros\/empty_body.h 8:9@

    __exported by:__ @macros\/empty_body.h@
-}
eMPTY_OBJECT :: Macro.Raw String
eMPTY_OBJECT = Macro.objectLike "EMPTY_OBJECT" []

{-| __C declaration:__ @macro EMPTY_FUNCTION@

    __defined at:__ @macros\/empty_body.h 9:9@

    __exported by:__ @macros\/empty_body.h@
-}
eMPTY_FUNCTION :: Macro.Raw String
eMPTY_FUNCTION =
  Macro.functionLike "EMPTY_FUNCTION" [] []

{-| __C declaration:__ @macro EMPTY_FUNCTION_PARAMS@

    __defined at:__ @macros\/empty_body.h 10:9@

    __exported by:__ @macros\/empty_body.h@
-}
eMPTY_FUNCTION_PARAMS :: Macro.Raw String
eMPTY_FUNCTION_PARAMS =
  Macro.functionLike "EMPTY_FUNCTION_PARAMS" ["x", "y"] []

{-| __C declaration:__ @macro EMPTY_FUNCTION_VARIADIC@

    __defined at:__ @macros\/empty_body.h 11:9@

    __exported by:__ @macros\/empty_body.h@
-}
eMPTY_FUNCTION_VARIADIC :: Macro.Raw String
eMPTY_FUNCTION_VARIADIC =
  Macro.variadic "EMPTY_FUNCTION_VARIADIC" ["x"] []
