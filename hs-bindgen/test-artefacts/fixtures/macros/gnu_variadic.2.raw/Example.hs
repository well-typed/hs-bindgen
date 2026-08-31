module Example
    ( Example.c99_VARIADIC
    , Example.gNU_VARIADIC
    , Example.gNU_VARIADIC_ONLY
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro C99_VARIADIC@

    __defined at:__ @macros\/gnu_variadic.h 9:9@

    __exported by:__ @macros\/gnu_variadic.h@
-}
c99_VARIADIC :: Macro.Raw BG.Text
c99_VARIADIC =
  Macro.variadicFunctionLike "C99_VARIADIC" ["fmt"] ["fmt"]

{-| __C declaration:__ @macro GNU_VARIADIC@

    __defined at:__ @macros\/gnu_variadic.h 11:9@

    __exported by:__ @macros\/gnu_variadic.h@
-}
gNU_VARIADIC :: Macro.Raw BG.Text
gNU_VARIADIC =
  Macro.namedVariadicFunctionLike "GNU_VARIADIC" ["fmt"] "args" ["args"]

{-| __C declaration:__ @macro GNU_VARIADIC_ONLY@

    __defined at:__ @macros\/gnu_variadic.h 13:9@

    __exported by:__ @macros\/gnu_variadic.h@
-}
gNU_VARIADIC_ONLY :: Macro.Raw BG.Text
gNU_VARIADIC_ONLY =
  Macro.namedVariadicFunctionLike "GNU_VARIADIC_ONLY" [] "args" ["args"]
