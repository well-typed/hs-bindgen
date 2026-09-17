module Example
    ( Example.valueB
    , Example.valueA
    , Example.typeB
    , Example.typeA
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro

{-| __C declaration:__ @macro ValueB@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 6:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
valueB :: Macro.Raw String
valueB = Macro.objectLike "ValueB" ["ValueA"]

{-| __C declaration:__ @macro ValueA@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 7:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
valueA :: Macro.Raw String
valueA = Macro.objectLike "ValueA" ["1"]

{-| __C declaration:__ @macro TypeB@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 9:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
typeB :: Macro.Raw String
typeB = Macro.objectLike "TypeB" ["TypeA"]

{-| __C declaration:__ @macro TypeA@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 10:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
typeA :: Macro.Raw String
typeA = Macro.objectLike "TypeA" ["int"]
