module Example
    ( Example.valueB
    , Example.valueA
    , Example.typeB
    , Example.typeA
    )
  where

{-| __C declaration:__ @macro ValueB@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 6:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
valueB :: [String]
valueB = ["ValueA"]

{-| __C declaration:__ @macro ValueA@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 7:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
valueA :: [String]
valueA = ["1"]

{-| __C declaration:__ @macro TypeB@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 9:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
typeB :: [String]
typeB = ["TypeA"]

{-| __C declaration:__ @macro TypeA@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 10:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
typeA :: [String]
typeA = ["int"]
