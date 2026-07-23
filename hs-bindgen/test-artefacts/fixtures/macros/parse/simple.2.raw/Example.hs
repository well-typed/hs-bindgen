module Example
    ( Example.iNNER_A
    , Example.oUTER_A
    )
  where

{-| __C declaration:__ @macro INNER_A@

    __defined at:__ @simple_inner.h 1:9@

    __exported by:__ @macros\/parse\/simple.h@
-}
iNNER_A :: [String]
iNNER_A = ["OUTER_A"]

{-| __C declaration:__ @macro OUTER_A@

    __defined at:__ @macros\/parse\/simple.h 7:9@

    __exported by:__ @macros\/parse\/simple.h@
-}
oUTER_A :: [String]
oUTER_A = ["1"]
