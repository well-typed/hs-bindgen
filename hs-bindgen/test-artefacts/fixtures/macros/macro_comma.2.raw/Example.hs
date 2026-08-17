module Example
    ( Example.oBJ
    , Example.oBJ_NO_PARENS
    , Example.fUN
    , Example.fUN_THREE
    , Example.aRITH
    )
  where

{-| __C declaration:__ @macro OBJ@

    __defined at:__ @macros\/macro_comma.h 8:9@

    __exported by:__ @macros\/macro_comma.h@
-}
oBJ :: [String]
oBJ = ["(", "1", ",", "2", ")"]

{-| __C declaration:__ @macro OBJ_NO_PARENS@

    __defined at:__ @macros\/macro_comma.h 9:9@

    __exported by:__ @macros\/macro_comma.h@
-}
oBJ_NO_PARENS :: [String]
oBJ_NO_PARENS = ["1", ",", "2"]

{-| __C declaration:__ @macro FUN@

    __defined at:__ @macros\/macro_comma.h 10:9@

    __exported by:__ @macros\/macro_comma.h@
-}
fUN :: [String]
fUN =
  ["(", "x", ",", "y", ")", "(", "x", ",", "y", ")"]

{-| __C declaration:__ @macro FUN_THREE@

    __defined at:__ @macros\/macro_comma.h 11:9@

    __exported by:__ @macros\/macro_comma.h@
-}
fUN_THREE :: [String]
fUN_THREE =
  [ "("
  , "x"
  , ","
  , "y"
  , ","
  , "z"
  , ")"
  , "("
  , "("
  , "x"
  , ")"
  , ","
  , "("
  , "y"
  , ")"
  , ","
  , "("
  , "z"
  , ")"
  , ")"
  ]

{-| __C declaration:__ @macro ARITH@

    __defined at:__ @macros\/macro_comma.h 15:9@

    __exported by:__ @macros\/macro_comma.h@
-}
aRITH :: [String]
aRITH = ["(", "(", "1", ",", "2", ")", "+", "3", ")"]
