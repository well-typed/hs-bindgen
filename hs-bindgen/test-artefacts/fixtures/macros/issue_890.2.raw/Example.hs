module Example
    ( Example.a
    , Example.b
    , Example.c
    , Example.d
    , Example.e
    )
  where

{-| __C declaration:__ @macro A@

    __defined at:__ @macros\/issue_890.h 1:9@

    __exported by:__ @macros\/issue_890.h@
-}
a :: [String]
a = ["0"]

{-| __C declaration:__ @macro B@

    __defined at:__ @macros\/issue_890.h 2:9@

    __exported by:__ @macros\/issue_890.h@
-}
b :: [String]
b = ["(", "x", ")", "(", "x", "+", "1", ")"]

{-| __C declaration:__ @macro C@

    __defined at:__ @macros\/issue_890.h 3:9@

    __exported by:__ @macros\/issue_890.h@
-}
c :: [String]
c = ["B", "(", "0", ")"]

{-| __C declaration:__ @macro D@

    __defined at:__ @macros\/issue_890.h 4:9@

    __exported by:__ @macros\/issue_890.h@
-}
d :: [String]
d = ["B", "(", "A", ")"]

{-| __C declaration:__ @macro E@

    __defined at:__ @macros\/issue_890.h 5:9@

    __exported by:__ @macros\/issue_890.h@
-}
e :: [String]
e = ["B", "(", "1", ")"]
