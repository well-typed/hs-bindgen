module Example
    ( Example.bOOL
    , Example.eXPORT
    )
  where

{-| __C declaration:__ @macro BOOL@

    __defined at:__ @macros\/reparse\/cref_attributes.h 2:9@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
bOOL :: [String]
bOOL = ["int"]

{-| __C declaration:__ @macro EXPORT@

    __defined at:__ @macros\/reparse\/cref_attributes.h 10:9@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
eXPORT :: [String]
eXPORT =
  ["[", "[", "gnu", "::", "visibility", "(", "\"default\"", ")", "]", "]"]
