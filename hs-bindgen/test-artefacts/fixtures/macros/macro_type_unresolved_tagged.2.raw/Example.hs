module Example
    ( Example.pTR_UNPARSABLE
    , Example.pTR_DOES_NOT_EXIST
    , Example.dOES_NOT_EXIST
    )
  where

{-| __C declaration:__ @macro PTR_UNPARSABLE@

    __defined at:__ @macros\/macro_type_unresolved_tagged.h 8:9@

    __exported by:__ @macros\/macro_type_unresolved_tagged.h@
-}
pTR_UNPARSABLE :: [String]
pTR_UNPARSABLE = ["struct", "Unparsable", "*"]

{-| __C declaration:__ @macro PTR_DOES_NOT_EXIST@

    __defined at:__ @macros\/macro_type_unresolved_tagged.h 10:9@

    __exported by:__ @macros\/macro_type_unresolved_tagged.h@
-}
pTR_DOES_NOT_EXIST :: [String]
pTR_DOES_NOT_EXIST = ["struct", "DoesNotExist", "*"]

{-| __C declaration:__ @macro DOES_NOT_EXIST@

    __defined at:__ @macros\/macro_type_unresolved_tagged.h 12:9@

    __exported by:__ @macros\/macro_type_unresolved_tagged.h@
-}
dOES_NOT_EXIST :: [String]
dOES_NOT_EXIST = ["struct", "DoesNotExist"]
