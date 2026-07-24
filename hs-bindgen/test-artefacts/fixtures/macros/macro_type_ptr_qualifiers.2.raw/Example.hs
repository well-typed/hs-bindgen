module Example
    ( Example.ptrToVoid
    , Example.ptrToConstVoidL
    , Example.ptrToConstVoidR
    , Example.ptrToConstIntL
    , Example.ptrToConstIntR
    , Example.constPtrToInt
    )
  where

{-| __C declaration:__ @macro PtrToVoid@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 2:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
ptrToVoid :: [String]
ptrToVoid = ["void", "*"]

{-| __C declaration:__ @macro PtrToConstVoidL@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 5:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
ptrToConstVoidL :: [String]
ptrToConstVoidL = ["const", "void", "*"]

{-| __C declaration:__ @macro PtrToConstVoidR@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 8:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
ptrToConstVoidR :: [String]
ptrToConstVoidR = ["void", "const", "*"]

{-| __C declaration:__ @macro PtrToConstIntL@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 11:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
ptrToConstIntL :: [String]
ptrToConstIntL = ["const", "int", "*"]

{-| __C declaration:__ @macro PtrToConstIntR@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 14:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
ptrToConstIntR :: [String]
ptrToConstIntR = ["int", "const", "*"]

{-| __C declaration:__ @macro ConstPtrToInt@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 17:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
constPtrToInt :: [String]
constPtrToInt = ["int", "*", "const"]
