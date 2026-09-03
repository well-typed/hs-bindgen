module Example
    ( Example.ptrToVoid
    , Example.ptrToConstVoidL
    , Example.ptrToConstVoidR
    , Example.ptrToConstIntL
    , Example.ptrToConstIntR
    , Example.constPtrToInt
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro PtrToVoid@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 2:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
ptrToVoid :: Macro.Raw BG.Text
ptrToVoid =
  Macro.objectLike "PtrToVoid" ["void", "*"]

{-| __C declaration:__ @macro PtrToConstVoidL@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 5:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
ptrToConstVoidL :: Macro.Raw BG.Text
ptrToConstVoidL =
  Macro.objectLike "PtrToConstVoidL" ["const", "void", "*"]

{-| __C declaration:__ @macro PtrToConstVoidR@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 8:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
ptrToConstVoidR :: Macro.Raw BG.Text
ptrToConstVoidR =
  Macro.objectLike "PtrToConstVoidR" ["void", "const", "*"]

{-| __C declaration:__ @macro PtrToConstIntL@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 11:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
ptrToConstIntL :: Macro.Raw BG.Text
ptrToConstIntL =
  Macro.objectLike "PtrToConstIntL" ["const", "int", "*"]

{-| __C declaration:__ @macro PtrToConstIntR@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 14:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
ptrToConstIntR :: Macro.Raw BG.Text
ptrToConstIntR =
  Macro.objectLike "PtrToConstIntR" ["int", "const", "*"]

{-| __C declaration:__ @macro ConstPtrToInt@

    __defined at:__ @macros\/macro_type_ptr_qualifiers.h 17:9@

    __exported by:__ @macros\/macro_type_ptr_qualifiers.h@
-}
constPtrToInt :: Macro.Raw BG.Text
constPtrToInt =
  Macro.objectLike "ConstPtrToInt" ["int", "*", "const"]
