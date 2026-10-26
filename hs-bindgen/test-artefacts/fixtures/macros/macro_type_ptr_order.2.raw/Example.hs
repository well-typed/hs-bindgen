{-# LANGUAGE NoImplicitPrelude #-}

module Example
    ( Example.t
    , Example.p
    , Example.z_PTR
    , Example.a_T
    )
  where

import qualified HsBindgen.Runtime.Macro as Macro
import Prelude (String)

{-| __C declaration:__ @macro T@

    __defined at:__ @macros\/macro_type_ptr_order.h 2:9@

    __exported by:__ @macros\/macro_type_ptr_order.h@
-}
t :: Macro.Raw String
t = Macro.objectLike "T" ["int"]

{-| __C declaration:__ @macro P@

    __defined at:__ @macros\/macro_type_ptr_order.h 3:9@

    __exported by:__ @macros\/macro_type_ptr_order.h@
-}
p :: Macro.Raw String
p = Macro.objectLike "P" ["T", "*"]

{-| __C declaration:__ @macro Z_PTR@

    __defined at:__ @macros\/macro_type_ptr_order.h 6:9@

    __exported by:__ @macros\/macro_type_ptr_order.h@
-}
z_PTR :: Macro.Raw String
z_PTR = Macro.objectLike "Z_PTR" ["A_T", "*"]

{-| __C declaration:__ @macro A_T@

    __defined at:__ @macros\/macro_type_ptr_order.h 7:9@

    __exported by:__ @macros\/macro_type_ptr_order.h@
-}
a_T :: Macro.Raw String
a_T = Macro.objectLike "A_T" ["int"]
