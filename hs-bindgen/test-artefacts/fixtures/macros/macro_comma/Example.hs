{-# LANGUAGE ExplicitForAll #-}

module Example
    ( Example.oBJ
    , Example.oBJ_NO_PARENS
    , Example.fUN
    , Example.fUN_THREE
    )
  where

import qualified HsBindgen.Runtime.Support as BG

{-| __C declaration:__ @macro OBJ@

    __defined at:__ @macros\/macro_comma.h 8:9@

    __exported by:__ @macros\/macro_comma.h@
-}
oBJ :: (BG.CInt, BG.CInt)
oBJ = ((1 :: BG.CInt), (2 :: BG.CInt))

{-| __C declaration:__ @macro OBJ_NO_PARENS@

    __defined at:__ @macros\/macro_comma.h 9:9@

    __exported by:__ @macros\/macro_comma.h@
-}
oBJ_NO_PARENS :: (BG.CInt, BG.CInt)
oBJ_NO_PARENS = ((1 :: BG.CInt), (2 :: BG.CInt))

{-| __C declaration:__ @macro FUN@

    __defined at:__ @macros\/macro_comma.h 10:9@

    __exported by:__ @macros\/macro_comma.h@
-}
fUN :: forall a0 b1. a0 -> b1 -> (a0, b1)
fUN = \x0 -> \y1 -> (x0, y1)

{-| __C declaration:__ @macro FUN_THREE@

    __defined at:__ @macros\/macro_comma.h 11:9@

    __exported by:__ @macros\/macro_comma.h@
-}
fUN_THREE :: forall a0 b1 c2. a0 -> b1 -> c2 -> (a0, b1, c2)
fUN_THREE = \x0 -> \y1 -> \z2 -> (x0, y1, z2)
