{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.x
    , Example.Global.n
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <declarations/redeclaration.h>"
  , "/* test_declarationsredeclaration_Example_get_x */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_6f47e5cbb92690b9 (void)"
  , "{"
  , "  return &x;"
  , "}"
  , "/* test_declarationsredeclaration_Example_get_n */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_8afd78f6e3766c89 (void)"
  , "{"
  , "  return &n;"
  , "}"
  ]))

-- __unique:__ @test_declarationsredeclaration_Example_get_x@
foreign import ccall unsafe "hs_bindgen_6f47e5cbb92690b9" hs_bindgen_6f47e5cbb92690b9_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_declarationsredeclaration_Example_get_x@
hs_bindgen_6f47e5cbb92690b9 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_6f47e5cbb92690b9 =
  RIP.fromFFIType hs_bindgen_6f47e5cbb92690b9_base

{-# NOINLINE x #-}
{-| __C declaration:__ @x@

    __defined at:__ @declarations\/redeclaration.h 11:5@

    __exported by:__ @declarations\/redeclaration.h@
-}
x :: RIP.Ptr RIP.CInt
x = RIP.unsafePerformIO hs_bindgen_6f47e5cbb92690b9

-- __unique:__ @test_declarationsredeclaration_Example_get_n@
foreign import ccall unsafe "hs_bindgen_8afd78f6e3766c89" hs_bindgen_8afd78f6e3766c89_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_declarationsredeclaration_Example_get_n@
hs_bindgen_8afd78f6e3766c89 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_8afd78f6e3766c89 =
  RIP.fromFFIType hs_bindgen_8afd78f6e3766c89_base

{-# NOINLINE n #-}
{-| __C declaration:__ @n@

    __defined at:__ @declarations\/redeclaration.h 15:12@

    __exported by:__ @declarations\/redeclaration.h@
-}
n :: RIP.Ptr RIP.CInt
n = RIP.unsafePerformIO hs_bindgen_8afd78f6e3766c89
