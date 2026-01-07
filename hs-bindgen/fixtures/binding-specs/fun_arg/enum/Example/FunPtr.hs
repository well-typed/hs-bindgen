{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/enum.h>"
  , "/* test_bindingspecsfun_argenum_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ba3da81519e4d7fd (void)) ("
  , "  enum E arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argenum_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fd34ef2d99a5ab0e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_bindingspecsfun_argenum_Example_get_baz */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_164a203415267b29 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &baz;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argenum_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_ba3da81519e4d7fd" hs_bindgen_ba3da81519e4d7fd ::
     IO (Ptr.FunPtr (E -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/enum.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
foo :: Ptr.FunPtr (E -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ba3da81519e4d7fd

-- __unique:__ @test_bindingspecsfun_argenum_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_fd34ef2d99a5ab0e" hs_bindgen_fd34ef2d99a5ab0e ::
     IO (Ptr.FunPtr (M1.A -> IO ()))

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/enum.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
bar :: Ptr.FunPtr (M1.A -> IO ())
bar =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fd34ef2d99a5ab0e

-- __unique:__ @test_bindingspecsfun_argenum_Example_get_baz@
foreign import ccall unsafe "hs_bindgen_164a203415267b29" hs_bindgen_164a203415267b29 ::
     IO (Ptr.FunPtr (M2.B -> IO ()))

{-# NOINLINE baz #-}
{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/enum.h:6:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
baz :: Ptr.FunPtr (M2.B -> IO ())
baz =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_164a203415267b29
