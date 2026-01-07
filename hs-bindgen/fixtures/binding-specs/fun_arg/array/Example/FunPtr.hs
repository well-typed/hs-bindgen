{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/array.h>"
  , "/* test_bindingspecsfun_argarray_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_85fa838e81394ea2 (void)) ("
  , "  signed int arg1[]"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argarray_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4d9d29bd7998caa1 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_bindingspecsfun_argarray_Example_get_baz */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e886ff88054de4f7 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &baz;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argarray_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_85fa838e81394ea2" hs_bindgen_85fa838e81394ea2 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/array.h:3:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
foo :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_85fa838e81394ea2

-- __unique:__ @test_bindingspecsfun_argarray_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_4d9d29bd7998caa1" hs_bindgen_4d9d29bd7998caa1 ::
     IO (Ptr.FunPtr (M1.A -> IO ()))

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/array.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
bar :: Ptr.FunPtr (M1.A -> IO ())
bar =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4d9d29bd7998caa1

-- __unique:__ @test_bindingspecsfun_argarray_Example_get_baz@
foreign import ccall unsafe "hs_bindgen_e886ff88054de4f7" hs_bindgen_e886ff88054de4f7 ::
     IO (Ptr.FunPtr (M2.B -> IO ()))

{-# NOINLINE baz #-}
{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/array.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/array.h@
-}
baz :: Ptr.FunPtr (M2.B -> IO ())
baz =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e886ff88054de4f7
