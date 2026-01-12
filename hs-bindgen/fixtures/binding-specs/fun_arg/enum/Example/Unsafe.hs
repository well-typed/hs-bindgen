{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Prelude
import qualified M
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/enum.h>"
  , "void hs_bindgen_7a447ec2e0ff6918 ("
  , "  enum MyEnum arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_edec6f63d3bb95f2 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_44d9554b39200196 ("
  , "  B arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_425db0e548343d0e ("
  , "  C arg1"
  , ")"
  , "{"
  , "  fooC(arg1);"
  , "}"
  , "void hs_bindgen_a9d2a72404176a97 ("
  , "  D arg1"
  , ")"
  , "{"
  , "  fooD(arg1);"
  , "}"
  , "void hs_bindgen_293ee0f23fd07e98 ("
  , "  E arg1"
  , ")"
  , "{"
  , "  fooE(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argenum_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_7a447ec2e0ff6918" hs_bindgen_7a447ec2e0ff6918 ::
     MyEnum
  -> IO ()

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
foo ::
     MyEnum
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_7a447ec2e0ff6918

-- __unique:__ @test_bindingspecsfun_argenum_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_edec6f63d3bb95f2" hs_bindgen_edec6f63d3bb95f2 ::
     A
  -> IO ()

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_edec6f63d3bb95f2

-- __unique:__ @test_bindingspecsfun_argenum_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_44d9554b39200196" hs_bindgen_44d9554b39200196 ::
     B
  -> IO ()

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_44d9554b39200196

-- __unique:__ @test_bindingspecsfun_argenum_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_425db0e548343d0e" hs_bindgen_425db0e548343d0e ::
     M.C
  -> IO ()

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_425db0e548343d0e

-- __unique:__ @test_bindingspecsfun_argenum_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_a9d2a72404176a97" hs_bindgen_a9d2a72404176a97 ::
     M.D
  -> IO ()

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_a9d2a72404176a97

-- __unique:__ @test_bindingspecsfun_argenum_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_293ee0f23fd07e98" hs_bindgen_293ee0f23fd07e98 ::
     E
  -> IO ()

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/enum.h 25:6@

    __exported by:__ @binding-specs\/fun_arg\/enum.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_293ee0f23fd07e98
