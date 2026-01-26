{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified GHC.Word
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/macro/enum.h>"
  , "void hs_bindgen_0e6b98e93cad73ef ("
  , "  enum MyEnum arg1"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_1c6de1b89014dc52 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_a8e579f3b5035c03 ("
  , "  B arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_0e6b98e93cad73ef" hs_bindgen_0e6b98e93cad73ef_base ::
     GHC.Word.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_foo@
hs_bindgen_0e6b98e93cad73ef ::
     MyEnum
  -> IO ()
hs_bindgen_0e6b98e93cad73ef =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_0e6b98e93cad73ef_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
foo ::
     MyEnum
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_0e6b98e93cad73ef

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_1c6de1b89014dc52" hs_bindgen_1c6de1b89014dc52_base ::
     GHC.Word.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooA@
hs_bindgen_1c6de1b89014dc52 ::
     A
  -> IO ()
hs_bindgen_1c6de1b89014dc52 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1c6de1b89014dc52_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_1c6de1b89014dc52

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_a8e579f3b5035c03" hs_bindgen_a8e579f3b5035c03_base ::
     GHC.Word.Word32
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_Unsafe_fooB@
hs_bindgen_a8e579f3b5035c03 ::
     B
  -> IO ()
hs_bindgen_a8e579f3b5035c03 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a8e579f3b5035c03_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_a8e579f3b5035c03
