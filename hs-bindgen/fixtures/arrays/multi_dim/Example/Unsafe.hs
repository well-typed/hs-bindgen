{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <arrays/multi_dim.h>"
  , "signed int hs_bindgen_3389520bc7419af4 ("
  , "  signed int (*arg1)[3][4]"
  , ")"
  , "{"
  , "  return foo(*arg1);"
  , "}"
  , "signed int hs_bindgen_73e2db5ad5d807f7 ("
  , "  signed int const (*arg1)[3][4]"
  , ")"
  , "{"
  , "  return foo_const(*arg1);"
  , "}"
  , "signed int hs_bindgen_a28b81f0afc23eed ("
  , "  signed int (*arg1)[][2]"
  , ")"
  , "{"
  , "  return bar(*arg1);"
  , "}"
  , "signed int hs_bindgen_58337c492b64ae2c ("
  , "  signed int const (*arg1)[][2]"
  , ")"
  , "{"
  , "  return bar_const(*arg1);"
  , "}"
  , "signed int hs_bindgen_48876e6767cb5923 ("
  , "  matrix *arg1"
  , ")"
  , "{"
  , "  return baz(*arg1);"
  , "}"
  , "signed int hs_bindgen_13ce150055e8aa41 ("
  , "  matrix const *arg1"
  , ")"
  , "{"
  , "  return baz_const(*arg1);"
  , "}"
  , "signed int hs_bindgen_da5c432144bd5546 ("
  , "  triplets *arg1"
  , ")"
  , "{"
  , "  return quuz(*arg1);"
  , "}"
  , "signed int hs_bindgen_70be94a6fb59f547 ("
  , "  triplets const *arg1"
  , ")"
  , "{"
  , "  return quuz_const(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_3389520bc7419af4" hs_bindgen_3389520bc7419af4_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_foo@
hs_bindgen_3389520bc7419af4 ::
     RIP.Ptr ((CA.ConstantArray 3) ((CA.ConstantArray 4) RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_3389520bc7419af4 =
  RIP.fromFFIType hs_bindgen_3389520bc7419af4_base

{-| __C declaration:__ @foo@

    __defined at:__ @arrays\/multi_dim.h 4:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo ::
     RIP.Ptr ((CA.ConstantArray 3) ((CA.ConstantArray 4) RIP.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
foo = hs_bindgen_3389520bc7419af4

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_foo_const@
foreign import ccall unsafe "hs_bindgen_73e2db5ad5d807f7" hs_bindgen_73e2db5ad5d807f7_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_foo_const@
hs_bindgen_73e2db5ad5d807f7 ::
     PtrConst.PtrConst ((CA.ConstantArray 3) ((CA.ConstantArray 4) RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_73e2db5ad5d807f7 =
  RIP.fromFFIType hs_bindgen_73e2db5ad5d807f7_base

{-| __C declaration:__ @foo_const@

    __defined at:__ @arrays\/multi_dim.h 5:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo_const ::
     PtrConst.PtrConst ((CA.ConstantArray 3) ((CA.ConstantArray 4) RIP.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
foo_const = hs_bindgen_73e2db5ad5d807f7

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_a28b81f0afc23eed" hs_bindgen_a28b81f0afc23eed_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_bar@
hs_bindgen_a28b81f0afc23eed ::
     RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 2) RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_a28b81f0afc23eed =
  RIP.fromFFIType hs_bindgen_a28b81f0afc23eed_base

{-| __C declaration:__ @bar@

    __defined at:__ @arrays\/multi_dim.h 8:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar ::
     RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 2) RIP.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
bar = hs_bindgen_a28b81f0afc23eed

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_bar_const@
foreign import ccall unsafe "hs_bindgen_58337c492b64ae2c" hs_bindgen_58337c492b64ae2c_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_bar_const@
hs_bindgen_58337c492b64ae2c ::
     PtrConst.PtrConst (IA.IncompleteArray ((CA.ConstantArray 2) RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_58337c492b64ae2c =
  RIP.fromFFIType hs_bindgen_58337c492b64ae2c_base

{-| __C declaration:__ @bar_const@

    __defined at:__ @arrays\/multi_dim.h 9:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar_const ::
     PtrConst.PtrConst (IA.IncompleteArray ((CA.ConstantArray 2) RIP.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
bar_const = hs_bindgen_58337c492b64ae2c

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_baz@
foreign import ccall unsafe "hs_bindgen_48876e6767cb5923" hs_bindgen_48876e6767cb5923_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_baz@
hs_bindgen_48876e6767cb5923 ::
     RIP.Ptr Matrix
  -> IO RIP.CInt
hs_bindgen_48876e6767cb5923 =
  RIP.fromFFIType hs_bindgen_48876e6767cb5923_base

{-| __C declaration:__ @baz@

    __defined at:__ @arrays\/multi_dim.h 13:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz ::
     RIP.Ptr Matrix
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
baz = hs_bindgen_48876e6767cb5923

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_baz_const@
foreign import ccall unsafe "hs_bindgen_13ce150055e8aa41" hs_bindgen_13ce150055e8aa41_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_baz_const@
hs_bindgen_13ce150055e8aa41 ::
     PtrConst.PtrConst Matrix
  -> IO RIP.CInt
hs_bindgen_13ce150055e8aa41 =
  RIP.fromFFIType hs_bindgen_13ce150055e8aa41_base

{-| __C declaration:__ @baz_const@

    __defined at:__ @arrays\/multi_dim.h 14:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz_const ::
     PtrConst.PtrConst Matrix
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
baz_const = hs_bindgen_13ce150055e8aa41

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_quuz@
foreign import ccall unsafe "hs_bindgen_da5c432144bd5546" hs_bindgen_da5c432144bd5546_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_quuz@
hs_bindgen_da5c432144bd5546 ::
     RIP.Ptr Triplets
  -> IO RIP.CInt
hs_bindgen_da5c432144bd5546 =
  RIP.fromFFIType hs_bindgen_da5c432144bd5546_base

{-| __C declaration:__ @quuz@

    __defined at:__ @arrays\/multi_dim.h 18:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz ::
     RIP.Ptr Triplets
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
quuz = hs_bindgen_da5c432144bd5546

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_quuz_const@
foreign import ccall unsafe "hs_bindgen_70be94a6fb59f547" hs_bindgen_70be94a6fb59f547_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_quuz_const@
hs_bindgen_70be94a6fb59f547 ::
     PtrConst.PtrConst Triplets
  -> IO RIP.CInt
hs_bindgen_70be94a6fb59f547 =
  RIP.fromFFIType hs_bindgen_70be94a6fb59f547_base

{-| __C declaration:__ @quuz_const@

    __defined at:__ @arrays\/multi_dim.h 19:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz_const ::
     PtrConst.PtrConst Triplets
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
quuz_const = hs_bindgen_70be94a6fb59f547
