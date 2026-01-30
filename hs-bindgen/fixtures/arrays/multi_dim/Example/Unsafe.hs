{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.PtrConst
import Data.Void (Void)
import Example
import Prelude (IO)

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
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_foo@
hs_bindgen_3389520bc7419af4 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt))
  -> IO FC.CInt
hs_bindgen_3389520bc7419af4 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3389520bc7419af4_base

{-| __C declaration:__ @foo@

    __defined at:__ @arrays\/multi_dim.h 4:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
foo = hs_bindgen_3389520bc7419af4

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_foo_const@
foreign import ccall unsafe "hs_bindgen_73e2db5ad5d807f7" hs_bindgen_73e2db5ad5d807f7_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_foo_const@
hs_bindgen_73e2db5ad5d807f7 ::
     HsBindgen.Runtime.PtrConst.PtrConst ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt))
  -> IO FC.CInt
hs_bindgen_73e2db5ad5d807f7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_73e2db5ad5d807f7_base

{-| __C declaration:__ @foo_const@

    __defined at:__ @arrays\/multi_dim.h 5:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo_const ::
     HsBindgen.Runtime.PtrConst.PtrConst ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
foo_const = hs_bindgen_73e2db5ad5d807f7

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_a28b81f0afc23eed" hs_bindgen_a28b81f0afc23eed_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_bar@
hs_bindgen_a28b81f0afc23eed ::
     Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) FC.CInt))
  -> IO FC.CInt
hs_bindgen_a28b81f0afc23eed =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a28b81f0afc23eed_base

{-| __C declaration:__ @bar@

    __defined at:__ @arrays\/multi_dim.h 8:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar ::
     Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) FC.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
bar = hs_bindgen_a28b81f0afc23eed

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_bar_const@
foreign import ccall unsafe "hs_bindgen_58337c492b64ae2c" hs_bindgen_58337c492b64ae2c_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_bar_const@
hs_bindgen_58337c492b64ae2c ::
     HsBindgen.Runtime.PtrConst.PtrConst (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) FC.CInt))
  -> IO FC.CInt
hs_bindgen_58337c492b64ae2c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_58337c492b64ae2c_base

{-| __C declaration:__ @bar_const@

    __defined at:__ @arrays\/multi_dim.h 9:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar_const ::
     HsBindgen.Runtime.PtrConst.PtrConst (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) FC.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
bar_const = hs_bindgen_58337c492b64ae2c

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_baz@
foreign import ccall unsafe "hs_bindgen_48876e6767cb5923" hs_bindgen_48876e6767cb5923_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_baz@
hs_bindgen_48876e6767cb5923 ::
     Ptr.Ptr Matrix
  -> IO FC.CInt
hs_bindgen_48876e6767cb5923 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_48876e6767cb5923_base

{-| __C declaration:__ @baz@

    __defined at:__ @arrays\/multi_dim.h 13:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz ::
     Ptr.Ptr Matrix
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
baz = hs_bindgen_48876e6767cb5923

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_baz_const@
foreign import ccall unsafe "hs_bindgen_13ce150055e8aa41" hs_bindgen_13ce150055e8aa41_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_baz_const@
hs_bindgen_13ce150055e8aa41 ::
     HsBindgen.Runtime.PtrConst.PtrConst Matrix
  -> IO FC.CInt
hs_bindgen_13ce150055e8aa41 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_13ce150055e8aa41_base

{-| __C declaration:__ @baz_const@

    __defined at:__ @arrays\/multi_dim.h 14:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz_const ::
     HsBindgen.Runtime.PtrConst.PtrConst Matrix
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
baz_const = hs_bindgen_13ce150055e8aa41

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_quuz@
foreign import ccall unsafe "hs_bindgen_da5c432144bd5546" hs_bindgen_da5c432144bd5546_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_quuz@
hs_bindgen_da5c432144bd5546 ::
     Ptr.Ptr Triplets
  -> IO FC.CInt
hs_bindgen_da5c432144bd5546 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_da5c432144bd5546_base

{-| __C declaration:__ @quuz@

    __defined at:__ @arrays\/multi_dim.h 18:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz ::
     Ptr.Ptr Triplets
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
quuz = hs_bindgen_da5c432144bd5546

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_quuz_const@
foreign import ccall unsafe "hs_bindgen_70be94a6fb59f547" hs_bindgen_70be94a6fb59f547_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_quuz_const@
hs_bindgen_70be94a6fb59f547 ::
     HsBindgen.Runtime.PtrConst.PtrConst Triplets
  -> IO FC.CInt
hs_bindgen_70be94a6fb59f547 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_70be94a6fb59f547_base

{-| __C declaration:__ @quuz_const@

    __defined at:__ @arrays\/multi_dim.h 19:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz_const ::
     HsBindgen.Runtime.PtrConst.PtrConst Triplets
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
quuz_const = hs_bindgen_70be94a6fb59f547
