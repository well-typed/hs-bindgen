{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    , Example.Unsafe.foo_const
    , Example.Unsafe.bar
    , Example.Unsafe.bar_const
    , Example.Unsafe.baz
    , Example.Unsafe.baz_const
    , Example.Unsafe.quuz
    , Example.Unsafe.quuz_const
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <arrays/multi_dim.h>"
  , "signed int hs_bindgen_3389520bc7419af4 ("
  , "  signed int (*arg1)[4]"
  , ")"
  , "{"
  , "  return (foo)(arg1);"
  , "}"
  , "signed int hs_bindgen_73e2db5ad5d807f7 ("
  , "  signed int const (*arg1)[4]"
  , ")"
  , "{"
  , "  return (foo_const)(arg1);"
  , "}"
  , "signed int hs_bindgen_a28b81f0afc23eed ("
  , "  signed int (*arg1)[2]"
  , ")"
  , "{"
  , "  return (bar)(arg1);"
  , "}"
  , "signed int hs_bindgen_58337c492b64ae2c ("
  , "  signed int const (*arg1)[2]"
  , ")"
  , "{"
  , "  return (bar_const)(arg1);"
  , "}"
  , "signed int hs_bindgen_48876e6767cb5923 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return (baz)(arg1);"
  , "}"
  , "signed int hs_bindgen_13ce150055e8aa41 ("
  , "  signed int const (*arg1)[3]"
  , ")"
  , "{"
  , "  return (baz_const)(arg1);"
  , "}"
  , "signed int hs_bindgen_da5c432144bd5546 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return (quuz)(arg1);"
  , "}"
  , "signed int hs_bindgen_70be94a6fb59f547 ("
  , "  signed int const (*arg1)[3]"
  , ")"
  , "{"
  , "  return (quuz_const)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_3389520bc7419af4" hs_bindgen_3389520bc7419af4_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_foo@
hs_bindgen_3389520bc7419af4 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 3 (CA.ConstantArray 4 BG.CInt)))
  -> IO BG.CInt
hs_bindgen_3389520bc7419af4 =
  BG.fromFFIType hs_bindgen_3389520bc7419af4_base

{-| __C declaration:__ @foo@

    __defined at:__ @arrays\/multi_dim.h 4:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 3 (CA.ConstantArray 4 BG.CInt)))
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
foo = hs_bindgen_3389520bc7419af4

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_foo_const@
foreign import ccall unsafe "hs_bindgen_73e2db5ad5d807f7" hs_bindgen_73e2db5ad5d807f7_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_foo_const@
hs_bindgen_73e2db5ad5d807f7 ::
     PtrConst.PtrConst (IsA.Elem (CA.ConstantArray 3 (CA.ConstantArray 4 BG.CInt)))
  -> IO BG.CInt
hs_bindgen_73e2db5ad5d807f7 =
  BG.fromFFIType hs_bindgen_73e2db5ad5d807f7_base

{-| __C declaration:__ @foo_const@

    __defined at:__ @arrays\/multi_dim.h 5:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo_const ::
     PtrConst.PtrConst (IsA.Elem (CA.ConstantArray 3 (CA.ConstantArray 4 BG.CInt)))
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
foo_const = hs_bindgen_73e2db5ad5d807f7

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_a28b81f0afc23eed" hs_bindgen_a28b81f0afc23eed_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_bar@
hs_bindgen_a28b81f0afc23eed ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 2 BG.CInt)))
  -> IO BG.CInt
hs_bindgen_a28b81f0afc23eed =
  BG.fromFFIType hs_bindgen_a28b81f0afc23eed_base

{-| __C declaration:__ @bar@

    __defined at:__ @arrays\/multi_dim.h 8:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 2 BG.CInt)))
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
bar = hs_bindgen_a28b81f0afc23eed

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_bar_const@
foreign import ccall unsafe "hs_bindgen_58337c492b64ae2c" hs_bindgen_58337c492b64ae2c_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_bar_const@
hs_bindgen_58337c492b64ae2c ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 2 BG.CInt)))
  -> IO BG.CInt
hs_bindgen_58337c492b64ae2c =
  BG.fromFFIType hs_bindgen_58337c492b64ae2c_base

{-| __C declaration:__ @bar_const@

    __defined at:__ @arrays\/multi_dim.h 9:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar_const ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 2 BG.CInt)))
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
bar_const = hs_bindgen_58337c492b64ae2c

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_baz@
foreign import ccall unsafe "hs_bindgen_48876e6767cb5923" hs_bindgen_48876e6767cb5923_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_baz@
hs_bindgen_48876e6767cb5923 ::
     BG.Ptr (IsA.Elem Matrix)
  -> IO BG.CInt
hs_bindgen_48876e6767cb5923 =
  BG.fromFFIType hs_bindgen_48876e6767cb5923_base

{-| __C declaration:__ @baz@

    __defined at:__ @arrays\/multi_dim.h 13:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz ::
     BG.Ptr (IsA.Elem Matrix)
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
baz = hs_bindgen_48876e6767cb5923

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_baz_const@
foreign import ccall unsafe "hs_bindgen_13ce150055e8aa41" hs_bindgen_13ce150055e8aa41_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_baz_const@
hs_bindgen_13ce150055e8aa41 ::
     PtrConst.PtrConst (IsA.Elem Matrix)
  -> IO BG.CInt
hs_bindgen_13ce150055e8aa41 =
  BG.fromFFIType hs_bindgen_13ce150055e8aa41_base

{-| __C declaration:__ @baz_const@

    __defined at:__ @arrays\/multi_dim.h 14:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz_const ::
     PtrConst.PtrConst (IsA.Elem Matrix)
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
baz_const = hs_bindgen_13ce150055e8aa41

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_quuz@
foreign import ccall unsafe "hs_bindgen_da5c432144bd5546" hs_bindgen_da5c432144bd5546_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_quuz@
hs_bindgen_da5c432144bd5546 ::
     BG.Ptr (IsA.Elem Triplets)
  -> IO BG.CInt
hs_bindgen_da5c432144bd5546 =
  BG.fromFFIType hs_bindgen_da5c432144bd5546_base

{-| __C declaration:__ @quuz@

    __defined at:__ @arrays\/multi_dim.h 18:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz ::
     BG.Ptr (IsA.Elem Triplets)
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
quuz = hs_bindgen_da5c432144bd5546

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_quuz_const@
foreign import ccall unsafe "hs_bindgen_70be94a6fb59f547" hs_bindgen_70be94a6fb59f547_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Unsafe_quuz_const@
hs_bindgen_70be94a6fb59f547 ::
     PtrConst.PtrConst (IsA.Elem Triplets)
  -> IO BG.CInt
hs_bindgen_70be94a6fb59f547 =
  BG.fromFFIType hs_bindgen_70be94a6fb59f547_base

{-| __C declaration:__ @quuz_const@

    __defined at:__ @arrays\/multi_dim.h 19:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz_const ::
     PtrConst.PtrConst (IsA.Elem Triplets)
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
quuz_const = hs_bindgen_70be94a6fb59f547
