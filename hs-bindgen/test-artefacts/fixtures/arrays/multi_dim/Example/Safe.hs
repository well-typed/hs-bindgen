{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.foo_const
    , Example.Safe.bar
    , Example.Safe.bar_const
    , Example.Safe.baz
    , Example.Safe.baz_const
    , Example.Safe.quuz
    , Example.Safe.quuz_const
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
  , "signed int hs_bindgen_57b00b79dd5b838e ("
  , "  signed int (*arg1)[4]"
  , ")"
  , "{"
  , "  return (foo)(arg1);"
  , "}"
  , "signed int hs_bindgen_b75122693c32e26c ("
  , "  signed int const (*arg1)[4]"
  , ")"
  , "{"
  , "  return (foo_const)(arg1);"
  , "}"
  , "signed int hs_bindgen_595b7d50222e502a ("
  , "  signed int (*arg1)[2]"
  , ")"
  , "{"
  , "  return (bar)(arg1);"
  , "}"
  , "signed int hs_bindgen_1e0c14a0ed027681 ("
  , "  signed int const (*arg1)[2]"
  , ")"
  , "{"
  , "  return (bar_const)(arg1);"
  , "}"
  , "signed int hs_bindgen_39849d83dadd53d9 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return (baz)(arg1);"
  , "}"
  , "signed int hs_bindgen_0c873d371c129e78 ("
  , "  signed int const (*arg1)[3]"
  , ")"
  , "{"
  , "  return (baz_const)(arg1);"
  , "}"
  , "signed int hs_bindgen_a364da5adecafb37 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return (quuz)(arg1);"
  , "}"
  , "signed int hs_bindgen_e39fa59aeb1fef3d ("
  , "  signed int const (*arg1)[3]"
  , ")"
  , "{"
  , "  return (quuz_const)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_arraysmulti_dim_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_57b00b79dd5b838e" hs_bindgen_57b00b79dd5b838e_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_foo@
hs_bindgen_57b00b79dd5b838e ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 3 (CA.ConstantArray 4 BG.CInt)))
  -> IO BG.CInt
hs_bindgen_57b00b79dd5b838e =
  BG.fromFFIType hs_bindgen_57b00b79dd5b838e_base

{-| __C declaration:__ @foo@

    __defined at:__ @arrays\/multi_dim.h 4:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 3 (CA.ConstantArray 4 BG.CInt)))
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
foo = hs_bindgen_57b00b79dd5b838e

-- __unique:__ @test_arraysmulti_dim_Example_Safe_foo_const@
foreign import ccall safe "hs_bindgen_b75122693c32e26c" hs_bindgen_b75122693c32e26c_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_foo_const@
hs_bindgen_b75122693c32e26c ::
     PtrConst.PtrConst (IsA.Elem (CA.ConstantArray 3 (CA.ConstantArray 4 BG.CInt)))
  -> IO BG.CInt
hs_bindgen_b75122693c32e26c =
  BG.fromFFIType hs_bindgen_b75122693c32e26c_base

{-| __C declaration:__ @foo_const@

    __defined at:__ @arrays\/multi_dim.h 5:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo_const ::
     PtrConst.PtrConst (IsA.Elem (CA.ConstantArray 3 (CA.ConstantArray 4 BG.CInt)))
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
foo_const = hs_bindgen_b75122693c32e26c

-- __unique:__ @test_arraysmulti_dim_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_595b7d50222e502a" hs_bindgen_595b7d50222e502a_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_bar@
hs_bindgen_595b7d50222e502a ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 2 BG.CInt)))
  -> IO BG.CInt
hs_bindgen_595b7d50222e502a =
  BG.fromFFIType hs_bindgen_595b7d50222e502a_base

{-| __C declaration:__ @bar@

    __defined at:__ @arrays\/multi_dim.h 8:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 2 BG.CInt)))
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
bar = hs_bindgen_595b7d50222e502a

-- __unique:__ @test_arraysmulti_dim_Example_Safe_bar_const@
foreign import ccall safe "hs_bindgen_1e0c14a0ed027681" hs_bindgen_1e0c14a0ed027681_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_bar_const@
hs_bindgen_1e0c14a0ed027681 ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 2 BG.CInt)))
  -> IO BG.CInt
hs_bindgen_1e0c14a0ed027681 =
  BG.fromFFIType hs_bindgen_1e0c14a0ed027681_base

{-| __C declaration:__ @bar_const@

    __defined at:__ @arrays\/multi_dim.h 9:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar_const ::
     PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 2 BG.CInt)))
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
bar_const = hs_bindgen_1e0c14a0ed027681

-- __unique:__ @test_arraysmulti_dim_Example_Safe_baz@
foreign import ccall safe "hs_bindgen_39849d83dadd53d9" hs_bindgen_39849d83dadd53d9_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_baz@
hs_bindgen_39849d83dadd53d9 ::
     BG.Ptr (IsA.Elem Matrix)
  -> IO BG.CInt
hs_bindgen_39849d83dadd53d9 =
  BG.fromFFIType hs_bindgen_39849d83dadd53d9_base

{-| __C declaration:__ @baz@

    __defined at:__ @arrays\/multi_dim.h 13:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz ::
     BG.Ptr (IsA.Elem Matrix)
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
baz = hs_bindgen_39849d83dadd53d9

-- __unique:__ @test_arraysmulti_dim_Example_Safe_baz_const@
foreign import ccall safe "hs_bindgen_0c873d371c129e78" hs_bindgen_0c873d371c129e78_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_baz_const@
hs_bindgen_0c873d371c129e78 ::
     PtrConst.PtrConst (IsA.Elem Matrix)
  -> IO BG.CInt
hs_bindgen_0c873d371c129e78 =
  BG.fromFFIType hs_bindgen_0c873d371c129e78_base

{-| __C declaration:__ @baz_const@

    __defined at:__ @arrays\/multi_dim.h 14:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz_const ::
     PtrConst.PtrConst (IsA.Elem Matrix)
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
baz_const = hs_bindgen_0c873d371c129e78

-- __unique:__ @test_arraysmulti_dim_Example_Safe_quuz@
foreign import ccall safe "hs_bindgen_a364da5adecafb37" hs_bindgen_a364da5adecafb37_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_quuz@
hs_bindgen_a364da5adecafb37 ::
     BG.Ptr (IsA.Elem Triplets)
  -> IO BG.CInt
hs_bindgen_a364da5adecafb37 =
  BG.fromFFIType hs_bindgen_a364da5adecafb37_base

{-| __C declaration:__ @quuz@

    __defined at:__ @arrays\/multi_dim.h 18:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz ::
     BG.Ptr (IsA.Elem Triplets)
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
quuz = hs_bindgen_a364da5adecafb37

-- __unique:__ @test_arraysmulti_dim_Example_Safe_quuz_const@
foreign import ccall safe "hs_bindgen_e39fa59aeb1fef3d" hs_bindgen_e39fa59aeb1fef3d_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_quuz_const@
hs_bindgen_e39fa59aeb1fef3d ::
     PtrConst.PtrConst (IsA.Elem Triplets)
  -> IO BG.CInt
hs_bindgen_e39fa59aeb1fef3d =
  BG.fromFFIType hs_bindgen_e39fa59aeb1fef3d_base

{-| __C declaration:__ @quuz_const@

    __defined at:__ @arrays\/multi_dim.h 19:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz_const ::
     PtrConst.PtrConst (IsA.Elem Triplets)
     -- ^ __C declaration:__ @xss@
  -> IO BG.CInt
quuz_const = hs_bindgen_e39fa59aeb1fef3d
