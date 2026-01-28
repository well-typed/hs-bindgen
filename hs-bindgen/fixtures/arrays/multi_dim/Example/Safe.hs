{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <arrays/multi_dim.h>"
  , "signed int hs_bindgen_57b00b79dd5b838e ("
  , "  signed int (*arg1)[4]"
  , ")"
  , "{"
  , "  return foo(arg1);"
  , "}"
  , "signed int hs_bindgen_b75122693c32e26c ("
  , "  signed int const (*arg1)[4]"
  , ")"
  , "{"
  , "  return foo_const(arg1);"
  , "}"
  , "signed int hs_bindgen_595b7d50222e502a ("
  , "  signed int (*arg1)[2]"
  , ")"
  , "{"
  , "  return bar(arg1);"
  , "}"
  , "signed int hs_bindgen_1e0c14a0ed027681 ("
  , "  signed int const (*arg1)[2]"
  , ")"
  , "{"
  , "  return bar_const(arg1);"
  , "}"
  , "signed int hs_bindgen_39849d83dadd53d9 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return baz(arg1);"
  , "}"
  , "signed int hs_bindgen_0c873d371c129e78 ("
  , "  signed int const (*arg1)[3]"
  , ")"
  , "{"
  , "  return baz_const(arg1);"
  , "}"
  , "signed int hs_bindgen_a364da5adecafb37 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return quuz(arg1);"
  , "}"
  , "signed int hs_bindgen_e39fa59aeb1fef3d ("
  , "  signed int const (*arg1)[3]"
  , ")"
  , "{"
  , "  return quuz_const(arg1);"
  , "}"
  ]))

-- __unique:__ @test_arraysmulti_dim_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_57b00b79dd5b838e" hs_bindgen_57b00b79dd5b838e_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_foo@
hs_bindgen_57b00b79dd5b838e ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt)
  -> IO FC.CInt
hs_bindgen_57b00b79dd5b838e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_57b00b79dd5b838e_base

{-| __C declaration:__ @foo@

    __defined at:__ @arrays\/multi_dim.h 4:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
foo = hs_bindgen_57b00b79dd5b838e

-- __unique:__ @test_arraysmulti_dim_Example_Safe_foo_const@
foreign import ccall safe "hs_bindgen_b75122693c32e26c" hs_bindgen_b75122693c32e26c_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_foo_const@
hs_bindgen_b75122693c32e26c ::
     HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt)
  -> IO FC.CInt
hs_bindgen_b75122693c32e26c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_b75122693c32e26c_base

{-| __C declaration:__ @foo_const@

    __defined at:__ @arrays\/multi_dim.h 5:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo_const ::
     HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
foo_const = hs_bindgen_b75122693c32e26c

-- __unique:__ @test_arraysmulti_dim_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_595b7d50222e502a" hs_bindgen_595b7d50222e502a_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_bar@
hs_bindgen_595b7d50222e502a ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) FC.CInt)
  -> IO FC.CInt
hs_bindgen_595b7d50222e502a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_595b7d50222e502a_base

{-| __C declaration:__ @bar@

    __defined at:__ @arrays\/multi_dim.h 8:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
bar = hs_bindgen_595b7d50222e502a

-- __unique:__ @test_arraysmulti_dim_Example_Safe_bar_const@
foreign import ccall safe "hs_bindgen_1e0c14a0ed027681" hs_bindgen_1e0c14a0ed027681_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_bar_const@
hs_bindgen_1e0c14a0ed027681 ::
     HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) FC.CInt)
  -> IO FC.CInt
hs_bindgen_1e0c14a0ed027681 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_1e0c14a0ed027681_base

{-| __C declaration:__ @bar_const@

    __defined at:__ @arrays\/multi_dim.h 9:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar_const ::
     HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
bar_const = hs_bindgen_1e0c14a0ed027681

-- __unique:__ @test_arraysmulti_dim_Example_Safe_baz@
foreign import ccall safe "hs_bindgen_39849d83dadd53d9" hs_bindgen_39849d83dadd53d9_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_baz@
hs_bindgen_39849d83dadd53d9 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
hs_bindgen_39849d83dadd53d9 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_39849d83dadd53d9_base

{-| __C declaration:__ @baz@

    __defined at:__ @arrays\/multi_dim.h 13:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
baz = hs_bindgen_39849d83dadd53d9

-- __unique:__ @test_arraysmulti_dim_Example_Safe_baz_const@
foreign import ccall safe "hs_bindgen_0c873d371c129e78" hs_bindgen_0c873d371c129e78_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_baz_const@
hs_bindgen_0c873d371c129e78 ::
     HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
hs_bindgen_0c873d371c129e78 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0c873d371c129e78_base

{-| __C declaration:__ @baz_const@

    __defined at:__ @arrays\/multi_dim.h 14:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz_const ::
     HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
baz_const = hs_bindgen_0c873d371c129e78

-- __unique:__ @test_arraysmulti_dim_Example_Safe_quuz@
foreign import ccall safe "hs_bindgen_a364da5adecafb37" hs_bindgen_a364da5adecafb37_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_quuz@
hs_bindgen_a364da5adecafb37 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
hs_bindgen_a364da5adecafb37 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a364da5adecafb37_base

{-| __C declaration:__ @quuz@

    __defined at:__ @arrays\/multi_dim.h 18:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
quuz = hs_bindgen_a364da5adecafb37

-- __unique:__ @test_arraysmulti_dim_Example_Safe_quuz_const@
foreign import ccall safe "hs_bindgen_e39fa59aeb1fef3d" hs_bindgen_e39fa59aeb1fef3d_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysmulti_dim_Example_Safe_quuz_const@
hs_bindgen_e39fa59aeb1fef3d ::
     HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
hs_bindgen_e39fa59aeb1fef3d =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_e39fa59aeb1fef3d_base

{-| __C declaration:__ @quuz_const@

    __defined at:__ @arrays\/multi_dim.h 19:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz_const ::
     HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
quuz_const = hs_bindgen_e39fa59aeb1fef3d
