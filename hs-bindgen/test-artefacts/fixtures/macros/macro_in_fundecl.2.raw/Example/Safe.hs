{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.quux
    , Example.Safe.wam
    , Example.Safe.foo1
    , Example.Safe.foo2
    , Example.Safe.foo3
    , Example.Safe.bar1
    , Example.Safe.bar2
    , Example.Safe.bar3
    , Example.Safe.bar4
    , Example.Safe.baz1
    , Example.Safe.baz2
    , Example.Safe.baz3
    , Example.Safe.no_args_no_void
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/macro_in_fundecl.h>"
  , "char hs_bindgen_d9bba200e8ee3d2e ("
  , "  float arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return (quux)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_8e4fd99c7d7552e4 ("
  , "  float arg1,"
  , "  char *arg2"
  , ")"
  , "{"
  , "  return (wam)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_0d427252ce24447b ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo1)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_6e91d4c8387d9391 ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo2)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_d36a763eccc05a06 ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo3)(arg1, arg2);"
  , "}"
  , "signed int (*hs_bindgen_f38b40583a94f166 ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_2eee4ad4c74521f2 ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar2)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_e390113427697fef ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar3)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_30d0a25d063e8e6d ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar4)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_4f754558fc5ce39a ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_cd12ed5725181713 ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz2)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_442db04f13fdac86 ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz3)(arg1);"
  , "}"
  , "signed int hs_bindgen_e84f5f25b8a3d57c (void)"
  , "{"
  , "  return (no_args_no_void)();"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_quux@
foreign import ccall safe "hs_bindgen_d9bba200e8ee3d2e" hs_bindgen_d9bba200e8ee3d2e_base ::
     Float
  -> BG.Int8
  -> IO BG.Int8

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_quux@
hs_bindgen_d9bba200e8ee3d2e ::
     BG.CFloat
  -> BG.CChar
  -> IO BG.CChar
hs_bindgen_d9bba200e8ee3d2e =
  BG.fromFFIType hs_bindgen_d9bba200e8ee3d2e_base

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/macro_in_fundecl.h 12:6@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
quux ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> BG.CChar
     -- ^ __C declaration:__ @y@
  -> IO BG.CChar
quux = hs_bindgen_d9bba200e8ee3d2e

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_wam@
foreign import ccall safe "hs_bindgen_8e4fd99c7d7552e4" hs_bindgen_8e4fd99c7d7552e4_base ::
     Float
  -> BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_wam@
hs_bindgen_8e4fd99c7d7552e4 ::
     BG.CFloat
  -> BG.Ptr BG.CChar
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_8e4fd99c7d7552e4 =
  BG.fromFFIType hs_bindgen_8e4fd99c7d7552e4_base

{-| __C declaration:__ @wam@

    __defined at:__ @macros\/macro_in_fundecl.h 13:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
wam ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> BG.Ptr BG.CChar
     -- ^ __C declaration:__ @y@
  -> IO (BG.Ptr BG.CChar)
wam = hs_bindgen_8e4fd99c7d7552e4

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_foo1@
foreign import ccall safe "hs_bindgen_0d427252ce24447b" hs_bindgen_0d427252ce24447b_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_foo1@
hs_bindgen_0d427252ce24447b ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_0d427252ce24447b =
  BG.fromFFIType hs_bindgen_0d427252ce24447b_base

{-| __C declaration:__ @foo1@

    __defined at:__ @macros\/macro_in_fundecl.h 16:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo1 ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (BG.Ptr BG.CChar)
foo1 = hs_bindgen_0d427252ce24447b

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_foo2@
foreign import ccall safe "hs_bindgen_6e91d4c8387d9391" hs_bindgen_6e91d4c8387d9391_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_foo2@
hs_bindgen_6e91d4c8387d9391 ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_6e91d4c8387d9391 =
  BG.fromFFIType hs_bindgen_6e91d4c8387d9391_base

{-| __C declaration:__ @foo2@

    __defined at:__ @macros\/macro_in_fundecl.h 17:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo2 ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (BG.Ptr BG.CChar)
foo2 = hs_bindgen_6e91d4c8387d9391

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_foo3@
foreign import ccall safe "hs_bindgen_d36a763eccc05a06" hs_bindgen_d36a763eccc05a06_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_foo3@
hs_bindgen_d36a763eccc05a06 ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_d36a763eccc05a06 =
  BG.fromFFIType hs_bindgen_d36a763eccc05a06_base

{-| __C declaration:__ @foo3@

    __defined at:__ @macros\/macro_in_fundecl.h 18:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo3 ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (BG.Ptr BG.CChar)
foo3 = hs_bindgen_d36a763eccc05a06

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_bar1@
foreign import ccall safe "hs_bindgen_f38b40583a94f166" hs_bindgen_f38b40583a94f166_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_bar1@
hs_bindgen_f38b40583a94f166 ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_f38b40583a94f166 =
  BG.fromFFIType hs_bindgen_f38b40583a94f166_base

{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h 21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar1 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar1 = hs_bindgen_f38b40583a94f166

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_bar2@
foreign import ccall safe "hs_bindgen_2eee4ad4c74521f2" hs_bindgen_2eee4ad4c74521f2_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_bar2@
hs_bindgen_2eee4ad4c74521f2 ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_2eee4ad4c74521f2 =
  BG.fromFFIType hs_bindgen_2eee4ad4c74521f2_base

{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h 22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar2 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar2 = hs_bindgen_2eee4ad4c74521f2

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_bar3@
foreign import ccall safe "hs_bindgen_e390113427697fef" hs_bindgen_e390113427697fef_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_bar3@
hs_bindgen_e390113427697fef ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_e390113427697fef =
  BG.fromFFIType hs_bindgen_e390113427697fef_base

{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h 23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar3 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar3 = hs_bindgen_e390113427697fef

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_bar4@
foreign import ccall safe "hs_bindgen_30d0a25d063e8e6d" hs_bindgen_30d0a25d063e8e6d_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_bar4@
hs_bindgen_30d0a25d063e8e6d ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_30d0a25d063e8e6d =
  BG.fromFFIType hs_bindgen_30d0a25d063e8e6d_base

{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h 24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar4 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar4 = hs_bindgen_30d0a25d063e8e6d

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_baz1@
foreign import ccall safe "hs_bindgen_4f754558fc5ce39a" hs_bindgen_4f754558fc5ce39a_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_baz1@
hs_bindgen_4f754558fc5ce39a ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_4f754558fc5ce39a =
  BG.fromFFIType hs_bindgen_4f754558fc5ce39a_base

{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h 27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz1 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz1 = hs_bindgen_4f754558fc5ce39a

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_baz2@
foreign import ccall safe "hs_bindgen_cd12ed5725181713" hs_bindgen_cd12ed5725181713_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_baz2@
hs_bindgen_cd12ed5725181713 ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_cd12ed5725181713 =
  BG.fromFFIType hs_bindgen_cd12ed5725181713_base

{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h 35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz2 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz2 = hs_bindgen_cd12ed5725181713

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_baz3@
foreign import ccall safe "hs_bindgen_442db04f13fdac86" hs_bindgen_442db04f13fdac86_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_baz3@
hs_bindgen_442db04f13fdac86 ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_442db04f13fdac86 =
  BG.fromFFIType hs_bindgen_442db04f13fdac86_base

{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h 43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz3 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz3 = hs_bindgen_442db04f13fdac86

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_no_args_no_void@
foreign import ccall safe "hs_bindgen_e84f5f25b8a3d57c" hs_bindgen_e84f5f25b8a3d57c_base ::
     IO BG.Int32

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Safe_no_args_no_void@
hs_bindgen_e84f5f25b8a3d57c :: IO BG.CInt
hs_bindgen_e84f5f25b8a3d57c =
  BG.fromFFIType hs_bindgen_e84f5f25b8a3d57c_base

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h 53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
no_args_no_void :: IO BG.CInt
no_args_no_void = hs_bindgen_e84f5f25b8a3d57c
