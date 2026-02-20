{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/macro_in_fundecl_vs_typedef.h>"
  , "char hs_bindgen_df7e2b8e86de411a ("
  , "  MC arg1,"
  , "  TC arg2"
  , ")"
  , "{"
  , "  return (quux1)(arg1, arg2);"
  , "}"
  , "TC hs_bindgen_6f0b13ed02b696df ("
  , "  MC arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return (quux2)(arg1, arg2);"
  , "}"
  , "MC *hs_bindgen_f92059cc98dde342 ("
  , "  float arg1,"
  , "  TC *arg2"
  , ")"
  , "{"
  , "  return (wam1)(arg1, arg2);"
  , "}"
  , "TC *hs_bindgen_3e6ecd1b2cc616bc ("
  , "  float arg1,"
  , "  MC *arg2"
  , ")"
  , "{"
  , "  return (wam2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ffa9d5a3e8f0f221 ("
  , "  struct2 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  (struct_typedef1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4801667560542114 ("
  , "  struct3_t *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  (struct_typedef2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_04fa5bbd479146eb ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  (struct_typedef3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_08025fd0bd589ac2 ("
  , "  struct struct1 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  (struct_name1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9de286608f952fc7 ("
  , "  struct struct3 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  (struct_name2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_38cce6bb1ac71578 ("
  , "  struct struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  (struct_name3)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_quux1@
foreign import ccall unsafe "hs_bindgen_df7e2b8e86de411a" hs_bindgen_df7e2b8e86de411a_base ::
     RIP.Int8
  -> RIP.Int8
  -> IO RIP.Int8

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_quux1@
hs_bindgen_df7e2b8e86de411a ::
     MC
  -> TC
  -> IO RIP.CChar
hs_bindgen_df7e2b8e86de411a =
  RIP.fromFFIType hs_bindgen_df7e2b8e86de411a_base

{-| __C declaration:__ @quux1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 8:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux1 ::
     MC
     -- ^ __C declaration:__ @x@
  -> TC
     -- ^ __C declaration:__ @y@
  -> IO RIP.CChar
quux1 = hs_bindgen_df7e2b8e86de411a

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_quux2@
foreign import ccall unsafe "hs_bindgen_6f0b13ed02b696df" hs_bindgen_6f0b13ed02b696df_base ::
     RIP.Int8
  -> RIP.Int8
  -> IO RIP.Int8

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_quux2@
hs_bindgen_6f0b13ed02b696df ::
     MC
  -> RIP.CChar
  -> IO TC
hs_bindgen_6f0b13ed02b696df =
  RIP.fromFFIType hs_bindgen_6f0b13ed02b696df_base

{-| __C declaration:__ @quux2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 9:4@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux2 ::
     MC
     -- ^ __C declaration:__ @x@
  -> RIP.CChar
     -- ^ __C declaration:__ @y@
  -> IO TC
quux2 = hs_bindgen_6f0b13ed02b696df

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_wam1@
foreign import ccall unsafe "hs_bindgen_f92059cc98dde342" hs_bindgen_f92059cc98dde342_base ::
     Float
  -> RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_wam1@
hs_bindgen_f92059cc98dde342 ::
     RIP.CFloat
  -> RIP.Ptr TC
  -> IO (RIP.Ptr MC)
hs_bindgen_f92059cc98dde342 =
  RIP.fromFFIType hs_bindgen_f92059cc98dde342_base

{-| __C declaration:__ @wam1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 10:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam1 ::
     RIP.CFloat
     -- ^ __C declaration:__ @x@
  -> RIP.Ptr TC
     -- ^ __C declaration:__ @y@
  -> IO (RIP.Ptr MC)
wam1 = hs_bindgen_f92059cc98dde342

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_wam2@
foreign import ccall unsafe "hs_bindgen_3e6ecd1b2cc616bc" hs_bindgen_3e6ecd1b2cc616bc_base ::
     Float
  -> RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_wam2@
hs_bindgen_3e6ecd1b2cc616bc ::
     RIP.CFloat
  -> RIP.Ptr MC
  -> IO (RIP.Ptr TC)
hs_bindgen_3e6ecd1b2cc616bc =
  RIP.fromFFIType hs_bindgen_3e6ecd1b2cc616bc_base

{-| __C declaration:__ @wam2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 11:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam2 ::
     RIP.CFloat
     -- ^ __C declaration:__ @x@
  -> RIP.Ptr MC
     -- ^ __C declaration:__ @y@
  -> IO (RIP.Ptr TC)
wam2 = hs_bindgen_3e6ecd1b2cc616bc

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_typedef1@
foreign import ccall unsafe "hs_bindgen_ffa9d5a3e8f0f221" hs_bindgen_ffa9d5a3e8f0f221_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_typedef1@
hs_bindgen_ffa9d5a3e8f0f221 ::
     RIP.Ptr Struct2
  -> MC
  -> IO ()
hs_bindgen_ffa9d5a3e8f0f221 =
  RIP.fromFFIType hs_bindgen_ffa9d5a3e8f0f221_base

{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 23:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef1 ::
     RIP.Ptr Struct2
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_typedef1 = hs_bindgen_ffa9d5a3e8f0f221

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_typedef2@
foreign import ccall unsafe "hs_bindgen_4801667560542114" hs_bindgen_4801667560542114_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_typedef2@
hs_bindgen_4801667560542114 ::
     RIP.Ptr Struct3_t
  -> MC
  -> IO ()
hs_bindgen_4801667560542114 =
  RIP.fromFFIType hs_bindgen_4801667560542114_base

{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 24:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef2 ::
     RIP.Ptr Struct3_t
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_typedef2 = hs_bindgen_4801667560542114

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_typedef3@
foreign import ccall unsafe "hs_bindgen_04fa5bbd479146eb" hs_bindgen_04fa5bbd479146eb_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_typedef3@
hs_bindgen_04fa5bbd479146eb ::
     RIP.Ptr Struct4
  -> MC
  -> IO ()
hs_bindgen_04fa5bbd479146eb =
  RIP.fromFFIType hs_bindgen_04fa5bbd479146eb_base

{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 25:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef3 ::
     RIP.Ptr Struct4
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_typedef3 = hs_bindgen_04fa5bbd479146eb

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_name1@
foreign import ccall unsafe "hs_bindgen_08025fd0bd589ac2" hs_bindgen_08025fd0bd589ac2_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_name1@
hs_bindgen_08025fd0bd589ac2 ::
     RIP.Ptr Struct1
  -> MC
  -> IO ()
hs_bindgen_08025fd0bd589ac2 =
  RIP.fromFFIType hs_bindgen_08025fd0bd589ac2_base

{-| __C declaration:__ @struct_name1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 27:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name1 ::
     RIP.Ptr Struct1
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_name1 = hs_bindgen_08025fd0bd589ac2

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_name2@
foreign import ccall unsafe "hs_bindgen_9de286608f952fc7" hs_bindgen_9de286608f952fc7_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_name2@
hs_bindgen_9de286608f952fc7 ::
     RIP.Ptr Struct3
  -> MC
  -> IO ()
hs_bindgen_9de286608f952fc7 =
  RIP.fromFFIType hs_bindgen_9de286608f952fc7_base

{-| __C declaration:__ @struct_name2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 28:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name2 ::
     RIP.Ptr Struct3
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_name2 = hs_bindgen_9de286608f952fc7

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_name3@
foreign import ccall unsafe "hs_bindgen_38cce6bb1ac71578" hs_bindgen_38cce6bb1ac71578_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_name3@
hs_bindgen_38cce6bb1ac71578 ::
     RIP.Ptr Struct4
  -> MC
  -> IO ()
hs_bindgen_38cce6bb1ac71578 =
  RIP.fromFFIType hs_bindgen_38cce6bb1ac71578_base

{-| __C declaration:__ @struct_name3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 29:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name3 ::
     RIP.Ptr Struct4
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_name3 = hs_bindgen_38cce6bb1ac71578
