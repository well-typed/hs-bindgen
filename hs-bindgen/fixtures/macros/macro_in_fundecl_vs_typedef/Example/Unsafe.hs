{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/macro_in_fundecl_vs_typedef.h>"
  , "char hs_bindgen_df7e2b8e86de411a ("
  , "  MC arg1,"
  , "  TC arg2"
  , ")"
  , "{"
  , "  return quux1(arg1, arg2);"
  , "}"
  , "TC hs_bindgen_6f0b13ed02b696df ("
  , "  MC arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return quux2(arg1, arg2);"
  , "}"
  , "MC *hs_bindgen_f92059cc98dde342 ("
  , "  float arg1,"
  , "  TC *arg2"
  , ")"
  , "{"
  , "  return wam1(arg1, arg2);"
  , "}"
  , "TC *hs_bindgen_3e6ecd1b2cc616bc ("
  , "  float arg1,"
  , "  MC *arg2"
  , ")"
  , "{"
  , "  return wam2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ffa9d5a3e8f0f221 ("
  , "  struct2 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_4801667560542114 ("
  , "  struct3_t *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_04fa5bbd479146eb ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_08025fd0bd589ac2 ("
  , "  struct struct1 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_9de286608f952fc7 ("
  , "  struct struct3 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_38cce6bb1ac71578 ("
  , "  struct struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name3(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_quux1@
foreign import ccall unsafe "hs_bindgen_df7e2b8e86de411a" hs_bindgen_df7e2b8e86de411a_base ::
     FC.CChar
  -> FC.CChar
  -> IO FC.CChar

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_quux1@
hs_bindgen_df7e2b8e86de411a ::
     MC
  -> TC
  -> IO FC.CChar
hs_bindgen_df7e2b8e86de411a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_df7e2b8e86de411a_base

{-| __C declaration:__ @quux1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 8:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux1 ::
     MC
     -- ^ __C declaration:__ @x@
  -> TC
     -- ^ __C declaration:__ @y@
  -> IO FC.CChar
quux1 = hs_bindgen_df7e2b8e86de411a

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_quux2@
foreign import ccall unsafe "hs_bindgen_6f0b13ed02b696df" hs_bindgen_6f0b13ed02b696df_base ::
     FC.CChar
  -> FC.CChar
  -> IO FC.CChar

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_quux2@
hs_bindgen_6f0b13ed02b696df ::
     MC
  -> FC.CChar
  -> IO TC
hs_bindgen_6f0b13ed02b696df =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_6f0b13ed02b696df_base

{-| __C declaration:__ @quux2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 9:4@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux2 ::
     MC
     -- ^ __C declaration:__ @x@
  -> FC.CChar
     -- ^ __C declaration:__ @y@
  -> IO TC
quux2 = hs_bindgen_6f0b13ed02b696df

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_wam1@
foreign import ccall unsafe "hs_bindgen_f92059cc98dde342" hs_bindgen_f92059cc98dde342_base ::
     FC.CFloat
  -> Ptr.Ptr Void
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_wam1@
hs_bindgen_f92059cc98dde342 ::
     FC.CFloat
  -> Ptr.Ptr TC
  -> IO (Ptr.Ptr MC)
hs_bindgen_f92059cc98dde342 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f92059cc98dde342_base

{-| __C declaration:__ @wam1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 10:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam1 ::
     FC.CFloat
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr TC
     -- ^ __C declaration:__ @y@
  -> IO (Ptr.Ptr MC)
wam1 = hs_bindgen_f92059cc98dde342

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_wam2@
foreign import ccall unsafe "hs_bindgen_3e6ecd1b2cc616bc" hs_bindgen_3e6ecd1b2cc616bc_base ::
     FC.CFloat
  -> Ptr.Ptr Void
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_wam2@
hs_bindgen_3e6ecd1b2cc616bc ::
     FC.CFloat
  -> Ptr.Ptr MC
  -> IO (Ptr.Ptr TC)
hs_bindgen_3e6ecd1b2cc616bc =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3e6ecd1b2cc616bc_base

{-| __C declaration:__ @wam2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 11:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam2 ::
     FC.CFloat
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr MC
     -- ^ __C declaration:__ @y@
  -> IO (Ptr.Ptr TC)
wam2 = hs_bindgen_3e6ecd1b2cc616bc

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_typedef1@
foreign import ccall unsafe "hs_bindgen_ffa9d5a3e8f0f221" hs_bindgen_ffa9d5a3e8f0f221_base ::
     Ptr.Ptr Void
  -> FC.CChar
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_typedef1@
hs_bindgen_ffa9d5a3e8f0f221 ::
     Ptr.Ptr Struct2
  -> MC
  -> IO ()
hs_bindgen_ffa9d5a3e8f0f221 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_ffa9d5a3e8f0f221_base

{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 23:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef1 ::
     Ptr.Ptr Struct2
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_typedef1 = hs_bindgen_ffa9d5a3e8f0f221

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_typedef2@
foreign import ccall unsafe "hs_bindgen_4801667560542114" hs_bindgen_4801667560542114_base ::
     Ptr.Ptr Void
  -> FC.CChar
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_typedef2@
hs_bindgen_4801667560542114 ::
     Ptr.Ptr Struct3_t
  -> MC
  -> IO ()
hs_bindgen_4801667560542114 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4801667560542114_base

{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 24:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef2 ::
     Ptr.Ptr Struct3_t
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_typedef2 = hs_bindgen_4801667560542114

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_typedef3@
foreign import ccall unsafe "hs_bindgen_04fa5bbd479146eb" hs_bindgen_04fa5bbd479146eb_base ::
     Ptr.Ptr Void
  -> FC.CChar
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_typedef3@
hs_bindgen_04fa5bbd479146eb ::
     Ptr.Ptr Struct4
  -> MC
  -> IO ()
hs_bindgen_04fa5bbd479146eb =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_04fa5bbd479146eb_base

{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 25:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef3 ::
     Ptr.Ptr Struct4
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_typedef3 = hs_bindgen_04fa5bbd479146eb

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_name1@
foreign import ccall unsafe "hs_bindgen_08025fd0bd589ac2" hs_bindgen_08025fd0bd589ac2_base ::
     Ptr.Ptr Void
  -> FC.CChar
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_name1@
hs_bindgen_08025fd0bd589ac2 ::
     Ptr.Ptr Struct1
  -> MC
  -> IO ()
hs_bindgen_08025fd0bd589ac2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_08025fd0bd589ac2_base

{-| __C declaration:__ @struct_name1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 27:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name1 ::
     Ptr.Ptr Struct1
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_name1 = hs_bindgen_08025fd0bd589ac2

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_name2@
foreign import ccall unsafe "hs_bindgen_9de286608f952fc7" hs_bindgen_9de286608f952fc7_base ::
     Ptr.Ptr Void
  -> FC.CChar
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_name2@
hs_bindgen_9de286608f952fc7 ::
     Ptr.Ptr Struct3
  -> MC
  -> IO ()
hs_bindgen_9de286608f952fc7 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_9de286608f952fc7_base

{-| __C declaration:__ @struct_name2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 28:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name2 ::
     Ptr.Ptr Struct3
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_name2 = hs_bindgen_9de286608f952fc7

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_name3@
foreign import ccall unsafe "hs_bindgen_38cce6bb1ac71578" hs_bindgen_38cce6bb1ac71578_base ::
     Ptr.Ptr Void
  -> FC.CChar
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Unsafe_struct_name3@
hs_bindgen_38cce6bb1ac71578 ::
     Ptr.Ptr Struct4
  -> MC
  -> IO ()
hs_bindgen_38cce6bb1ac71578 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_38cce6bb1ac71578_base

{-| __C declaration:__ @struct_name3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 29:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name3 ::
     Ptr.Ptr Struct4
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_name3 = hs_bindgen_38cce6bb1ac71578
