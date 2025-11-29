{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/macro_in_fundecl_vs_typedef.h>"
  , "/* Example_get_quux1_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_145df45a32488372 (void)) ("
  , "  MC arg1,"
  , "  TC arg2"
  , ")"
  , "{"
  , "  return &quux1;"
  , "}"
  , "/* Example_get_quux2_ptr */"
  , "__attribute__ ((const))"
  , "TC (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_b81f6697ece237d9 (void)) ("
  , "  MC arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &quux2;"
  , "}"
  , "/* Example_get_wam1_ptr */"
  , "__attribute__ ((const))"
  , "MC *(*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7f3786b87654b1fb (void)) ("
  , "  float arg1,"
  , "  TC *arg2"
  , ")"
  , "{"
  , "  return &wam1;"
  , "}"
  , "/* Example_get_wam2_ptr */"
  , "__attribute__ ((const))"
  , "TC *(*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_540afd44424e4c59 (void)) ("
  , "  float arg1,"
  , "  MC *arg2"
  , ")"
  , "{"
  , "  return &wam2;"
  , "}"
  , "/* Example_get_struct_typedef1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_0dea50ddd5b6ca06 (void)) ("
  , "  struct2 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_typedef1;"
  , "}"
  , "/* Example_get_struct_typedef2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_434fb3300450ea3a (void)) ("
  , "  struct3_t *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_typedef2;"
  , "}"
  , "/* Example_get_struct_typedef3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_6ef76e6cdde5f560 (void)) ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_typedef3;"
  , "}"
  , "/* Example_get_struct_name1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_89909167992a336a (void)) ("
  , "  struct struct1 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_name1;"
  , "}"
  , "/* Example_get_struct_name2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_ff58316a7092a3c8 (void)) ("
  , "  struct struct3 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_name2;"
  , "}"
  , "/* Example_get_struct_name3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_95e033e6b275b4eb (void)) ("
  , "  struct struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_name3;"
  , "}"
  ]))

{-| __unique:__ @Example_get_quux1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_145df45a32488372" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_145df45a32488372 ::
     IO (Ptr.FunPtr (MC -> TC -> IO FC.CChar))

{-# NOINLINE quux1_ptr #-}

{-| __C declaration:__ @quux1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:8:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux1_ptr :: Ptr.FunPtr (MC -> TC -> IO FC.CChar)
quux1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_145df45a32488372

{-| __unique:__ @Example_get_quux2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_b81f6697ece237d9" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_b81f6697ece237d9 ::
     IO (Ptr.FunPtr (MC -> FC.CChar -> IO TC))

{-# NOINLINE quux2_ptr #-}

{-| __C declaration:__ @quux2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:9:4@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux2_ptr :: Ptr.FunPtr (MC -> FC.CChar -> IO TC)
quux2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_b81f6697ece237d9

{-| __unique:__ @Example_get_wam1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7f3786b87654b1fb" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7f3786b87654b1fb ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr TC) -> IO (Ptr.Ptr MC)))

{-# NOINLINE wam1_ptr #-}

{-| __C declaration:__ @wam1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:10:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam1_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr TC) -> IO (Ptr.Ptr MC))
wam1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7f3786b87654b1fb

{-| __unique:__ @Example_get_wam2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_540afd44424e4c59" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_540afd44424e4c59 ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr MC) -> IO (Ptr.Ptr TC)))

{-# NOINLINE wam2_ptr #-}

{-| __C declaration:__ @wam2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:11:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam2_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr MC) -> IO (Ptr.Ptr TC))
wam2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_540afd44424e4c59

{-| __unique:__ @Example_get_struct_typedef1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_0dea50ddd5b6ca06" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_0dea50ddd5b6ca06 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct2) -> MC -> IO ()))

{-# NOINLINE struct_typedef1_ptr #-}

{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:23:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef1_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct2) -> MC -> IO ())
struct_typedef1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_0dea50ddd5b6ca06

{-| __unique:__ @Example_get_struct_typedef2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_434fb3300450ea3a" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_434fb3300450ea3a ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct3_t) -> MC -> IO ()))

{-# NOINLINE struct_typedef2_ptr #-}

{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:24:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef2_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct3_t) -> MC -> IO ())
struct_typedef2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_434fb3300450ea3a

{-| __unique:__ @Example_get_struct_typedef3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_6ef76e6cdde5f560" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_6ef76e6cdde5f560 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ()))

{-# NOINLINE struct_typedef3_ptr #-}

{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:25:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef3_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ())
struct_typedef3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_6ef76e6cdde5f560

{-| __unique:__ @Example_get_struct_name1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_89909167992a336a" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_89909167992a336a ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct1) -> MC -> IO ()))

{-# NOINLINE struct_name1_ptr #-}

{-| __C declaration:__ @struct_name1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:27:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name1_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct1) -> MC -> IO ())
struct_name1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_89909167992a336a

{-| __unique:__ @Example_get_struct_name2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_ff58316a7092a3c8" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_ff58316a7092a3c8 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct3) -> MC -> IO ()))

{-# NOINLINE struct_name2_ptr #-}

{-| __C declaration:__ @struct_name2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:28:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name2_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct3) -> MC -> IO ())
struct_name2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_ff58316a7092a3c8

{-| __unique:__ @Example_get_struct_name3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_95e033e6b275b4eb" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_95e033e6b275b4eb ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ()))

{-# NOINLINE struct_name3_ptr #-}

{-| __C declaration:__ @struct_name3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:29:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name3_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ())
struct_name3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_95e033e6b275b4eb
