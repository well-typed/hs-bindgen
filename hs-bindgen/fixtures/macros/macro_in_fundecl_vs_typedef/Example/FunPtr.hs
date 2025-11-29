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
  , "/* ExampleNothingget_quux1_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7d7a63ab896ed293 (void)) ("
  , "  MC arg1,"
  , "  TC arg2"
  , ")"
  , "{"
  , "  return &quux1;"
  , "}"
  , "/* ExampleNothingget_quux2_ptr */"
  , "__attribute__ ((const))"
  , "TC (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_b64c564dd7071f5b (void)) ("
  , "  MC arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &quux2;"
  , "}"
  , "/* ExampleNothingget_wam1_ptr */"
  , "__attribute__ ((const))"
  , "MC *(*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_aa26b3a0f4d0aefe (void)) ("
  , "  float arg1,"
  , "  TC *arg2"
  , ")"
  , "{"
  , "  return &wam1;"
  , "}"
  , "/* ExampleNothingget_wam2_ptr */"
  , "__attribute__ ((const))"
  , "TC *(*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_5cb5ead73c0a3d63 (void)) ("
  , "  float arg1,"
  , "  MC *arg2"
  , ")"
  , "{"
  , "  return &wam2;"
  , "}"
  , "/* ExampleNothingget_struct_typedef1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_a1aadeb6878a5152 (void)) ("
  , "  struct2 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_typedef1;"
  , "}"
  , "/* ExampleNothingget_struct_typedef2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_e1dac8a006e6b043 (void)) ("
  , "  struct3_t *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_typedef2;"
  , "}"
  , "/* ExampleNothingget_struct_typedef3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_078075d0a80d4368 (void)) ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_typedef3;"
  , "}"
  , "/* ExampleNothingget_struct_name1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7574edf86480f042 (void)) ("
  , "  struct struct1 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_name1;"
  , "}"
  , "/* ExampleNothingget_struct_name2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_e7a8c1f45f8b20c2 (void)) ("
  , "  struct struct3 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_name2;"
  , "}"
  , "/* ExampleNothingget_struct_name3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_d52310663e8daa5c (void)) ("
  , "  struct struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_name3;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_quux1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7d7a63ab896ed293" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7d7a63ab896ed293 ::
     IO (Ptr.FunPtr (MC -> TC -> IO FC.CChar))

{-# NOINLINE quux1_ptr #-}

{-| __C declaration:__ @quux1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:8:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux1_ptr :: Ptr.FunPtr (MC -> TC -> IO FC.CChar)
quux1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7d7a63ab896ed293

{-| __unique:__ @ExampleNothingget_quux2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_b64c564dd7071f5b" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_b64c564dd7071f5b ::
     IO (Ptr.FunPtr (MC -> FC.CChar -> IO TC))

{-# NOINLINE quux2_ptr #-}

{-| __C declaration:__ @quux2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:9:4@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux2_ptr :: Ptr.FunPtr (MC -> FC.CChar -> IO TC)
quux2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_b64c564dd7071f5b

{-| __unique:__ @ExampleNothingget_wam1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_aa26b3a0f4d0aefe" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_aa26b3a0f4d0aefe ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr TC) -> IO (Ptr.Ptr MC)))

{-# NOINLINE wam1_ptr #-}

{-| __C declaration:__ @wam1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:10:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam1_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr TC) -> IO (Ptr.Ptr MC))
wam1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_aa26b3a0f4d0aefe

{-| __unique:__ @ExampleNothingget_wam2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_5cb5ead73c0a3d63" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_5cb5ead73c0a3d63 ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr MC) -> IO (Ptr.Ptr TC)))

{-# NOINLINE wam2_ptr #-}

{-| __C declaration:__ @wam2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:11:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam2_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr MC) -> IO (Ptr.Ptr TC))
wam2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_5cb5ead73c0a3d63

{-| __unique:__ @ExampleNothingget_struct_typedef1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_a1aadeb6878a5152" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_a1aadeb6878a5152 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct2) -> MC -> IO ()))

{-# NOINLINE struct_typedef1_ptr #-}

{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:23:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef1_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct2) -> MC -> IO ())
struct_typedef1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_a1aadeb6878a5152

{-| __unique:__ @ExampleNothingget_struct_typedef2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_e1dac8a006e6b043" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_e1dac8a006e6b043 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct3_t) -> MC -> IO ()))

{-# NOINLINE struct_typedef2_ptr #-}

{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:24:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef2_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct3_t) -> MC -> IO ())
struct_typedef2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_e1dac8a006e6b043

{-| __unique:__ @ExampleNothingget_struct_typedef3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_078075d0a80d4368" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_078075d0a80d4368 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ()))

{-# NOINLINE struct_typedef3_ptr #-}

{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:25:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef3_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ())
struct_typedef3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_078075d0a80d4368

{-| __unique:__ @ExampleNothingget_struct_name1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7574edf86480f042" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7574edf86480f042 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct1) -> MC -> IO ()))

{-# NOINLINE struct_name1_ptr #-}

{-| __C declaration:__ @struct_name1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:27:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name1_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct1) -> MC -> IO ())
struct_name1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7574edf86480f042

{-| __unique:__ @ExampleNothingget_struct_name2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_e7a8c1f45f8b20c2" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_e7a8c1f45f8b20c2 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct3) -> MC -> IO ()))

{-# NOINLINE struct_name2_ptr #-}

{-| __C declaration:__ @struct_name2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:28:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name2_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct3) -> MC -> IO ())
struct_name2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_e7a8c1f45f8b20c2

{-| __unique:__ @ExampleNothingget_struct_name3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_d52310663e8daa5c" hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_d52310663e8daa5c ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ()))

{-# NOINLINE struct_name3_ptr #-}

{-| __C declaration:__ @struct_name3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:29:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name3_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ())
struct_name3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_d52310663e8daa5c
