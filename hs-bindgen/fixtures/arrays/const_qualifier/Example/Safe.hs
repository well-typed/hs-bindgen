{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <arrays/const_qualifier.h>"
  , "void hs_bindgen_41e3579627406714 ("
  , "  signed int const (*arg1)[]"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_ef925a32def7d5e9 ("
  , "  S *arg1"
  , ")"
  , "{"
  , "  fooA(*arg1);"
  , "}"
  , "void hs_bindgen_4237cee8985eb7fc ("
  , "  S const *arg1"
  , ")"
  , "{"
  , "  fooB(*arg1);"
  , "}"
  , "void hs_bindgen_dc3d2325cbc25f6e ("
  , "  T const *arg1"
  , ")"
  , "{"
  , "  fooC(*arg1);"
  , "}"
  , "void hs_bindgen_0820d6c6e877663d ("
  , "  signed int const (*arg1)[3]"
  , ")"
  , "{"
  , "  bar(*arg1);"
  , "}"
  , "void hs_bindgen_bb3ba1c8635c0008 ("
  , "  U *arg1"
  , ")"
  , "{"
  , "  barA(*arg1);"
  , "}"
  , "void hs_bindgen_63794b19284a8cd9 ("
  , "  U const *arg1"
  , ")"
  , "{"
  , "  barB(*arg1);"
  , "}"
  , "void hs_bindgen_e936f5907d7bdf9b ("
  , "  V const *arg1"
  , ")"
  , "{"
  , "  barC(*arg1);"
  , "}"
  , "void hs_bindgen_e4f32da8a7d205db ("
  , "  signed int const (*arg1)[3]"
  , ")"
  , "{"
  , "  baz(arg1);"
  , "}"
  , "void hs_bindgen_abced453716ffe1c ("
  , "  W *arg1"
  , ")"
  , "{"
  , "  bazA(arg1);"
  , "}"
  , "void hs_bindgen_4602d92b6dafcfa7 ("
  , "  W const *arg1"
  , ")"
  , "{"
  , "  bazB(arg1);"
  , "}"
  , "void hs_bindgen_b49b704068741c9c ("
  , "  X const *arg1"
  , ")"
  , "{"
  , "  bazC(arg1);"
  , "}"
  ]))

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_41e3579627406714" hs_bindgen_41e3579627406714_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_foo@
hs_bindgen_41e3579627406714 ::
     PtrConst.PtrConst (IA.IncompleteArray RIP.CInt)
  -> IO ()
hs_bindgen_41e3579627406714 =
  RIP.fromFFIType hs_bindgen_41e3579627406714_base

{-| __C declaration:__ @foo@

    __defined at:__ @arrays\/const_qualifier.h 5:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
foo ::
     PtrConst.PtrConst (IA.IncompleteArray RIP.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_41e3579627406714

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_ef925a32def7d5e9" hs_bindgen_ef925a32def7d5e9_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_fooA@
hs_bindgen_ef925a32def7d5e9 ::
     PtrConst.PtrConst S
  -> IO ()
hs_bindgen_ef925a32def7d5e9 =
  RIP.fromFFIType hs_bindgen_ef925a32def7d5e9_base

{-| __C declaration:__ @fooA@

    __defined at:__ @arrays\/const_qualifier.h 10:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
fooA ::
     PtrConst.PtrConst S
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_ef925a32def7d5e9

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_4237cee8985eb7fc" hs_bindgen_4237cee8985eb7fc_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_fooB@
hs_bindgen_4237cee8985eb7fc ::
     PtrConst.PtrConst S
  -> IO ()
hs_bindgen_4237cee8985eb7fc =
  RIP.fromFFIType hs_bindgen_4237cee8985eb7fc_base

{-| __C declaration:__ @fooB@

    __defined at:__ @arrays\/const_qualifier.h 11:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
fooB ::
     PtrConst.PtrConst S
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_4237cee8985eb7fc

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_dc3d2325cbc25f6e" hs_bindgen_dc3d2325cbc25f6e_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_fooC@
hs_bindgen_dc3d2325cbc25f6e ::
     PtrConst.PtrConst T
  -> IO ()
hs_bindgen_dc3d2325cbc25f6e =
  RIP.fromFFIType hs_bindgen_dc3d2325cbc25f6e_base

{-| __C declaration:__ @fooC@

    __defined at:__ @arrays\/const_qualifier.h 12:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
fooC ::
     PtrConst.PtrConst T
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_dc3d2325cbc25f6e

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_0820d6c6e877663d" hs_bindgen_0820d6c6e877663d_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_bar@
hs_bindgen_0820d6c6e877663d ::
     PtrConst.PtrConst ((CA.ConstantArray 3) RIP.CInt)
  -> IO ()
hs_bindgen_0820d6c6e877663d =
  RIP.fromFFIType hs_bindgen_0820d6c6e877663d_base

{-| __C declaration:__ @bar@

    __defined at:__ @arrays\/const_qualifier.h 16:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
bar ::
     PtrConst.PtrConst ((CA.ConstantArray 3) RIP.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_0820d6c6e877663d

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_barA@
foreign import ccall safe "hs_bindgen_bb3ba1c8635c0008" hs_bindgen_bb3ba1c8635c0008_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_barA@
hs_bindgen_bb3ba1c8635c0008 ::
     PtrConst.PtrConst U
  -> IO ()
hs_bindgen_bb3ba1c8635c0008 =
  RIP.fromFFIType hs_bindgen_bb3ba1c8635c0008_base

{-| __C declaration:__ @barA@

    __defined at:__ @arrays\/const_qualifier.h 21:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
barA ::
     PtrConst.PtrConst U
     -- ^ __C declaration:__ @x@
  -> IO ()
barA = hs_bindgen_bb3ba1c8635c0008

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_barB@
foreign import ccall safe "hs_bindgen_63794b19284a8cd9" hs_bindgen_63794b19284a8cd9_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_barB@
hs_bindgen_63794b19284a8cd9 ::
     PtrConst.PtrConst U
  -> IO ()
hs_bindgen_63794b19284a8cd9 =
  RIP.fromFFIType hs_bindgen_63794b19284a8cd9_base

{-| __C declaration:__ @barB@

    __defined at:__ @arrays\/const_qualifier.h 22:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
barB ::
     PtrConst.PtrConst U
     -- ^ __C declaration:__ @x@
  -> IO ()
barB = hs_bindgen_63794b19284a8cd9

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_barC@
foreign import ccall safe "hs_bindgen_e936f5907d7bdf9b" hs_bindgen_e936f5907d7bdf9b_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_barC@
hs_bindgen_e936f5907d7bdf9b ::
     PtrConst.PtrConst V
  -> IO ()
hs_bindgen_e936f5907d7bdf9b =
  RIP.fromFFIType hs_bindgen_e936f5907d7bdf9b_base

{-| __C declaration:__ @barC@

    __defined at:__ @arrays\/const_qualifier.h 23:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
barC ::
     PtrConst.PtrConst V
     -- ^ __C declaration:__ @x@
  -> IO ()
barC = hs_bindgen_e936f5907d7bdf9b

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_baz@
foreign import ccall safe "hs_bindgen_e4f32da8a7d205db" hs_bindgen_e4f32da8a7d205db_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_baz@
hs_bindgen_e4f32da8a7d205db ::
     PtrConst.PtrConst ((CA.ConstantArray 3) RIP.CInt)
  -> IO ()
hs_bindgen_e4f32da8a7d205db =
  RIP.fromFFIType hs_bindgen_e4f32da8a7d205db_base

{-| __C declaration:__ @baz@

    __defined at:__ @arrays\/const_qualifier.h 27:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
baz ::
     PtrConst.PtrConst ((CA.ConstantArray 3) RIP.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
baz = hs_bindgen_e4f32da8a7d205db

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_bazA@
foreign import ccall safe "hs_bindgen_abced453716ffe1c" hs_bindgen_abced453716ffe1c_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_bazA@
hs_bindgen_abced453716ffe1c ::
     PtrConst.PtrConst W
  -> IO ()
hs_bindgen_abced453716ffe1c =
  RIP.fromFFIType hs_bindgen_abced453716ffe1c_base

{-| __C declaration:__ @bazA@

    __defined at:__ @arrays\/const_qualifier.h 32:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
bazA ::
     PtrConst.PtrConst W
     -- ^ __C declaration:__ @x@
  -> IO ()
bazA = hs_bindgen_abced453716ffe1c

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_bazB@
foreign import ccall safe "hs_bindgen_4602d92b6dafcfa7" hs_bindgen_4602d92b6dafcfa7_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_bazB@
hs_bindgen_4602d92b6dafcfa7 ::
     PtrConst.PtrConst W
  -> IO ()
hs_bindgen_4602d92b6dafcfa7 =
  RIP.fromFFIType hs_bindgen_4602d92b6dafcfa7_base

{-| __C declaration:__ @bazB@

    __defined at:__ @arrays\/const_qualifier.h 33:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
bazB ::
     PtrConst.PtrConst W
     -- ^ __C declaration:__ @x@
  -> IO ()
bazB = hs_bindgen_4602d92b6dafcfa7

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_bazC@
foreign import ccall safe "hs_bindgen_b49b704068741c9c" hs_bindgen_b49b704068741c9c_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_arraysconst_qualifier_Example_Safe_bazC@
hs_bindgen_b49b704068741c9c ::
     PtrConst.PtrConst X
  -> IO ()
hs_bindgen_b49b704068741c9c =
  RIP.fromFFIType hs_bindgen_b49b704068741c9c_base

{-| __C declaration:__ @bazC@

    __defined at:__ @arrays\/const_qualifier.h 34:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
bazC ::
     PtrConst.PtrConst X
     -- ^ __C declaration:__ @x@
  -> IO ()
bazC = hs_bindgen_b49b704068741c9c
