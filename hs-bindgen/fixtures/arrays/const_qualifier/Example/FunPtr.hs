{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.PtrConst
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <arrays/const_qualifier.h>"
  , "/* test_arraysconst_qualifier_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_08ac8ef9de29103e (void)) ("
  , "  signed int const arg1[]"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_arraysconst_qualifier_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_54fbf41deabb186a (void)) ("
  , "  S arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_arraysconst_qualifier_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_639b20c53a422f2a (void)) ("
  , "  S const arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_arraysconst_qualifier_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fef335c60d51fed2 (void)) ("
  , "  T const arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_arraysconst_qualifier_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_20a55b314c10f7b0 (void)) ("
  , "  signed int const arg1[3]"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_arraysconst_qualifier_Example_get_barA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d75c925bc91f45d8 (void)) ("
  , "  U arg1"
  , ")"
  , "{"
  , "  return &barA;"
  , "}"
  , "/* test_arraysconst_qualifier_Example_get_barB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_aecd16e35ff42812 (void)) ("
  , "  U const arg1"
  , ")"
  , "{"
  , "  return &barB;"
  , "}"
  , "/* test_arraysconst_qualifier_Example_get_barC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7b514bc063bee42a (void)) ("
  , "  V const arg1"
  , ")"
  , "{"
  , "  return &barC;"
  , "}"
  , "/* test_arraysconst_qualifier_Example_get_baz */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4839eb9bacfd303d (void)) ("
  , "  signed int const (*arg1)[3]"
  , ")"
  , "{"
  , "  return &baz;"
  , "}"
  , "/* test_arraysconst_qualifier_Example_get_bazA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0ada0d6bbdb28946 (void)) ("
  , "  W *arg1"
  , ")"
  , "{"
  , "  return &bazA;"
  , "}"
  , "/* test_arraysconst_qualifier_Example_get_bazB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d035691c9268d21a (void)) ("
  , "  W const *arg1"
  , ")"
  , "{"
  , "  return &bazB;"
  , "}"
  , "/* test_arraysconst_qualifier_Example_get_bazC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fc2dfbe62c6dc8b1 (void)) ("
  , "  X const *arg1"
  , ")"
  , "{"
  , "  return &bazC;"
  , "}"
  ]))

-- __unique:__ @test_arraysconst_qualifier_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_08ac8ef9de29103e" hs_bindgen_08ac8ef9de29103e_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_arraysconst_qualifier_Example_get_foo@
hs_bindgen_08ac8ef9de29103e :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO ()))
hs_bindgen_08ac8ef9de29103e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_08ac8ef9de29103e_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @arrays\/const_qualifier.h 5:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
foo :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_08ac8ef9de29103e

-- __unique:__ @test_arraysconst_qualifier_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_54fbf41deabb186a" hs_bindgen_54fbf41deabb186a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_arraysconst_qualifier_Example_get_fooA@
hs_bindgen_54fbf41deabb186a :: IO (Ptr.FunPtr (S -> IO ()))
hs_bindgen_54fbf41deabb186a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_54fbf41deabb186a_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @arrays\/const_qualifier.h 10:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
fooA :: Ptr.FunPtr (S -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_54fbf41deabb186a

-- __unique:__ @test_arraysconst_qualifier_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_639b20c53a422f2a" hs_bindgen_639b20c53a422f2a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_arraysconst_qualifier_Example_get_fooB@
hs_bindgen_639b20c53a422f2a :: IO (Ptr.FunPtr (S -> IO ()))
hs_bindgen_639b20c53a422f2a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_639b20c53a422f2a_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @arrays\/const_qualifier.h 11:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
fooB :: Ptr.FunPtr (S -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_639b20c53a422f2a

-- __unique:__ @test_arraysconst_qualifier_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_fef335c60d51fed2" hs_bindgen_fef335c60d51fed2_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_arraysconst_qualifier_Example_get_fooC@
hs_bindgen_fef335c60d51fed2 :: IO (Ptr.FunPtr (T -> IO ()))
hs_bindgen_fef335c60d51fed2 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_fef335c60d51fed2_base

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @arrays\/const_qualifier.h 12:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
fooC :: Ptr.FunPtr (T -> IO ())
fooC =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fef335c60d51fed2

-- __unique:__ @test_arraysconst_qualifier_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_20a55b314c10f7b0" hs_bindgen_20a55b314c10f7b0_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_arraysconst_qualifier_Example_get_bar@
hs_bindgen_20a55b314c10f7b0 :: IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO ()))
hs_bindgen_20a55b314c10f7b0 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_20a55b314c10f7b0_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @arrays\/const_qualifier.h 16:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
bar :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO ())
bar =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_20a55b314c10f7b0

-- __unique:__ @test_arraysconst_qualifier_Example_get_barA@
foreign import ccall unsafe "hs_bindgen_d75c925bc91f45d8" hs_bindgen_d75c925bc91f45d8_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_arraysconst_qualifier_Example_get_barA@
hs_bindgen_d75c925bc91f45d8 :: IO (Ptr.FunPtr (U -> IO ()))
hs_bindgen_d75c925bc91f45d8 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d75c925bc91f45d8_base

{-# NOINLINE barA #-}
{-| __C declaration:__ @barA@

    __defined at:__ @arrays\/const_qualifier.h 21:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
barA :: Ptr.FunPtr (U -> IO ())
barA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d75c925bc91f45d8

-- __unique:__ @test_arraysconst_qualifier_Example_get_barB@
foreign import ccall unsafe "hs_bindgen_aecd16e35ff42812" hs_bindgen_aecd16e35ff42812_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_arraysconst_qualifier_Example_get_barB@
hs_bindgen_aecd16e35ff42812 :: IO (Ptr.FunPtr (U -> IO ()))
hs_bindgen_aecd16e35ff42812 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_aecd16e35ff42812_base

{-# NOINLINE barB #-}
{-| __C declaration:__ @barB@

    __defined at:__ @arrays\/const_qualifier.h 22:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
barB :: Ptr.FunPtr (U -> IO ())
barB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aecd16e35ff42812

-- __unique:__ @test_arraysconst_qualifier_Example_get_barC@
foreign import ccall unsafe "hs_bindgen_7b514bc063bee42a" hs_bindgen_7b514bc063bee42a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_arraysconst_qualifier_Example_get_barC@
hs_bindgen_7b514bc063bee42a :: IO (Ptr.FunPtr (V -> IO ()))
hs_bindgen_7b514bc063bee42a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_7b514bc063bee42a_base

{-# NOINLINE barC #-}
{-| __C declaration:__ @barC@

    __defined at:__ @arrays\/const_qualifier.h 23:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
barC :: Ptr.FunPtr (V -> IO ())
barC =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7b514bc063bee42a

-- __unique:__ @test_arraysconst_qualifier_Example_get_baz@
foreign import ccall unsafe "hs_bindgen_4839eb9bacfd303d" hs_bindgen_4839eb9bacfd303d_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_arraysconst_qualifier_Example_get_baz@
hs_bindgen_4839eb9bacfd303d :: IO (Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO ()))
hs_bindgen_4839eb9bacfd303d =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4839eb9bacfd303d_base

{-# NOINLINE baz #-}
{-| __C declaration:__ @baz@

    __defined at:__ @arrays\/const_qualifier.h 27:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
baz :: Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO ())
baz =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4839eb9bacfd303d

-- __unique:__ @test_arraysconst_qualifier_Example_get_bazA@
foreign import ccall unsafe "hs_bindgen_0ada0d6bbdb28946" hs_bindgen_0ada0d6bbdb28946_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_arraysconst_qualifier_Example_get_bazA@
hs_bindgen_0ada0d6bbdb28946 :: IO (Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst W) -> IO ()))
hs_bindgen_0ada0d6bbdb28946 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0ada0d6bbdb28946_base

{-# NOINLINE bazA #-}
{-| __C declaration:__ @bazA@

    __defined at:__ @arrays\/const_qualifier.h 32:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
bazA :: Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst W) -> IO ())
bazA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0ada0d6bbdb28946

-- __unique:__ @test_arraysconst_qualifier_Example_get_bazB@
foreign import ccall unsafe "hs_bindgen_d035691c9268d21a" hs_bindgen_d035691c9268d21a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_arraysconst_qualifier_Example_get_bazB@
hs_bindgen_d035691c9268d21a :: IO (Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst W) -> IO ()))
hs_bindgen_d035691c9268d21a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d035691c9268d21a_base

{-# NOINLINE bazB #-}
{-| __C declaration:__ @bazB@

    __defined at:__ @arrays\/const_qualifier.h 33:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
bazB :: Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst W) -> IO ())
bazB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d035691c9268d21a

-- __unique:__ @test_arraysconst_qualifier_Example_get_bazC@
foreign import ccall unsafe "hs_bindgen_fc2dfbe62c6dc8b1" hs_bindgen_fc2dfbe62c6dc8b1_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_arraysconst_qualifier_Example_get_bazC@
hs_bindgen_fc2dfbe62c6dc8b1 :: IO (Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst X) -> IO ()))
hs_bindgen_fc2dfbe62c6dc8b1 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_fc2dfbe62c6dc8b1_base

{-# NOINLINE bazC #-}
{-| __C declaration:__ @bazC@

    __defined at:__ @arrays\/const_qualifier.h 34:6@

    __exported by:__ @arrays\/const_qualifier.h@
-}
bazC :: Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst X) -> IO ())
bazC =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fc2dfbe62c6dc8b1
