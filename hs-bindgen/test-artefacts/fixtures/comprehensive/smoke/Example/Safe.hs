{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo1
    , Example.Safe.foo2
    , Example.Safe.foo3
    , Example.Safe.foo4
    , Example.Safe.foo5
    , Example.Safe.foo6
    , Example.Safe.foo7
    , Example.Safe.foo8
    , Example.Safe.foo9
    , Example.Safe.foo10
    , Example.Safe.foo11
    , Example.Safe.foo12
    , Example.Safe.foo13
    , Example.Safe.foo14
    , Example.Safe.foo15
    , Example.Safe.foo16
    , Example.Safe.foo17
    , Example.Safe.foo18
    , Example.Safe.foo19
    , Example.Safe.foo20
    , Example.Safe.foo21
    , Example.Safe.foo22
    , Example.Safe.foo23
    , Example.Safe.foo24
    , Example.Safe.foo25
    , Example.Safe.foo26
    , Example.Safe.foo27
    , Example.Safe.foo28
    , Example.Safe.inline_foo
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
  [ "#include <comprehensive/smoke.h>"
  , "void hs_bindgen_2743241662e587e2 (void)"
  , "{"
  , "  (foo1)();"
  , "}"
  , "void hs_bindgen_ca1ea7ed39cf6882 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo2)(arg1);"
  , "}"
  , "void hs_bindgen_f0e8a93d6c4bd770 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (foo3)(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_fce3101db708d25c (void)"
  , "{"
  , "  return (foo4)();"
  , "}"
  , "char hs_bindgen_65f052a80aab9141 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (foo5)(arg1);"
  , "}"
  , "char *hs_bindgen_558967a64195919a ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return (foo6)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_1b78d9ead15e9f30 ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (foo7)(arg1);"
  , "}"
  , "char *hs_bindgen_e7b32da27b5f4414 ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (foo8)(arg1);"
  , "}"
  , "char *hs_bindgen_7f4cc2802608a581 ("
  , "  char *(*arg1) (void)"
  , ")"
  , "{"
  , "  return (foo9)(arg1);"
  , "}"
  , "char *hs_bindgen_9e03d7bc759724b3 ("
  , "  char *(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo10)(arg1);"
  , "}"
  , "void *hs_bindgen_3ec0b6324e11cfb1 ("
  , "  void *(*arg1) (void)"
  , ")"
  , "{"
  , "  return (foo11)(arg1);"
  , "}"
  , "void *hs_bindgen_50dfd1970330be63 ("
  , "  void *(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo12)(arg1);"
  , "}"
  , "char *hs_bindgen_6bee8b08c7819690 ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (foo13)(arg1);"
  , "}"
  , "char *hs_bindgen_a670ae425db6b59c ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (foo14)(arg1);"
  , "}"
  , "char *hs_bindgen_2422a51b7a32f594 ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (foo15)(arg1);"
  , "}"
  , "char *hs_bindgen_cd49431b0bacd0ce ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (foo16)(arg1);"
  , "}"
  , "signed int hs_bindgen_fe100bb790f40421 ("
  , "  char ***arg1"
  , ")"
  , "{"
  , "  return (foo17)(arg1);"
  , "}"
  , "signed int hs_bindgen_be2fdd261963c129 ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (foo18)(arg1);"
  , "}"
  , "signed int hs_bindgen_f13f4f1384741d66 ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (foo19)(arg1);"
  , "}"
  , "signed int hs_bindgen_df4b5257915de35d ("
  , "  uint arg1"
  , ")"
  , "{"
  , "  return (foo20)(arg1);"
  , "}"
  , "signed int hs_bindgen_d938ee9ccc99950c ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo21)(arg1);"
  , "}"
  , "signed int hs_bindgen_aad643346a33f55d ("
  , "  signed int *(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo22)(arg1);"
  , "}"
  , "signed int hs_bindgen_1d8acd1d1684de0c ("
  , "  signed int **(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo23)(arg1);"
  , "}"
  , "signed int hs_bindgen_f754dbb729a16951 ("
  , "  signed int ***(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo24)(arg1);"
  , "}"
  , "signed int *hs_bindgen_bc943db5b2865d6c ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (foo25)(arg1);"
  , "}"
  , "signed int **hs_bindgen_51dc51cb7017f47c ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (foo26)(arg1);"
  , "}"
  , "signed int ***hs_bindgen_f53824a6d084bf14 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (foo27)(arg1);"
  , "}"
  , "signed int ***hs_bindgen_bbc45bfae582ebaa ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return (foo28)(arg1);"
  , "}"
  , "signed int hs_bindgen_258ded5549e4ed29 ("
  , "  signed int arg1,"
  , "  signed int *arg2,"
  , "  signed int const arg3,"
  , "  signed int const *arg4,"
  , "  signed int const **arg5,"
  , "  signed int const *const *arg6,"
  , "  size_t arg7"
  , ")"
  , "{"
  , "  return (inline_foo)(arg1, arg2, arg3, arg4, arg5, arg6, arg7);"
  , "}"
  ]))

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo1@
foreign import ccall safe "hs_bindgen_2743241662e587e2" hs_bindgen_2743241662e587e2_base ::
     IO ()

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo1@
hs_bindgen_2743241662e587e2 :: IO ()
hs_bindgen_2743241662e587e2 =
  BG.fromFFIType hs_bindgen_2743241662e587e2_base

{-| __C declaration:__ @foo1@

    __defined at:__ @comprehensive\/smoke.h 10:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo1 :: IO ()
foo1 = hs_bindgen_2743241662e587e2

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo2@
foreign import ccall safe "hs_bindgen_ca1ea7ed39cf6882" hs_bindgen_ca1ea7ed39cf6882_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo2@
hs_bindgen_ca1ea7ed39cf6882 ::
     BG.CInt
  -> IO ()
hs_bindgen_ca1ea7ed39cf6882 =
  BG.fromFFIType hs_bindgen_ca1ea7ed39cf6882_base

{-| __C declaration:__ @foo2@

    __defined at:__ @comprehensive\/smoke.h 11:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo2 ::
     BG.CInt
  -> IO ()
foo2 = hs_bindgen_ca1ea7ed39cf6882

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo3@
foreign import ccall safe "hs_bindgen_f0e8a93d6c4bd770" hs_bindgen_f0e8a93d6c4bd770_base ::
     BG.Int32
  -> BG.Int32
  -> IO ()

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo3@
hs_bindgen_f0e8a93d6c4bd770 ::
     BG.CInt
  -> BG.CInt
  -> IO ()
hs_bindgen_f0e8a93d6c4bd770 =
  BG.fromFFIType hs_bindgen_f0e8a93d6c4bd770_base

{-| __C declaration:__ @foo3@

    __defined at:__ @comprehensive\/smoke.h 12:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo3 ::
     BG.CInt
  -> BG.CInt
  -> IO ()
foo3 = hs_bindgen_f0e8a93d6c4bd770

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo4@
foreign import ccall safe "hs_bindgen_fce3101db708d25c" hs_bindgen_fce3101db708d25c_base ::
     IO BG.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo4@
hs_bindgen_fce3101db708d25c :: IO BG.CInt
hs_bindgen_fce3101db708d25c =
  BG.fromFFIType hs_bindgen_fce3101db708d25c_base

{-| __C declaration:__ @foo4@

    __defined at:__ @comprehensive\/smoke.h 13:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo4 :: IO BG.CInt
foo4 = hs_bindgen_fce3101db708d25c

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo5@
foreign import ccall safe "hs_bindgen_65f052a80aab9141" hs_bindgen_65f052a80aab9141_base ::
     BG.Int32
  -> IO BG.Int8

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo5@
hs_bindgen_65f052a80aab9141 ::
     BG.CInt
  -> IO BG.CChar
hs_bindgen_65f052a80aab9141 =
  BG.fromFFIType hs_bindgen_65f052a80aab9141_base

{-| __C declaration:__ @foo5@

    __defined at:__ @comprehensive\/smoke.h 14:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo5 ::
     BG.CInt
  -> IO BG.CChar
foo5 = hs_bindgen_65f052a80aab9141

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo6@
foreign import ccall safe "hs_bindgen_558967a64195919a" hs_bindgen_558967a64195919a_base ::
     BG.Int32
  -> BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo6@
hs_bindgen_558967a64195919a ::
     BG.CInt
  -> BG.CInt
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_558967a64195919a =
  BG.fromFFIType hs_bindgen_558967a64195919a_base

{-| __C declaration:__ @foo6@

    __defined at:__ @comprehensive\/smoke.h 15:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo6 ::
     BG.CInt
  -> BG.CInt
  -> IO (BG.Ptr BG.CChar)
foo6 = hs_bindgen_558967a64195919a

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo7@
foreign import ccall safe "hs_bindgen_1b78d9ead15e9f30" hs_bindgen_1b78d9ead15e9f30_base ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo7@
hs_bindgen_1b78d9ead15e9f30 ::
     BG.Ptr BG.CChar
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_1b78d9ead15e9f30 =
  BG.fromFFIType hs_bindgen_1b78d9ead15e9f30_base

{-| __C declaration:__ @foo7@

    __defined at:__ @comprehensive\/smoke.h 16:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo7 ::
     BG.Ptr BG.CChar
  -> IO (BG.Ptr BG.CChar)
foo7 = hs_bindgen_1b78d9ead15e9f30

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo8@
foreign import ccall safe "hs_bindgen_e7b32da27b5f4414" hs_bindgen_e7b32da27b5f4414_base ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo8@
hs_bindgen_e7b32da27b5f4414 ::
     BG.Ptr BG.CChar
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_e7b32da27b5f4414 =
  BG.fromFFIType hs_bindgen_e7b32da27b5f4414_base

{-| __C declaration:__ @foo8@

    __defined at:__ @comprehensive\/smoke.h 17:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo8 ::
     BG.Ptr BG.CChar
     -- ^ __C declaration:__ @b@
  -> IO (BG.Ptr BG.CChar)
foo8 = hs_bindgen_e7b32da27b5f4414

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo9@
foreign import ccall safe "hs_bindgen_7f4cc2802608a581" hs_bindgen_7f4cc2802608a581_base ::
     BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo9@
hs_bindgen_7f4cc2802608a581 ::
     BG.FunPtr (IO (BG.Ptr BG.CChar))
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_7f4cc2802608a581 =
  BG.fromFFIType hs_bindgen_7f4cc2802608a581_base

{-| __C declaration:__ @foo9@

    __defined at:__ @comprehensive\/smoke.h 18:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo9 ::
     BG.FunPtr (IO (BG.Ptr BG.CChar))
     -- ^ __C declaration:__ @b@
  -> IO (BG.Ptr BG.CChar)
foo9 = hs_bindgen_7f4cc2802608a581

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo10@
foreign import ccall safe "hs_bindgen_9e03d7bc759724b3" hs_bindgen_9e03d7bc759724b3_base ::
     BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo10@
hs_bindgen_9e03d7bc759724b3 ::
     BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CChar))
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_9e03d7bc759724b3 =
  BG.fromFFIType hs_bindgen_9e03d7bc759724b3_base

{-| __C declaration:__ @foo10@

    __defined at:__ @comprehensive\/smoke.h 19:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo10 ::
     BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CChar))
     -- ^ __C declaration:__ @b@
  -> IO (BG.Ptr BG.CChar)
foo10 = hs_bindgen_9e03d7bc759724b3

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo11@
foreign import ccall safe "hs_bindgen_3ec0b6324e11cfb1" hs_bindgen_3ec0b6324e11cfb1_base ::
     BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo11@
hs_bindgen_3ec0b6324e11cfb1 ::
     BG.FunPtr (IO (BG.Ptr BG.Void))
  -> IO (BG.Ptr BG.Void)
hs_bindgen_3ec0b6324e11cfb1 =
  BG.fromFFIType hs_bindgen_3ec0b6324e11cfb1_base

{-| __C declaration:__ @foo11@

    __defined at:__ @comprehensive\/smoke.h 20:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo11 ::
     BG.FunPtr (IO (BG.Ptr BG.Void))
     -- ^ __C declaration:__ @b@
  -> IO (BG.Ptr BG.Void)
foo11 = hs_bindgen_3ec0b6324e11cfb1

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo12@
foreign import ccall safe "hs_bindgen_50dfd1970330be63" hs_bindgen_50dfd1970330be63_base ::
     BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo12@
hs_bindgen_50dfd1970330be63 ::
     BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.Void))
  -> IO (BG.Ptr BG.Void)
hs_bindgen_50dfd1970330be63 =
  BG.fromFFIType hs_bindgen_50dfd1970330be63_base

{-| __C declaration:__ @foo12@

    __defined at:__ @comprehensive\/smoke.h 21:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo12 ::
     BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.Void))
     -- ^ __C declaration:__ @b@
  -> IO (BG.Ptr BG.Void)
foo12 = hs_bindgen_50dfd1970330be63

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo13@
foreign import ccall safe "hs_bindgen_6bee8b08c7819690" hs_bindgen_6bee8b08c7819690_base ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo13@
hs_bindgen_6bee8b08c7819690 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CChar))
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_6bee8b08c7819690 =
  BG.fromFFIType hs_bindgen_6bee8b08c7819690_base

{-| __C declaration:__ @foo13@

    __defined at:__ @comprehensive\/smoke.h 22:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo13 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CChar))
  -> IO (BG.Ptr BG.CChar)
foo13 = hs_bindgen_6bee8b08c7819690

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo14@
foreign import ccall safe "hs_bindgen_a670ae425db6b59c" hs_bindgen_a670ae425db6b59c_base ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo14@
hs_bindgen_a670ae425db6b59c ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CChar))
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_a670ae425db6b59c =
  BG.fromFFIType hs_bindgen_a670ae425db6b59c_base

{-| __C declaration:__ @foo14@

    __defined at:__ @comprehensive\/smoke.h 23:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo14 ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CChar))
     -- ^ __C declaration:__ @b@
  -> IO (BG.Ptr BG.CChar)
foo14 = hs_bindgen_a670ae425db6b59c

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo15@
foreign import ccall safe "hs_bindgen_2422a51b7a32f594" hs_bindgen_2422a51b7a32f594_base ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo15@
hs_bindgen_2422a51b7a32f594 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CChar))
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_2422a51b7a32f594 =
  BG.fromFFIType hs_bindgen_2422a51b7a32f594_base

{-| __C declaration:__ @foo15@

    __defined at:__ @comprehensive\/smoke.h 24:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo15 ::
     BG.Ptr (IsA.Elem (CA.ConstantArray 5 BG.CChar))
     -- ^ __C declaration:__ @b@
  -> IO (BG.Ptr BG.CChar)
foo15 = hs_bindgen_2422a51b7a32f594

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo16@
foreign import ccall safe "hs_bindgen_cd49431b0bacd0ce" hs_bindgen_cd49431b0bacd0ce_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo16@
hs_bindgen_cd49431b0bacd0ce ::
     BG.CInt
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_cd49431b0bacd0ce =
  BG.fromFFIType hs_bindgen_cd49431b0bacd0ce_base

{-| __C declaration:__ @foo16@

    __defined at:__ @comprehensive\/smoke.h 25:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo16 ::
     BG.CInt
  -> IO (BG.Ptr BG.CChar)
foo16 = hs_bindgen_cd49431b0bacd0ce

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo17@
foreign import ccall safe "hs_bindgen_fe100bb790f40421" hs_bindgen_fe100bb790f40421_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo17@
hs_bindgen_fe100bb790f40421 ::
     BG.Ptr (BG.Ptr (BG.Ptr BG.CChar))
  -> IO BG.CInt
hs_bindgen_fe100bb790f40421 =
  BG.fromFFIType hs_bindgen_fe100bb790f40421_base

{-| __C declaration:__ @foo17@

    __defined at:__ @comprehensive\/smoke.h 26:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo17 ::
     BG.Ptr (BG.Ptr (BG.Ptr BG.CChar))
  -> IO BG.CInt
foo17 = hs_bindgen_fe100bb790f40421

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo18@
foreign import ccall safe "hs_bindgen_be2fdd261963c129" hs_bindgen_be2fdd261963c129_base ::
     BG.Word32
  -> IO BG.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo18@
hs_bindgen_be2fdd261963c129 ::
     BG.CUInt
  -> IO BG.CInt
hs_bindgen_be2fdd261963c129 =
  BG.fromFFIType hs_bindgen_be2fdd261963c129_base

{-| __C declaration:__ @foo18@

    __defined at:__ @comprehensive\/smoke.h 27:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo18 ::
     BG.CUInt
  -> IO BG.CInt
foo18 = hs_bindgen_be2fdd261963c129

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo19@
foreign import ccall safe "hs_bindgen_f13f4f1384741d66" hs_bindgen_f13f4f1384741d66_base ::
     BG.Word32
  -> IO BG.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo19@
hs_bindgen_f13f4f1384741d66 ::
     BG.CUInt
  -> IO BG.CInt
hs_bindgen_f13f4f1384741d66 =
  BG.fromFFIType hs_bindgen_f13f4f1384741d66_base

{-| __C declaration:__ @foo19@

    __defined at:__ @comprehensive\/smoke.h 28:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo19 ::
     BG.CUInt
  -> IO BG.CInt
foo19 = hs_bindgen_f13f4f1384741d66

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo20@
foreign import ccall safe "hs_bindgen_df4b5257915de35d" hs_bindgen_df4b5257915de35d_base ::
     BG.Word32
  -> IO BG.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo20@
hs_bindgen_df4b5257915de35d ::
     Uint
  -> IO BG.CInt
hs_bindgen_df4b5257915de35d =
  BG.fromFFIType hs_bindgen_df4b5257915de35d_base

{-| __C declaration:__ @foo20@

    __defined at:__ @comprehensive\/smoke.h 29:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo20 ::
     Uint
  -> IO BG.CInt
foo20 = hs_bindgen_df4b5257915de35d

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo21@
foreign import ccall safe "hs_bindgen_d938ee9ccc99950c" hs_bindgen_d938ee9ccc99950c_base ::
     BG.FunPtr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo21@
hs_bindgen_d938ee9ccc99950c ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO BG.CInt
hs_bindgen_d938ee9ccc99950c =
  BG.fromFFIType hs_bindgen_d938ee9ccc99950c_base

{-| __C declaration:__ @foo21@

    __defined at:__ @comprehensive\/smoke.h 30:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo21 ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO BG.CInt
foo21 = hs_bindgen_d938ee9ccc99950c

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo22@
foreign import ccall safe "hs_bindgen_aad643346a33f55d" hs_bindgen_aad643346a33f55d_base ::
     BG.FunPtr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo22@
hs_bindgen_aad643346a33f55d ::
     BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt))
  -> IO BG.CInt
hs_bindgen_aad643346a33f55d =
  BG.fromFFIType hs_bindgen_aad643346a33f55d_base

{-| __C declaration:__ @foo22@

    __defined at:__ @comprehensive\/smoke.h 31:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo22 ::
     BG.FunPtr (BG.CInt -> IO (BG.Ptr BG.CInt))
  -> IO BG.CInt
foo22 = hs_bindgen_aad643346a33f55d

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo23@
foreign import ccall safe "hs_bindgen_1d8acd1d1684de0c" hs_bindgen_1d8acd1d1684de0c_base ::
     BG.FunPtr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo23@
hs_bindgen_1d8acd1d1684de0c ::
     BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt)))
  -> IO BG.CInt
hs_bindgen_1d8acd1d1684de0c =
  BG.fromFFIType hs_bindgen_1d8acd1d1684de0c_base

{-| __C declaration:__ @foo23@

    __defined at:__ @comprehensive\/smoke.h 32:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo23 ::
     BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr BG.CInt)))
  -> IO BG.CInt
foo23 = hs_bindgen_1d8acd1d1684de0c

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo24@
foreign import ccall safe "hs_bindgen_f754dbb729a16951" hs_bindgen_f754dbb729a16951_base ::
     BG.FunPtr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo24@
hs_bindgen_f754dbb729a16951 ::
     BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt))))
  -> IO BG.CInt
hs_bindgen_f754dbb729a16951 =
  BG.fromFFIType hs_bindgen_f754dbb729a16951_base

{-| __C declaration:__ @foo24@

    __defined at:__ @comprehensive\/smoke.h 33:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo24 ::
     BG.FunPtr (BG.CInt -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt))))
  -> IO BG.CInt
foo24 = hs_bindgen_f754dbb729a16951

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo25@
foreign import ccall safe "hs_bindgen_bc943db5b2865d6c" hs_bindgen_bc943db5b2865d6c_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo25@
hs_bindgen_bc943db5b2865d6c ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
hs_bindgen_bc943db5b2865d6c =
  BG.fromFFIType hs_bindgen_bc943db5b2865d6c_base

{-| __C declaration:__ @foo25@

    __defined at:__ @comprehensive\/smoke.h 34:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo25 ::
     BG.CInt
  -> IO (BG.Ptr BG.CInt)
foo25 = hs_bindgen_bc943db5b2865d6c

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo26@
foreign import ccall safe "hs_bindgen_51dc51cb7017f47c" hs_bindgen_51dc51cb7017f47c_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo26@
hs_bindgen_51dc51cb7017f47c ::
     BG.CInt
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_51dc51cb7017f47c =
  BG.fromFFIType hs_bindgen_51dc51cb7017f47c_base

{-| __C declaration:__ @foo26@

    __defined at:__ @comprehensive\/smoke.h 35:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo26 ::
     BG.CInt
  -> IO (BG.Ptr (BG.Ptr BG.CInt))
foo26 = hs_bindgen_51dc51cb7017f47c

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo27@
foreign import ccall safe "hs_bindgen_f53824a6d084bf14" hs_bindgen_f53824a6d084bf14_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo27@
hs_bindgen_f53824a6d084bf14 ::
     BG.CInt
  -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt)))
hs_bindgen_f53824a6d084bf14 =
  BG.fromFFIType hs_bindgen_f53824a6d084bf14_base

{-| __C declaration:__ @foo27@

    __defined at:__ @comprehensive\/smoke.h 36:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo27 ::
     BG.CInt
  -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt)))
foo27 = hs_bindgen_f53824a6d084bf14

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo28@
foreign import ccall safe "hs_bindgen_bbc45bfae582ebaa" hs_bindgen_bbc45bfae582ebaa_base ::
     BG.Word64
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo28@
hs_bindgen_bbc45bfae582ebaa ::
     Size_t
  -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt)))
hs_bindgen_bbc45bfae582ebaa =
  BG.fromFFIType hs_bindgen_bbc45bfae582ebaa_base

{-| __C declaration:__ @foo28@

    __defined at:__ @comprehensive\/smoke.h 37:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo28 ::
     Size_t
  -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt)))
foo28 = hs_bindgen_bbc45bfae582ebaa

-- __unique:__ @test_comprehensivesmoke_Example_Safe_inline_foo@
foreign import ccall safe "hs_bindgen_258ded5549e4ed29" hs_bindgen_258ded5549e4ed29_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> BG.Int32
  -> BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> BG.Word64
  -> IO BG.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_inline_foo@
hs_bindgen_258ded5549e4ed29 ::
     BG.CInt
  -> BG.Ptr BG.CInt
  -> BG.CInt
  -> PtrConst.PtrConst BG.CInt
  -> BG.Ptr (PtrConst.PtrConst BG.CInt)
  -> PtrConst.PtrConst (PtrConst.PtrConst BG.CInt)
  -> Size_t
  -> IO BG.CInt
hs_bindgen_258ded5549e4ed29 =
  BG.fromFFIType hs_bindgen_258ded5549e4ed29_base

{-| __C declaration:__ @inline_foo@

    __defined at:__ @comprehensive\/smoke.h 80:12@

    __exported by:__ @comprehensive\/smoke.h@
-}
inline_foo ::
     BG.CInt
     -- ^ __C declaration:__ @a@
  -> BG.Ptr BG.CInt
     -- ^ __C declaration:__ @b@
  -> BG.CInt
     -- ^ __C declaration:__ @c@
  -> PtrConst.PtrConst BG.CInt
     -- ^ __C declaration:__ @d@
  -> BG.Ptr (PtrConst.PtrConst BG.CInt)
     -- ^ __C declaration:__ @e@
  -> PtrConst.PtrConst (PtrConst.PtrConst BG.CInt)
     -- ^ __C declaration:__ @f@
  -> Size_t
     -- ^ __C declaration:__ @g@
  -> IO BG.CInt
inline_foo = hs_bindgen_258ded5549e4ed29
