{-# LANGUAGE DataKinds #-}
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
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
  RIP.fromFFIType hs_bindgen_2743241662e587e2_base

{-| __C declaration:__ @foo1@

    __defined at:__ @comprehensive\/smoke.h 10:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo1 :: IO ()
foo1 = hs_bindgen_2743241662e587e2

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo2@
foreign import ccall safe "hs_bindgen_ca1ea7ed39cf6882" hs_bindgen_ca1ea7ed39cf6882_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo2@
hs_bindgen_ca1ea7ed39cf6882 ::
     RIP.CInt
  -> IO ()
hs_bindgen_ca1ea7ed39cf6882 =
  RIP.fromFFIType hs_bindgen_ca1ea7ed39cf6882_base

{-| __C declaration:__ @foo2@

    __defined at:__ @comprehensive\/smoke.h 11:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo2 ::
     RIP.CInt
  -> IO ()
foo2 = hs_bindgen_ca1ea7ed39cf6882

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo3@
foreign import ccall safe "hs_bindgen_f0e8a93d6c4bd770" hs_bindgen_f0e8a93d6c4bd770_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo3@
hs_bindgen_f0e8a93d6c4bd770 ::
     RIP.CInt
  -> RIP.CInt
  -> IO ()
hs_bindgen_f0e8a93d6c4bd770 =
  RIP.fromFFIType hs_bindgen_f0e8a93d6c4bd770_base

{-| __C declaration:__ @foo3@

    __defined at:__ @comprehensive\/smoke.h 12:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo3 ::
     RIP.CInt
  -> RIP.CInt
  -> IO ()
foo3 = hs_bindgen_f0e8a93d6c4bd770

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo4@
foreign import ccall safe "hs_bindgen_fce3101db708d25c" hs_bindgen_fce3101db708d25c_base ::
     IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo4@
hs_bindgen_fce3101db708d25c :: IO RIP.CInt
hs_bindgen_fce3101db708d25c =
  RIP.fromFFIType hs_bindgen_fce3101db708d25c_base

{-| __C declaration:__ @foo4@

    __defined at:__ @comprehensive\/smoke.h 13:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo4 :: IO RIP.CInt
foo4 = hs_bindgen_fce3101db708d25c

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo5@
foreign import ccall safe "hs_bindgen_65f052a80aab9141" hs_bindgen_65f052a80aab9141_base ::
     RIP.Int32
  -> IO RIP.Int8

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo5@
hs_bindgen_65f052a80aab9141 ::
     RIP.CInt
  -> IO RIP.CChar
hs_bindgen_65f052a80aab9141 =
  RIP.fromFFIType hs_bindgen_65f052a80aab9141_base

{-| __C declaration:__ @foo5@

    __defined at:__ @comprehensive\/smoke.h 14:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo5 ::
     RIP.CInt
  -> IO RIP.CChar
foo5 = hs_bindgen_65f052a80aab9141

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo6@
foreign import ccall safe "hs_bindgen_558967a64195919a" hs_bindgen_558967a64195919a_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo6@
hs_bindgen_558967a64195919a ::
     RIP.CInt
  -> RIP.CInt
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_558967a64195919a =
  RIP.fromFFIType hs_bindgen_558967a64195919a_base

{-| __C declaration:__ @foo6@

    __defined at:__ @comprehensive\/smoke.h 15:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo6 ::
     RIP.CInt
  -> RIP.CInt
  -> IO (RIP.Ptr RIP.CChar)
foo6 = hs_bindgen_558967a64195919a

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo7@
foreign import ccall safe "hs_bindgen_1b78d9ead15e9f30" hs_bindgen_1b78d9ead15e9f30_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo7@
hs_bindgen_1b78d9ead15e9f30 ::
     RIP.Ptr RIP.CChar
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_1b78d9ead15e9f30 =
  RIP.fromFFIType hs_bindgen_1b78d9ead15e9f30_base

{-| __C declaration:__ @foo7@

    __defined at:__ @comprehensive\/smoke.h 16:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo7 ::
     RIP.Ptr RIP.CChar
  -> IO (RIP.Ptr RIP.CChar)
foo7 = hs_bindgen_1b78d9ead15e9f30

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo8@
foreign import ccall safe "hs_bindgen_e7b32da27b5f4414" hs_bindgen_e7b32da27b5f4414_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo8@
hs_bindgen_e7b32da27b5f4414 ::
     RIP.Ptr RIP.CChar
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_e7b32da27b5f4414 =
  RIP.fromFFIType hs_bindgen_e7b32da27b5f4414_base

{-| __C declaration:__ @foo8@

    __defined at:__ @comprehensive\/smoke.h 17:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo8 ::
     RIP.Ptr RIP.CChar
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.CChar)
foo8 = hs_bindgen_e7b32da27b5f4414

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo9@
foreign import ccall safe "hs_bindgen_7f4cc2802608a581" hs_bindgen_7f4cc2802608a581_base ::
     RIP.FunPtr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo9@
hs_bindgen_7f4cc2802608a581 ::
     RIP.FunPtr (IO (RIP.Ptr RIP.CChar))
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_7f4cc2802608a581 =
  RIP.fromFFIType hs_bindgen_7f4cc2802608a581_base

{-| __C declaration:__ @foo9@

    __defined at:__ @comprehensive\/smoke.h 18:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo9 ::
     RIP.FunPtr (IO (RIP.Ptr RIP.CChar))
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.CChar)
foo9 = hs_bindgen_7f4cc2802608a581

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo10@
foreign import ccall safe "hs_bindgen_9e03d7bc759724b3" hs_bindgen_9e03d7bc759724b3_base ::
     RIP.FunPtr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo10@
hs_bindgen_9e03d7bc759724b3 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CChar))
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_9e03d7bc759724b3 =
  RIP.fromFFIType hs_bindgen_9e03d7bc759724b3_base

{-| __C declaration:__ @foo10@

    __defined at:__ @comprehensive\/smoke.h 19:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo10 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CChar))
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.CChar)
foo10 = hs_bindgen_9e03d7bc759724b3

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo11@
foreign import ccall safe "hs_bindgen_3ec0b6324e11cfb1" hs_bindgen_3ec0b6324e11cfb1_base ::
     RIP.FunPtr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo11@
hs_bindgen_3ec0b6324e11cfb1 ::
     RIP.FunPtr (IO (RIP.Ptr RIP.Void))
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_3ec0b6324e11cfb1 =
  RIP.fromFFIType hs_bindgen_3ec0b6324e11cfb1_base

{-| __C declaration:__ @foo11@

    __defined at:__ @comprehensive\/smoke.h 20:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo11 ::
     RIP.FunPtr (IO (RIP.Ptr RIP.Void))
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.Void)
foo11 = hs_bindgen_3ec0b6324e11cfb1

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo12@
foreign import ccall safe "hs_bindgen_50dfd1970330be63" hs_bindgen_50dfd1970330be63_base ::
     RIP.FunPtr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo12@
hs_bindgen_50dfd1970330be63 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.Void))
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_50dfd1970330be63 =
  RIP.fromFFIType hs_bindgen_50dfd1970330be63_base

{-| __C declaration:__ @foo12@

    __defined at:__ @comprehensive\/smoke.h 21:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo12 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.Void))
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.Void)
foo12 = hs_bindgen_50dfd1970330be63

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo13@
foreign import ccall safe "hs_bindgen_6bee8b08c7819690" hs_bindgen_6bee8b08c7819690_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo13@
hs_bindgen_6bee8b08c7819690 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray RIP.CChar))
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_6bee8b08c7819690 =
  RIP.fromFFIType hs_bindgen_6bee8b08c7819690_base

{-| __C declaration:__ @foo13@

    __defined at:__ @comprehensive\/smoke.h 22:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo13 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray RIP.CChar))
  -> IO (RIP.Ptr RIP.CChar)
foo13 = hs_bindgen_6bee8b08c7819690

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo14@
foreign import ccall safe "hs_bindgen_a670ae425db6b59c" hs_bindgen_a670ae425db6b59c_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo14@
hs_bindgen_a670ae425db6b59c ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray RIP.CChar))
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_a670ae425db6b59c =
  RIP.fromFFIType hs_bindgen_a670ae425db6b59c_base

{-| __C declaration:__ @foo14@

    __defined at:__ @comprehensive\/smoke.h 23:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo14 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray RIP.CChar))
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.CChar)
foo14 = hs_bindgen_a670ae425db6b59c

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo15@
foreign import ccall safe "hs_bindgen_2422a51b7a32f594" hs_bindgen_2422a51b7a32f594_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo15@
hs_bindgen_2422a51b7a32f594 ::
     RIP.Ptr (IsA.Elem (CA.ConstantArray 5 RIP.CChar))
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_2422a51b7a32f594 =
  RIP.fromFFIType hs_bindgen_2422a51b7a32f594_base

{-| __C declaration:__ @foo15@

    __defined at:__ @comprehensive\/smoke.h 24:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo15 ::
     RIP.Ptr (IsA.Elem (CA.ConstantArray 5 RIP.CChar))
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.CChar)
foo15 = hs_bindgen_2422a51b7a32f594

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo16@
foreign import ccall safe "hs_bindgen_cd49431b0bacd0ce" hs_bindgen_cd49431b0bacd0ce_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo16@
hs_bindgen_cd49431b0bacd0ce ::
     RIP.CInt
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_cd49431b0bacd0ce =
  RIP.fromFFIType hs_bindgen_cd49431b0bacd0ce_base

{-| __C declaration:__ @foo16@

    __defined at:__ @comprehensive\/smoke.h 25:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo16 ::
     RIP.CInt
  -> IO (RIP.Ptr RIP.CChar)
foo16 = hs_bindgen_cd49431b0bacd0ce

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo17@
foreign import ccall safe "hs_bindgen_fe100bb790f40421" hs_bindgen_fe100bb790f40421_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo17@
hs_bindgen_fe100bb790f40421 ::
     RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CChar))
  -> IO RIP.CInt
hs_bindgen_fe100bb790f40421 =
  RIP.fromFFIType hs_bindgen_fe100bb790f40421_base

{-| __C declaration:__ @foo17@

    __defined at:__ @comprehensive\/smoke.h 26:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo17 ::
     RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CChar))
  -> IO RIP.CInt
foo17 = hs_bindgen_fe100bb790f40421

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo18@
foreign import ccall safe "hs_bindgen_be2fdd261963c129" hs_bindgen_be2fdd261963c129_base ::
     RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo18@
hs_bindgen_be2fdd261963c129 ::
     RIP.CUInt
  -> IO RIP.CInt
hs_bindgen_be2fdd261963c129 =
  RIP.fromFFIType hs_bindgen_be2fdd261963c129_base

{-| __C declaration:__ @foo18@

    __defined at:__ @comprehensive\/smoke.h 27:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo18 ::
     RIP.CUInt
  -> IO RIP.CInt
foo18 = hs_bindgen_be2fdd261963c129

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo19@
foreign import ccall safe "hs_bindgen_f13f4f1384741d66" hs_bindgen_f13f4f1384741d66_base ::
     RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo19@
hs_bindgen_f13f4f1384741d66 ::
     RIP.CUInt
  -> IO RIP.CInt
hs_bindgen_f13f4f1384741d66 =
  RIP.fromFFIType hs_bindgen_f13f4f1384741d66_base

{-| __C declaration:__ @foo19@

    __defined at:__ @comprehensive\/smoke.h 28:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo19 ::
     RIP.CUInt
  -> IO RIP.CInt
foo19 = hs_bindgen_f13f4f1384741d66

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo20@
foreign import ccall safe "hs_bindgen_df4b5257915de35d" hs_bindgen_df4b5257915de35d_base ::
     RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo20@
hs_bindgen_df4b5257915de35d ::
     Uint
  -> IO RIP.CInt
hs_bindgen_df4b5257915de35d =
  RIP.fromFFIType hs_bindgen_df4b5257915de35d_base

{-| __C declaration:__ @foo20@

    __defined at:__ @comprehensive\/smoke.h 29:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo20 ::
     Uint
  -> IO RIP.CInt
foo20 = hs_bindgen_df4b5257915de35d

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo21@
foreign import ccall safe "hs_bindgen_d938ee9ccc99950c" hs_bindgen_d938ee9ccc99950c_base ::
     RIP.FunPtr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo21@
hs_bindgen_d938ee9ccc99950c ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
  -> IO RIP.CInt
hs_bindgen_d938ee9ccc99950c =
  RIP.fromFFIType hs_bindgen_d938ee9ccc99950c_base

{-| __C declaration:__ @foo21@

    __defined at:__ @comprehensive\/smoke.h 30:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo21 ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
  -> IO RIP.CInt
foo21 = hs_bindgen_d938ee9ccc99950c

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo22@
foreign import ccall safe "hs_bindgen_aad643346a33f55d" hs_bindgen_aad643346a33f55d_base ::
     RIP.FunPtr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo22@
hs_bindgen_aad643346a33f55d ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_aad643346a33f55d =
  RIP.fromFFIType hs_bindgen_aad643346a33f55d_base

{-| __C declaration:__ @foo22@

    __defined at:__ @comprehensive\/smoke.h 31:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo22 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CInt))
  -> IO RIP.CInt
foo22 = hs_bindgen_aad643346a33f55d

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo23@
foreign import ccall safe "hs_bindgen_1d8acd1d1684de0c" hs_bindgen_1d8acd1d1684de0c_base ::
     RIP.FunPtr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo23@
hs_bindgen_1d8acd1d1684de0c ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr RIP.CInt)))
  -> IO RIP.CInt
hs_bindgen_1d8acd1d1684de0c =
  RIP.fromFFIType hs_bindgen_1d8acd1d1684de0c_base

{-| __C declaration:__ @foo23@

    __defined at:__ @comprehensive\/smoke.h 32:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo23 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr RIP.CInt)))
  -> IO RIP.CInt
foo23 = hs_bindgen_1d8acd1d1684de0c

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo24@
foreign import ccall safe "hs_bindgen_f754dbb729a16951" hs_bindgen_f754dbb729a16951_base ::
     RIP.FunPtr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo24@
hs_bindgen_f754dbb729a16951 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt))))
  -> IO RIP.CInt
hs_bindgen_f754dbb729a16951 =
  RIP.fromFFIType hs_bindgen_f754dbb729a16951_base

{-| __C declaration:__ @foo24@

    __defined at:__ @comprehensive\/smoke.h 33:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo24 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt))))
  -> IO RIP.CInt
foo24 = hs_bindgen_f754dbb729a16951

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo25@
foreign import ccall safe "hs_bindgen_bc943db5b2865d6c" hs_bindgen_bc943db5b2865d6c_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo25@
hs_bindgen_bc943db5b2865d6c ::
     RIP.CInt
  -> IO (RIP.Ptr RIP.CInt)
hs_bindgen_bc943db5b2865d6c =
  RIP.fromFFIType hs_bindgen_bc943db5b2865d6c_base

{-| __C declaration:__ @foo25@

    __defined at:__ @comprehensive\/smoke.h 34:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo25 ::
     RIP.CInt
  -> IO (RIP.Ptr RIP.CInt)
foo25 = hs_bindgen_bc943db5b2865d6c

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo26@
foreign import ccall safe "hs_bindgen_51dc51cb7017f47c" hs_bindgen_51dc51cb7017f47c_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo26@
hs_bindgen_51dc51cb7017f47c ::
     RIP.CInt
  -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))
hs_bindgen_51dc51cb7017f47c =
  RIP.fromFFIType hs_bindgen_51dc51cb7017f47c_base

{-| __C declaration:__ @foo26@

    __defined at:__ @comprehensive\/smoke.h 35:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo26 ::
     RIP.CInt
  -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))
foo26 = hs_bindgen_51dc51cb7017f47c

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo27@
foreign import ccall safe "hs_bindgen_f53824a6d084bf14" hs_bindgen_f53824a6d084bf14_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo27@
hs_bindgen_f53824a6d084bf14 ::
     RIP.CInt
  -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))
hs_bindgen_f53824a6d084bf14 =
  RIP.fromFFIType hs_bindgen_f53824a6d084bf14_base

{-| __C declaration:__ @foo27@

    __defined at:__ @comprehensive\/smoke.h 36:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo27 ::
     RIP.CInt
  -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))
foo27 = hs_bindgen_f53824a6d084bf14

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo28@
foreign import ccall safe "hs_bindgen_bbc45bfae582ebaa" hs_bindgen_bbc45bfae582ebaa_base ::
     RIP.Word64
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Safe_foo28@
hs_bindgen_bbc45bfae582ebaa ::
     Size_t
  -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))
hs_bindgen_bbc45bfae582ebaa =
  RIP.fromFFIType hs_bindgen_bbc45bfae582ebaa_base

{-| __C declaration:__ @foo28@

    __defined at:__ @comprehensive\/smoke.h 37:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo28 ::
     Size_t
  -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))
foo28 = hs_bindgen_bbc45bfae582ebaa

-- __unique:__ @test_comprehensivesmoke_Example_Safe_inline_foo@
foreign import ccall safe "hs_bindgen_258ded5549e4ed29" hs_bindgen_258ded5549e4ed29_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Safe_inline_foo@
hs_bindgen_258ded5549e4ed29 ::
     RIP.CInt
  -> RIP.Ptr RIP.CInt
  -> RIP.CInt
  -> PtrConst.PtrConst RIP.CInt
  -> RIP.Ptr (PtrConst.PtrConst RIP.CInt)
  -> PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt)
  -> Size_t
  -> IO RIP.CInt
hs_bindgen_258ded5549e4ed29 =
  RIP.fromFFIType hs_bindgen_258ded5549e4ed29_base

{-| __C declaration:__ @inline_foo@

    __defined at:__ @comprehensive\/smoke.h 80:12@

    __exported by:__ @comprehensive\/smoke.h@
-}
inline_foo ::
     RIP.CInt
     -- ^ __C declaration:__ @a@
  -> RIP.Ptr RIP.CInt
     -- ^ __C declaration:__ @b@
  -> RIP.CInt
     -- ^ __C declaration:__ @c@
  -> PtrConst.PtrConst RIP.CInt
     -- ^ __C declaration:__ @d@
  -> RIP.Ptr (PtrConst.PtrConst RIP.CInt)
     -- ^ __C declaration:__ @e@
  -> PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt)
     -- ^ __C declaration:__ @f@
  -> Size_t
     -- ^ __C declaration:__ @g@
  -> IO RIP.CInt
inline_foo = hs_bindgen_258ded5549e4ed29
