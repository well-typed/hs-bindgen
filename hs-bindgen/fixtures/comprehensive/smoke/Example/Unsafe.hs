{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo1
    , Example.Unsafe.foo2
    , Example.Unsafe.foo3
    , Example.Unsafe.foo4
    , Example.Unsafe.foo5
    , Example.Unsafe.foo6
    , Example.Unsafe.foo7
    , Example.Unsafe.foo8
    , Example.Unsafe.foo9
    , Example.Unsafe.foo10
    , Example.Unsafe.foo11
    , Example.Unsafe.foo12
    , Example.Unsafe.foo13
    , Example.Unsafe.foo14
    , Example.Unsafe.foo15
    , Example.Unsafe.foo16
    , Example.Unsafe.foo17
    , Example.Unsafe.foo18
    , Example.Unsafe.foo19
    , Example.Unsafe.foo20
    , Example.Unsafe.foo21
    , Example.Unsafe.foo22
    , Example.Unsafe.foo23
    , Example.Unsafe.foo24
    , Example.Unsafe.foo25
    , Example.Unsafe.foo26
    , Example.Unsafe.foo27
    , Example.Unsafe.foo28
    , Example.Unsafe.inline_foo
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
  , "void hs_bindgen_c3403d27760c03de (void)"
  , "{"
  , "  (foo1)();"
  , "}"
  , "void hs_bindgen_4be46c6638f43029 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo2)(arg1);"
  , "}"
  , "void hs_bindgen_e8661399efa3c25e ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  (foo3)(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_f7977cfca2b865a1 (void)"
  , "{"
  , "  return (foo4)();"
  , "}"
  , "char hs_bindgen_bedb91b426f6e8db ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (foo5)(arg1);"
  , "}"
  , "char *hs_bindgen_2b7a0c438c9bf357 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return (foo6)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_463e49e39aad627b ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (foo7)(arg1);"
  , "}"
  , "char *hs_bindgen_70751b3802485c4e ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (foo8)(arg1);"
  , "}"
  , "char *hs_bindgen_7cda1a20c5ed8c7a ("
  , "  char *(*arg1) (void)"
  , ")"
  , "{"
  , "  return (foo9)(arg1);"
  , "}"
  , "char *hs_bindgen_25ddc364c2fbc486 ("
  , "  char *(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo10)(arg1);"
  , "}"
  , "void *hs_bindgen_59d6c7b752cf0d2c ("
  , "  void *(*arg1) (void)"
  , ")"
  , "{"
  , "  return (foo11)(arg1);"
  , "}"
  , "void *hs_bindgen_6f61d879de93f509 ("
  , "  void *(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo12)(arg1);"
  , "}"
  , "char *hs_bindgen_325eccfd4464f38f ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (foo13)(arg1);"
  , "}"
  , "char *hs_bindgen_e6168413cff5674e ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (foo14)(arg1);"
  , "}"
  , "char *hs_bindgen_0ea84b195eb56e51 ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (foo15)(arg1);"
  , "}"
  , "char *hs_bindgen_46c499f7b2f29dbb ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (foo16)(arg1);"
  , "}"
  , "signed int hs_bindgen_7d34cd671fa1d8c9 ("
  , "  char ***arg1"
  , ")"
  , "{"
  , "  return (foo17)(arg1);"
  , "}"
  , "signed int hs_bindgen_68092727aed54767 ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (foo18)(arg1);"
  , "}"
  , "signed int hs_bindgen_16ecb628ad8bc943 ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (foo19)(arg1);"
  , "}"
  , "signed int hs_bindgen_137b6b359edafeb7 ("
  , "  uint arg1"
  , ")"
  , "{"
  , "  return (foo20)(arg1);"
  , "}"
  , "signed int hs_bindgen_30ddc0dce4dbba94 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo21)(arg1);"
  , "}"
  , "signed int hs_bindgen_27c8fe28c4787aef ("
  , "  signed int *(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo22)(arg1);"
  , "}"
  , "signed int hs_bindgen_b88d57f2bfe36cf8 ("
  , "  signed int **(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo23)(arg1);"
  , "}"
  , "signed int hs_bindgen_257c8c4361081ed9 ("
  , "  signed int ***(*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo24)(arg1);"
  , "}"
  , "signed int *hs_bindgen_401c9920eb162e99 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (foo25)(arg1);"
  , "}"
  , "signed int **hs_bindgen_bf36517f6f9412b1 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (foo26)(arg1);"
  , "}"
  , "signed int ***hs_bindgen_6727bd704a28f329 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (foo27)(arg1);"
  , "}"
  , "signed int ***hs_bindgen_c5bd28320c79f368 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return (foo28)(arg1);"
  , "}"
  , "signed int hs_bindgen_4b6dbdd1ba04d213 ("
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

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo1@
foreign import ccall unsafe "hs_bindgen_c3403d27760c03de" hs_bindgen_c3403d27760c03de_base ::
     IO ()

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo1@
hs_bindgen_c3403d27760c03de :: IO ()
hs_bindgen_c3403d27760c03de =
  RIP.fromFFIType hs_bindgen_c3403d27760c03de_base

{-| __C declaration:__ @foo1@

    __defined at:__ @comprehensive\/smoke.h 10:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo1 :: IO ()
foo1 = hs_bindgen_c3403d27760c03de

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo2@
foreign import ccall unsafe "hs_bindgen_4be46c6638f43029" hs_bindgen_4be46c6638f43029_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo2@
hs_bindgen_4be46c6638f43029 ::
     RIP.CInt
  -> IO ()
hs_bindgen_4be46c6638f43029 =
  RIP.fromFFIType hs_bindgen_4be46c6638f43029_base

{-| __C declaration:__ @foo2@

    __defined at:__ @comprehensive\/smoke.h 11:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo2 ::
     RIP.CInt
  -> IO ()
foo2 = hs_bindgen_4be46c6638f43029

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo3@
foreign import ccall unsafe "hs_bindgen_e8661399efa3c25e" hs_bindgen_e8661399efa3c25e_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo3@
hs_bindgen_e8661399efa3c25e ::
     RIP.CInt
  -> RIP.CInt
  -> IO ()
hs_bindgen_e8661399efa3c25e =
  RIP.fromFFIType hs_bindgen_e8661399efa3c25e_base

{-| __C declaration:__ @foo3@

    __defined at:__ @comprehensive\/smoke.h 12:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo3 ::
     RIP.CInt
  -> RIP.CInt
  -> IO ()
foo3 = hs_bindgen_e8661399efa3c25e

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo4@
foreign import ccall unsafe "hs_bindgen_f7977cfca2b865a1" hs_bindgen_f7977cfca2b865a1_base ::
     IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo4@
hs_bindgen_f7977cfca2b865a1 :: IO RIP.CInt
hs_bindgen_f7977cfca2b865a1 =
  RIP.fromFFIType hs_bindgen_f7977cfca2b865a1_base

{-| __C declaration:__ @foo4@

    __defined at:__ @comprehensive\/smoke.h 13:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo4 :: IO RIP.CInt
foo4 = hs_bindgen_f7977cfca2b865a1

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo5@
foreign import ccall unsafe "hs_bindgen_bedb91b426f6e8db" hs_bindgen_bedb91b426f6e8db_base ::
     RIP.Int32
  -> IO RIP.Int8

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo5@
hs_bindgen_bedb91b426f6e8db ::
     RIP.CInt
  -> IO RIP.CChar
hs_bindgen_bedb91b426f6e8db =
  RIP.fromFFIType hs_bindgen_bedb91b426f6e8db_base

{-| __C declaration:__ @foo5@

    __defined at:__ @comprehensive\/smoke.h 14:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo5 ::
     RIP.CInt
  -> IO RIP.CChar
foo5 = hs_bindgen_bedb91b426f6e8db

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo6@
foreign import ccall unsafe "hs_bindgen_2b7a0c438c9bf357" hs_bindgen_2b7a0c438c9bf357_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo6@
hs_bindgen_2b7a0c438c9bf357 ::
     RIP.CInt
  -> RIP.CInt
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_2b7a0c438c9bf357 =
  RIP.fromFFIType hs_bindgen_2b7a0c438c9bf357_base

{-| __C declaration:__ @foo6@

    __defined at:__ @comprehensive\/smoke.h 15:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo6 ::
     RIP.CInt
  -> RIP.CInt
  -> IO (RIP.Ptr RIP.CChar)
foo6 = hs_bindgen_2b7a0c438c9bf357

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo7@
foreign import ccall unsafe "hs_bindgen_463e49e39aad627b" hs_bindgen_463e49e39aad627b_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo7@
hs_bindgen_463e49e39aad627b ::
     RIP.Ptr RIP.CChar
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_463e49e39aad627b =
  RIP.fromFFIType hs_bindgen_463e49e39aad627b_base

{-| __C declaration:__ @foo7@

    __defined at:__ @comprehensive\/smoke.h 16:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo7 ::
     RIP.Ptr RIP.CChar
  -> IO (RIP.Ptr RIP.CChar)
foo7 = hs_bindgen_463e49e39aad627b

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo8@
foreign import ccall unsafe "hs_bindgen_70751b3802485c4e" hs_bindgen_70751b3802485c4e_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo8@
hs_bindgen_70751b3802485c4e ::
     RIP.Ptr RIP.CChar
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_70751b3802485c4e =
  RIP.fromFFIType hs_bindgen_70751b3802485c4e_base

{-| __C declaration:__ @foo8@

    __defined at:__ @comprehensive\/smoke.h 17:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo8 ::
     RIP.Ptr RIP.CChar
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.CChar)
foo8 = hs_bindgen_70751b3802485c4e

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo9@
foreign import ccall unsafe "hs_bindgen_7cda1a20c5ed8c7a" hs_bindgen_7cda1a20c5ed8c7a_base ::
     RIP.FunPtr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo9@
hs_bindgen_7cda1a20c5ed8c7a ::
     RIP.FunPtr (IO (RIP.Ptr RIP.CChar))
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_7cda1a20c5ed8c7a =
  RIP.fromFFIType hs_bindgen_7cda1a20c5ed8c7a_base

{-| __C declaration:__ @foo9@

    __defined at:__ @comprehensive\/smoke.h 18:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo9 ::
     RIP.FunPtr (IO (RIP.Ptr RIP.CChar))
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.CChar)
foo9 = hs_bindgen_7cda1a20c5ed8c7a

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo10@
foreign import ccall unsafe "hs_bindgen_25ddc364c2fbc486" hs_bindgen_25ddc364c2fbc486_base ::
     RIP.FunPtr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo10@
hs_bindgen_25ddc364c2fbc486 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CChar))
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_25ddc364c2fbc486 =
  RIP.fromFFIType hs_bindgen_25ddc364c2fbc486_base

{-| __C declaration:__ @foo10@

    __defined at:__ @comprehensive\/smoke.h 19:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo10 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CChar))
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.CChar)
foo10 = hs_bindgen_25ddc364c2fbc486

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo11@
foreign import ccall unsafe "hs_bindgen_59d6c7b752cf0d2c" hs_bindgen_59d6c7b752cf0d2c_base ::
     RIP.FunPtr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo11@
hs_bindgen_59d6c7b752cf0d2c ::
     RIP.FunPtr (IO (RIP.Ptr RIP.Void))
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_59d6c7b752cf0d2c =
  RIP.fromFFIType hs_bindgen_59d6c7b752cf0d2c_base

{-| __C declaration:__ @foo11@

    __defined at:__ @comprehensive\/smoke.h 20:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo11 ::
     RIP.FunPtr (IO (RIP.Ptr RIP.Void))
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.Void)
foo11 = hs_bindgen_59d6c7b752cf0d2c

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo12@
foreign import ccall unsafe "hs_bindgen_6f61d879de93f509" hs_bindgen_6f61d879de93f509_base ::
     RIP.FunPtr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo12@
hs_bindgen_6f61d879de93f509 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.Void))
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_6f61d879de93f509 =
  RIP.fromFFIType hs_bindgen_6f61d879de93f509_base

{-| __C declaration:__ @foo12@

    __defined at:__ @comprehensive\/smoke.h 21:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo12 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.Void))
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.Void)
foo12 = hs_bindgen_6f61d879de93f509

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo13@
foreign import ccall unsafe "hs_bindgen_325eccfd4464f38f" hs_bindgen_325eccfd4464f38f_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo13@
hs_bindgen_325eccfd4464f38f ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray RIP.CChar))
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_325eccfd4464f38f =
  RIP.fromFFIType hs_bindgen_325eccfd4464f38f_base

{-| __C declaration:__ @foo13@

    __defined at:__ @comprehensive\/smoke.h 22:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo13 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray RIP.CChar))
  -> IO (RIP.Ptr RIP.CChar)
foo13 = hs_bindgen_325eccfd4464f38f

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo14@
foreign import ccall unsafe "hs_bindgen_e6168413cff5674e" hs_bindgen_e6168413cff5674e_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo14@
hs_bindgen_e6168413cff5674e ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray RIP.CChar))
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_e6168413cff5674e =
  RIP.fromFFIType hs_bindgen_e6168413cff5674e_base

{-| __C declaration:__ @foo14@

    __defined at:__ @comprehensive\/smoke.h 23:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo14 ::
     RIP.Ptr (IsA.Elem (IA.IncompleteArray RIP.CChar))
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.CChar)
foo14 = hs_bindgen_e6168413cff5674e

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo15@
foreign import ccall unsafe "hs_bindgen_0ea84b195eb56e51" hs_bindgen_0ea84b195eb56e51_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo15@
hs_bindgen_0ea84b195eb56e51 ::
     RIP.Ptr (IsA.Elem (CA.ConstantArray 5 RIP.CChar))
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_0ea84b195eb56e51 =
  RIP.fromFFIType hs_bindgen_0ea84b195eb56e51_base

{-| __C declaration:__ @foo15@

    __defined at:__ @comprehensive\/smoke.h 24:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo15 ::
     RIP.Ptr (IsA.Elem (CA.ConstantArray 5 RIP.CChar))
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Ptr RIP.CChar)
foo15 = hs_bindgen_0ea84b195eb56e51

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo16@
foreign import ccall unsafe "hs_bindgen_46c499f7b2f29dbb" hs_bindgen_46c499f7b2f29dbb_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo16@
hs_bindgen_46c499f7b2f29dbb ::
     RIP.CInt
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_46c499f7b2f29dbb =
  RIP.fromFFIType hs_bindgen_46c499f7b2f29dbb_base

{-| __C declaration:__ @foo16@

    __defined at:__ @comprehensive\/smoke.h 25:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo16 ::
     RIP.CInt
  -> IO (RIP.Ptr RIP.CChar)
foo16 = hs_bindgen_46c499f7b2f29dbb

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo17@
foreign import ccall unsafe "hs_bindgen_7d34cd671fa1d8c9" hs_bindgen_7d34cd671fa1d8c9_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo17@
hs_bindgen_7d34cd671fa1d8c9 ::
     RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CChar))
  -> IO RIP.CInt
hs_bindgen_7d34cd671fa1d8c9 =
  RIP.fromFFIType hs_bindgen_7d34cd671fa1d8c9_base

{-| __C declaration:__ @foo17@

    __defined at:__ @comprehensive\/smoke.h 26:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo17 ::
     RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CChar))
  -> IO RIP.CInt
foo17 = hs_bindgen_7d34cd671fa1d8c9

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo18@
foreign import ccall unsafe "hs_bindgen_68092727aed54767" hs_bindgen_68092727aed54767_base ::
     RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo18@
hs_bindgen_68092727aed54767 ::
     RIP.CUInt
  -> IO RIP.CInt
hs_bindgen_68092727aed54767 =
  RIP.fromFFIType hs_bindgen_68092727aed54767_base

{-| __C declaration:__ @foo18@

    __defined at:__ @comprehensive\/smoke.h 27:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo18 ::
     RIP.CUInt
  -> IO RIP.CInt
foo18 = hs_bindgen_68092727aed54767

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo19@
foreign import ccall unsafe "hs_bindgen_16ecb628ad8bc943" hs_bindgen_16ecb628ad8bc943_base ::
     RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo19@
hs_bindgen_16ecb628ad8bc943 ::
     RIP.CUInt
  -> IO RIP.CInt
hs_bindgen_16ecb628ad8bc943 =
  RIP.fromFFIType hs_bindgen_16ecb628ad8bc943_base

{-| __C declaration:__ @foo19@

    __defined at:__ @comprehensive\/smoke.h 28:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo19 ::
     RIP.CUInt
  -> IO RIP.CInt
foo19 = hs_bindgen_16ecb628ad8bc943

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo20@
foreign import ccall unsafe "hs_bindgen_137b6b359edafeb7" hs_bindgen_137b6b359edafeb7_base ::
     RIP.Word32
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo20@
hs_bindgen_137b6b359edafeb7 ::
     Uint
  -> IO RIP.CInt
hs_bindgen_137b6b359edafeb7 =
  RIP.fromFFIType hs_bindgen_137b6b359edafeb7_base

{-| __C declaration:__ @foo20@

    __defined at:__ @comprehensive\/smoke.h 29:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo20 ::
     Uint
  -> IO RIP.CInt
foo20 = hs_bindgen_137b6b359edafeb7

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo21@
foreign import ccall unsafe "hs_bindgen_30ddc0dce4dbba94" hs_bindgen_30ddc0dce4dbba94_base ::
     RIP.FunPtr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo21@
hs_bindgen_30ddc0dce4dbba94 ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
  -> IO RIP.CInt
hs_bindgen_30ddc0dce4dbba94 =
  RIP.fromFFIType hs_bindgen_30ddc0dce4dbba94_base

{-| __C declaration:__ @foo21@

    __defined at:__ @comprehensive\/smoke.h 30:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo21 ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
  -> IO RIP.CInt
foo21 = hs_bindgen_30ddc0dce4dbba94

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo22@
foreign import ccall unsafe "hs_bindgen_27c8fe28c4787aef" hs_bindgen_27c8fe28c4787aef_base ::
     RIP.FunPtr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo22@
hs_bindgen_27c8fe28c4787aef ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_27c8fe28c4787aef =
  RIP.fromFFIType hs_bindgen_27c8fe28c4787aef_base

{-| __C declaration:__ @foo22@

    __defined at:__ @comprehensive\/smoke.h 31:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo22 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CInt))
  -> IO RIP.CInt
foo22 = hs_bindgen_27c8fe28c4787aef

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo23@
foreign import ccall unsafe "hs_bindgen_b88d57f2bfe36cf8" hs_bindgen_b88d57f2bfe36cf8_base ::
     RIP.FunPtr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo23@
hs_bindgen_b88d57f2bfe36cf8 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr RIP.CInt)))
  -> IO RIP.CInt
hs_bindgen_b88d57f2bfe36cf8 =
  RIP.fromFFIType hs_bindgen_b88d57f2bfe36cf8_base

{-| __C declaration:__ @foo23@

    __defined at:__ @comprehensive\/smoke.h 32:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo23 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr RIP.CInt)))
  -> IO RIP.CInt
foo23 = hs_bindgen_b88d57f2bfe36cf8

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo24@
foreign import ccall unsafe "hs_bindgen_257c8c4361081ed9" hs_bindgen_257c8c4361081ed9_base ::
     RIP.FunPtr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo24@
hs_bindgen_257c8c4361081ed9 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt))))
  -> IO RIP.CInt
hs_bindgen_257c8c4361081ed9 =
  RIP.fromFFIType hs_bindgen_257c8c4361081ed9_base

{-| __C declaration:__ @foo24@

    __defined at:__ @comprehensive\/smoke.h 33:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo24 ::
     RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt))))
  -> IO RIP.CInt
foo24 = hs_bindgen_257c8c4361081ed9

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo25@
foreign import ccall unsafe "hs_bindgen_401c9920eb162e99" hs_bindgen_401c9920eb162e99_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo25@
hs_bindgen_401c9920eb162e99 ::
     RIP.CInt
  -> IO (RIP.Ptr RIP.CInt)
hs_bindgen_401c9920eb162e99 =
  RIP.fromFFIType hs_bindgen_401c9920eb162e99_base

{-| __C declaration:__ @foo25@

    __defined at:__ @comprehensive\/smoke.h 34:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo25 ::
     RIP.CInt
  -> IO (RIP.Ptr RIP.CInt)
foo25 = hs_bindgen_401c9920eb162e99

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo26@
foreign import ccall unsafe "hs_bindgen_bf36517f6f9412b1" hs_bindgen_bf36517f6f9412b1_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo26@
hs_bindgen_bf36517f6f9412b1 ::
     RIP.CInt
  -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))
hs_bindgen_bf36517f6f9412b1 =
  RIP.fromFFIType hs_bindgen_bf36517f6f9412b1_base

{-| __C declaration:__ @foo26@

    __defined at:__ @comprehensive\/smoke.h 35:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo26 ::
     RIP.CInt
  -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))
foo26 = hs_bindgen_bf36517f6f9412b1

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo27@
foreign import ccall unsafe "hs_bindgen_6727bd704a28f329" hs_bindgen_6727bd704a28f329_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo27@
hs_bindgen_6727bd704a28f329 ::
     RIP.CInt
  -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))
hs_bindgen_6727bd704a28f329 =
  RIP.fromFFIType hs_bindgen_6727bd704a28f329_base

{-| __C declaration:__ @foo27@

    __defined at:__ @comprehensive\/smoke.h 36:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo27 ::
     RIP.CInt
  -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))
foo27 = hs_bindgen_6727bd704a28f329

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo28@
foreign import ccall unsafe "hs_bindgen_c5bd28320c79f368" hs_bindgen_c5bd28320c79f368_base ::
     RIP.Word64
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_foo28@
hs_bindgen_c5bd28320c79f368 ::
     Size_t
  -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))
hs_bindgen_c5bd28320c79f368 =
  RIP.fromFFIType hs_bindgen_c5bd28320c79f368_base

{-| __C declaration:__ @foo28@

    __defined at:__ @comprehensive\/smoke.h 37:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
foo28 ::
     Size_t
  -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))
foo28 = hs_bindgen_c5bd28320c79f368

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_inline_foo@
foreign import ccall unsafe "hs_bindgen_4b6dbdd1ba04d213" hs_bindgen_4b6dbdd1ba04d213_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> RIP.Int32
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_comprehensivesmoke_Example_Unsafe_inline_foo@
hs_bindgen_4b6dbdd1ba04d213 ::
     RIP.CInt
  -> RIP.Ptr RIP.CInt
  -> RIP.CInt
  -> PtrConst.PtrConst RIP.CInt
  -> RIP.Ptr (PtrConst.PtrConst RIP.CInt)
  -> PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt)
  -> Size_t
  -> IO RIP.CInt
hs_bindgen_4b6dbdd1ba04d213 =
  RIP.fromFFIType hs_bindgen_4b6dbdd1ba04d213_base

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
inline_foo = hs_bindgen_4b6dbdd1ba04d213
