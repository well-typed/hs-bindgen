{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified GHC.Int
import qualified GHC.Word
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/primitives/least_fast.h>"
  , "int_least8_t hs_bindgen_ad6572fbf5eadb77 (void)"
  , "{"
  , "  return int_least8_t_fun();"
  , "}"
  , "int_least16_t hs_bindgen_1b76f016106283f9 (void)"
  , "{"
  , "  return int_least16_t_fun();"
  , "}"
  , "int_least32_t hs_bindgen_6ed0daff627b4e5c (void)"
  , "{"
  , "  return int_least32_t_fun();"
  , "}"
  , "int_least64_t hs_bindgen_3973a552158e924e (void)"
  , "{"
  , "  return int_least64_t_fun();"
  , "}"
  , "uint_least8_t hs_bindgen_8444813dd4e79f7f (void)"
  , "{"
  , "  return uint_least8_t_fun();"
  , "}"
  , "uint_least16_t hs_bindgen_1b71c9bb2bf1881f (void)"
  , "{"
  , "  return uint_least16_t_fun();"
  , "}"
  , "uint_least32_t hs_bindgen_24abaf3275c0126f (void)"
  , "{"
  , "  return uint_least32_t_fun();"
  , "}"
  , "uint_least64_t hs_bindgen_45be86cc87258955 (void)"
  , "{"
  , "  return uint_least64_t_fun();"
  , "}"
  , "int_fast8_t hs_bindgen_223fd78a0ee18a9d (void)"
  , "{"
  , "  return int_fast8_t_fun();"
  , "}"
  , "int_fast16_t hs_bindgen_097db32b72dabc72 (void)"
  , "{"
  , "  return int_fast16_t_fun();"
  , "}"
  , "int_fast32_t hs_bindgen_a406174bb602d355 (void)"
  , "{"
  , "  return int_fast32_t_fun();"
  , "}"
  , "int_fast64_t hs_bindgen_eeb2cb113fbc86ce (void)"
  , "{"
  , "  return int_fast64_t_fun();"
  , "}"
  , "uint_fast8_t hs_bindgen_32aa304dae0c8a3d (void)"
  , "{"
  , "  return uint_fast8_t_fun();"
  , "}"
  , "uint_fast16_t hs_bindgen_81ec605d32f0724d (void)"
  , "{"
  , "  return uint_fast16_t_fun();"
  , "}"
  , "uint_fast32_t hs_bindgen_f20ac50dff139544 (void)"
  , "{"
  , "  return uint_fast32_t_fun();"
  , "}"
  , "uint_fast64_t hs_bindgen_9d5a9bc3a5b3b294 (void)"
  , "{"
  , "  return uint_fast64_t_fun();"
  , "}"
  ]))

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_least8_t_fun@
foreign import ccall safe "hs_bindgen_ad6572fbf5eadb77" hs_bindgen_ad6572fbf5eadb77_base ::
     IO GHC.Int.Int8

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_least8_t_fun@
hs_bindgen_ad6572fbf5eadb77 :: IO Int_least8_t
hs_bindgen_ad6572fbf5eadb77 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ad6572fbf5eadb77_base

{-| __C declaration:__ @int_least8_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 7:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_least8_t_fun :: IO Int_least8_t
int_least8_t_fun = hs_bindgen_ad6572fbf5eadb77

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_least16_t_fun@
foreign import ccall safe "hs_bindgen_1b76f016106283f9" hs_bindgen_1b76f016106283f9_base ::
     IO GHC.Int.Int16

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_least16_t_fun@
hs_bindgen_1b76f016106283f9 :: IO Int_least16_t
hs_bindgen_1b76f016106283f9 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_1b76f016106283f9_base

{-| __C declaration:__ @int_least16_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 8:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_least16_t_fun :: IO Int_least16_t
int_least16_t_fun = hs_bindgen_1b76f016106283f9

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_least32_t_fun@
foreign import ccall safe "hs_bindgen_6ed0daff627b4e5c" hs_bindgen_6ed0daff627b4e5c_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_least32_t_fun@
hs_bindgen_6ed0daff627b4e5c :: IO Int_least32_t
hs_bindgen_6ed0daff627b4e5c =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_6ed0daff627b4e5c_base

{-| __C declaration:__ @int_least32_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 9:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_least32_t_fun :: IO Int_least32_t
int_least32_t_fun = hs_bindgen_6ed0daff627b4e5c

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_least64_t_fun@
foreign import ccall safe "hs_bindgen_3973a552158e924e" hs_bindgen_3973a552158e924e_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_least64_t_fun@
hs_bindgen_3973a552158e924e :: IO Int_least64_t
hs_bindgen_3973a552158e924e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_3973a552158e924e_base

{-| __C declaration:__ @int_least64_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 10:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_least64_t_fun :: IO Int_least64_t
int_least64_t_fun = hs_bindgen_3973a552158e924e

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_least8_t_fun@
foreign import ccall safe "hs_bindgen_8444813dd4e79f7f" hs_bindgen_8444813dd4e79f7f_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_least8_t_fun@
hs_bindgen_8444813dd4e79f7f :: IO Uint_least8_t
hs_bindgen_8444813dd4e79f7f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_8444813dd4e79f7f_base

{-| __C declaration:__ @uint_least8_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 12:16@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_least8_t_fun :: IO Uint_least8_t
uint_least8_t_fun = hs_bindgen_8444813dd4e79f7f

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_least16_t_fun@
foreign import ccall safe "hs_bindgen_1b71c9bb2bf1881f" hs_bindgen_1b71c9bb2bf1881f_base ::
     IO GHC.Word.Word16

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_least16_t_fun@
hs_bindgen_1b71c9bb2bf1881f :: IO Uint_least16_t
hs_bindgen_1b71c9bb2bf1881f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_1b71c9bb2bf1881f_base

{-| __C declaration:__ @uint_least16_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 13:16@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_least16_t_fun :: IO Uint_least16_t
uint_least16_t_fun = hs_bindgen_1b71c9bb2bf1881f

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_least32_t_fun@
foreign import ccall safe "hs_bindgen_24abaf3275c0126f" hs_bindgen_24abaf3275c0126f_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_least32_t_fun@
hs_bindgen_24abaf3275c0126f :: IO Uint_least32_t
hs_bindgen_24abaf3275c0126f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_24abaf3275c0126f_base

{-| __C declaration:__ @uint_least32_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 14:16@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_least32_t_fun :: IO Uint_least32_t
uint_least32_t_fun = hs_bindgen_24abaf3275c0126f

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_least64_t_fun@
foreign import ccall safe "hs_bindgen_45be86cc87258955" hs_bindgen_45be86cc87258955_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_least64_t_fun@
hs_bindgen_45be86cc87258955 :: IO Uint_least64_t
hs_bindgen_45be86cc87258955 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_45be86cc87258955_base

{-| __C declaration:__ @uint_least64_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 15:16@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_least64_t_fun :: IO Uint_least64_t
uint_least64_t_fun = hs_bindgen_45be86cc87258955

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_fast8_t_fun@
foreign import ccall safe "hs_bindgen_223fd78a0ee18a9d" hs_bindgen_223fd78a0ee18a9d_base ::
     IO GHC.Int.Int8

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_fast8_t_fun@
hs_bindgen_223fd78a0ee18a9d :: IO Int_fast8_t
hs_bindgen_223fd78a0ee18a9d =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_223fd78a0ee18a9d_base

{-| __C declaration:__ @int_fast8_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 17:14@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_fast8_t_fun :: IO Int_fast8_t
int_fast8_t_fun = hs_bindgen_223fd78a0ee18a9d

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_fast16_t_fun@
foreign import ccall safe "hs_bindgen_097db32b72dabc72" hs_bindgen_097db32b72dabc72_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_fast16_t_fun@
hs_bindgen_097db32b72dabc72 :: IO Int_fast16_t
hs_bindgen_097db32b72dabc72 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_097db32b72dabc72_base

{-| __C declaration:__ @int_fast16_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 18:14@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_fast16_t_fun :: IO Int_fast16_t
int_fast16_t_fun = hs_bindgen_097db32b72dabc72

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_fast32_t_fun@
foreign import ccall safe "hs_bindgen_a406174bb602d355" hs_bindgen_a406174bb602d355_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_fast32_t_fun@
hs_bindgen_a406174bb602d355 :: IO Int_fast32_t
hs_bindgen_a406174bb602d355 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a406174bb602d355_base

{-| __C declaration:__ @int_fast32_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 19:14@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_fast32_t_fun :: IO Int_fast32_t
int_fast32_t_fun = hs_bindgen_a406174bb602d355

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_fast64_t_fun@
foreign import ccall safe "hs_bindgen_eeb2cb113fbc86ce" hs_bindgen_eeb2cb113fbc86ce_base ::
     IO GHC.Int.Int64

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_int_fast64_t_fun@
hs_bindgen_eeb2cb113fbc86ce :: IO Int_fast64_t
hs_bindgen_eeb2cb113fbc86ce =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_eeb2cb113fbc86ce_base

{-| __C declaration:__ @int_fast64_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 20:14@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
int_fast64_t_fun :: IO Int_fast64_t
int_fast64_t_fun = hs_bindgen_eeb2cb113fbc86ce

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_fast8_t_fun@
foreign import ccall safe "hs_bindgen_32aa304dae0c8a3d" hs_bindgen_32aa304dae0c8a3d_base ::
     IO GHC.Word.Word8

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_fast8_t_fun@
hs_bindgen_32aa304dae0c8a3d :: IO Uint_fast8_t
hs_bindgen_32aa304dae0c8a3d =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_32aa304dae0c8a3d_base

{-| __C declaration:__ @uint_fast8_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 22:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_fast8_t_fun :: IO Uint_fast8_t
uint_fast8_t_fun = hs_bindgen_32aa304dae0c8a3d

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_fast16_t_fun@
foreign import ccall safe "hs_bindgen_81ec605d32f0724d" hs_bindgen_81ec605d32f0724d_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_fast16_t_fun@
hs_bindgen_81ec605d32f0724d :: IO Uint_fast16_t
hs_bindgen_81ec605d32f0724d =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_81ec605d32f0724d_base

{-| __C declaration:__ @uint_fast16_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 23:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_fast16_t_fun :: IO Uint_fast16_t
uint_fast16_t_fun = hs_bindgen_81ec605d32f0724d

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_fast32_t_fun@
foreign import ccall safe "hs_bindgen_f20ac50dff139544" hs_bindgen_f20ac50dff139544_base ::
     IO GHC.Word.Word32

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_fast32_t_fun@
hs_bindgen_f20ac50dff139544 :: IO Uint_fast32_t
hs_bindgen_f20ac50dff139544 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f20ac50dff139544_base

{-| __C declaration:__ @uint_fast32_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 24:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_fast32_t_fun :: IO Uint_fast32_t
uint_fast32_t_fun = hs_bindgen_f20ac50dff139544

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_fast64_t_fun@
foreign import ccall safe "hs_bindgen_9d5a9bc3a5b3b294" hs_bindgen_9d5a9bc3a5b3b294_base ::
     IO GHC.Word.Word64

-- __unique:__ @test_typesprimitivesleast_fast_Example_Safe_uint_fast64_t_fun@
hs_bindgen_9d5a9bc3a5b3b294 :: IO Uint_fast64_t
hs_bindgen_9d5a9bc3a5b3b294 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_9d5a9bc3a5b3b294_base

{-| __C declaration:__ @uint_fast64_t_fun@

    __defined at:__ @types\/primitives\/least_fast.h 25:15@

    __exported by:__ @types\/primitives\/least_fast.h@
-}
uint_fast64_t_fun :: IO Uint_fast64_t
uint_fast64_t_fun = hs_bindgen_9d5a9bc3a5b3b294
