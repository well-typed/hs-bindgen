{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Ver.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Ds
import qualified RPM.Types
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmver.h>"
  , "/* get_rpmvercmp_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_d8fb97b8c9b6f01b (void)) ("
  , "  char const *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &rpmvercmp;"
  , "}"
  , "/* get_rpmverParse_ptr */"
  , "__attribute__ ((const))"
  , "rpmver (*hs_bindgen_a0e5c85c66ad161e (void)) ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return &rpmverParse;"
  , "}"
  , "/* get_rpmverNew_ptr */"
  , "__attribute__ ((const))"
  , "rpmver (*hs_bindgen_32d0b48dac163fd8 (void)) ("
  , "  char const *arg1,"
  , "  char const *arg2,"
  , "  char const *arg3"
  , ")"
  , "{"
  , "  return &rpmverNew;"
  , "}"
  , "/* get_rpmverFree_ptr */"
  , "__attribute__ ((const))"
  , "rpmver (*hs_bindgen_cc8bed62bfcbb206 (void)) ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return &rpmverFree;"
  , "}"
  , "/* get_rpmverEVal_ptr */"
  , "__attribute__ ((const))"
  , "uint32_t (*hs_bindgen_94bf432525736bb9 (void)) ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return &rpmverEVal;"
  , "}"
  , "/* get_rpmverE_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_b8fe24a031131c3e (void)) ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return &rpmverE;"
  , "}"
  , "/* get_rpmverV_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_beb87f92668dc4c5 (void)) ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return &rpmverV;"
  , "}"
  , "/* get_rpmverR_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_b1209811edb59574 (void)) ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return &rpmverR;"
  , "}"
  , "/* get_rpmverEVR_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_4c72ea6ec02c3e8d (void)) ("
  , "  rpmver arg1"
  , ")"
  , "{"
  , "  return &rpmverEVR;"
  , "}"
  , "/* get_rpmverCmp_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9469a0b62ccaea30 (void)) ("
  , "  rpmver arg1,"
  , "  rpmver arg2"
  , ")"
  , "{"
  , "  return &rpmverCmp;"
  , "}"
  , "/* get_rpmverOverlap_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_fb1abfb454580d25 (void)) ("
  , "  rpmver arg1,"
  , "  rpmsenseFlags arg2,"
  , "  rpmver arg3,"
  , "  rpmsenseFlags arg4"
  , ")"
  , "{"
  , "  return &rpmverOverlap;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_d8fb97b8c9b6f01b" hs_bindgen_d8fb97b8c9b6f01b ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE rpmvercmp_ptr #-}

{-|

  > rpmver

  Segmented string compare for version or release strings.

  [__@a@ /(input)/__]: 1st string

  [__@b@ /(input)/__]: 2nd string

  __returns:__ +1 if a is "newer", 0 if equal, -1 if b is "newer"

__C declaration:__ @rpmvercmp@

__defined at:__ @rpm\/rpmver.h:18:5@

__exported by:__ @rpm\/rpmver.h@
-}
rpmvercmp_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt)
rpmvercmp_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d8fb97b8c9b6f01b

foreign import ccall unsafe "hs_bindgen_a0e5c85c66ad161e" hs_bindgen_a0e5c85c66ad161e ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO RPM.Types.Rpmver))

{-# NOINLINE rpmverParse_ptr #-}

{-|

  > rpmver

  Parse rpm version handle from evr string

  [__@evr@ /(input)/__]: [epoch:]version[-release] string

  __returns:__ rpm version, NULL on invalid evr

__C declaration:__ @rpmverParse@

__defined at:__ @rpm\/rpmver.h:26:8@

__exported by:__ @rpm\/rpmver.h@
-}
rpmverParse_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO RPM.Types.Rpmver)
rpmverParse_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a0e5c85c66ad161e

foreign import ccall unsafe "hs_bindgen_32d0b48dac163fd8" hs_bindgen_32d0b48dac163fd8 ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO RPM.Types.Rpmver))

{-# NOINLINE rpmverNew_ptr #-}

{-|

  > rpmver

  Create new rpm version handle from e, v, r components

  [__@e@ /(input)/__]: epoch (or NULL)

  [__@v@ /(input)/__]: version

  [__@r@ /(input)/__]: release (or NULL)

  __returns:__ rpm version, NULL on invalid

__C declaration:__ @rpmverNew@

__defined at:__ @rpm\/rpmver.h:36:8@

__exported by:__ @rpm\/rpmver.h@
-}
rpmverNew_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO RPM.Types.Rpmver)
rpmverNew_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_32d0b48dac163fd8

foreign import ccall unsafe "hs_bindgen_cc8bed62bfcbb206" hs_bindgen_cc8bed62bfcbb206 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmver -> IO RPM.Types.Rpmver))

{-# NOINLINE rpmverFree_ptr #-}

{-|

  > rpmver

  Free rpm version handle

  [__@rv@ /(input)/__]: rpm version handle

  __returns:__ NULL always

__C declaration:__ @rpmverFree@

__defined at:__ @rpm\/rpmver.h:44:8@

__exported by:__ @rpm\/rpmver.h@
-}
rpmverFree_ptr :: Ptr.FunPtr (RPM.Types.Rpmver -> IO RPM.Types.Rpmver)
rpmverFree_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cc8bed62bfcbb206

foreign import ccall unsafe "hs_bindgen_94bf432525736bb9" hs_bindgen_94bf432525736bb9 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmver -> IO HsBindgen.Runtime.Prelude.Word32))

{-# NOINLINE rpmverEVal_ptr #-}

{-|

  > rpmver

  [__@rv@ /(input)/__]: rpm version handle

  __returns:__ numerical value of epoch

__C declaration:__ @rpmverEVal@

__defined at:__ @rpm\/rpmver.h:50:10@

__exported by:__ @rpm\/rpmver.h@
-}
rpmverEVal_ptr :: Ptr.FunPtr (RPM.Types.Rpmver -> IO HsBindgen.Runtime.Prelude.Word32)
rpmverEVal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_94bf432525736bb9

foreign import ccall unsafe "hs_bindgen_b8fe24a031131c3e" hs_bindgen_b8fe24a031131c3e ::
     IO (Ptr.FunPtr (RPM.Types.Rpmver -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmverE_ptr #-}

{-|

  > rpmver

  [__@rv@ /(input)/__]: rpm version handle

  __returns:__ epoch portion

__C declaration:__ @rpmverE@

__defined at:__ @rpm\/rpmver.h:56:13@

__exported by:__ @rpm\/rpmver.h@
-}
rpmverE_ptr :: Ptr.FunPtr (RPM.Types.Rpmver -> IO (Ptr.Ptr FC.CChar))
rpmverE_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b8fe24a031131c3e

foreign import ccall unsafe "hs_bindgen_beb87f92668dc4c5" hs_bindgen_beb87f92668dc4c5 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmver -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmverV_ptr #-}

{-|

  > rpmver

  [__@rv@ /(input)/__]: rpm version handle

  __returns:__ version portion

__C declaration:__ @rpmverV@

__defined at:__ @rpm\/rpmver.h:62:13@

__exported by:__ @rpm\/rpmver.h@
-}
rpmverV_ptr :: Ptr.FunPtr (RPM.Types.Rpmver -> IO (Ptr.Ptr FC.CChar))
rpmverV_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_beb87f92668dc4c5

foreign import ccall unsafe "hs_bindgen_b1209811edb59574" hs_bindgen_b1209811edb59574 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmver -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmverR_ptr #-}

{-|

  > rpmver

  [__@rv@ /(input)/__]: rpm version handle

  __returns:__ release portion

__C declaration:__ @rpmverR@

__defined at:__ @rpm\/rpmver.h:68:13@

__exported by:__ @rpm\/rpmver.h@
-}
rpmverR_ptr :: Ptr.FunPtr (RPM.Types.Rpmver -> IO (Ptr.Ptr FC.CChar))
rpmverR_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b1209811edb59574

foreign import ccall unsafe "hs_bindgen_4c72ea6ec02c3e8d" hs_bindgen_4c72ea6ec02c3e8d ::
     IO (Ptr.FunPtr (RPM.Types.Rpmver -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmverEVR_ptr #-}

{-|

  > rpmver

  [__@rv@ /(input)/__]: rpm version handle

  __returns:__ formatted [E:]V[-R] string (malloced)

__C declaration:__ @rpmverEVR@

__defined at:__ @rpm\/rpmver.h:74:7@

__exported by:__ @rpm\/rpmver.h@
-}
rpmverEVR_ptr :: Ptr.FunPtr (RPM.Types.Rpmver -> IO (Ptr.Ptr FC.CChar))
rpmverEVR_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4c72ea6ec02c3e8d

foreign import ccall unsafe "hs_bindgen_9469a0b62ccaea30" hs_bindgen_9469a0b62ccaea30 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmver -> RPM.Types.Rpmver -> IO FC.CInt))

{-# NOINLINE rpmverCmp_ptr #-}

{-|

  > rpmver

  Compare two rpm version handles

  [__@v1@ /(input)/__]: 1st version handle

  [__@v2@ /(input)/__]: 2nd version handle

  __returns:__ 0 if equal, -1 if v1 smaller, 1 if greater, than v2

__C declaration:__ @rpmverCmp@

__defined at:__ @rpm\/rpmver.h:83:5@

__exported by:__ @rpm\/rpmver.h@
-}
rpmverCmp_ptr :: Ptr.FunPtr (RPM.Types.Rpmver -> RPM.Types.Rpmver -> IO FC.CInt)
rpmverCmp_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9469a0b62ccaea30

foreign import ccall unsafe "hs_bindgen_fb1abfb454580d25" hs_bindgen_fb1abfb454580d25 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmver -> RPM.Ds.RpmsenseFlags -> RPM.Types.Rpmver -> RPM.Ds.RpmsenseFlags -> IO FC.CInt))

{-# NOINLINE rpmverOverlap_ptr #-}

{-|

  > rpmver

  Determine whether two versioned ranges overlap.

  [__@v1@ /(input)/__]: 1st version

  [__@f1@ /(input)/__]: 1st sense flags

  [__@v2@ /(input)/__]: 2nd version

  [__@f2@ /(input)/__]: 2nd sense flags

  __returns:__ 1 if ranges overlap, 0 otherwise

__C declaration:__ @rpmverOverlap@

__defined at:__ @rpm\/rpmver.h:93:5@

__exported by:__ @rpm\/rpmver.h@
-}
rpmverOverlap_ptr :: Ptr.FunPtr (RPM.Types.Rpmver -> RPM.Ds.RpmsenseFlags -> RPM.Types.Rpmver -> RPM.Ds.RpmsenseFlags -> IO FC.CInt)
rpmverOverlap_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fb1abfb454580d25
