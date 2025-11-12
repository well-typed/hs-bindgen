{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Ds.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Types
import Data.Void (Void)
import Prelude (IO)
import RPM.Ds

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmds.h>"
  , "/* get_rpmSanitizeDSFlags_ptr */"
  , "__attribute__ ((const))"
  , "rpmsenseFlags (*hs_bindgen_5cd1f3846ae5fab6 (void)) ("
  , "  rpmTagVal arg1,"
  , "  rpmsenseFlags arg2"
  , ")"
  , "{"
  , "  return &rpmSanitizeDSFlags;"
  , "}"
  , "/* get_rpmParseDSFlags_ptr */"
  , "__attribute__ ((const))"
  , "rpmsenseFlags (*hs_bindgen_bd4e0ad082d4504f (void)) ("
  , "  char const *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &rpmParseDSFlags;"
  , "}"
  , "/* get_rpmdsLink_ptr */"
  , "__attribute__ ((const))"
  , "rpmds (*hs_bindgen_74242d4a20a09854 (void)) ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return &rpmdsLink;"
  , "}"
  , "/* get_rpmdsFree_ptr */"
  , "__attribute__ ((const))"
  , "rpmds (*hs_bindgen_020b24cf456d307e (void)) ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return &rpmdsFree;"
  , "}"
  , "/* get_rpmdsNew_ptr */"
  , "__attribute__ ((const))"
  , "rpmds (*hs_bindgen_fd739d48476ccd4c (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &rpmdsNew;"
  , "}"
  , "/* get_rpmdsNewDNEVR_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_b95777ef4358f873 (void)) ("
  , "  char const *arg1,"
  , "  rpmds const arg2"
  , ")"
  , "{"
  , "  return &rpmdsNewDNEVR;"
  , "}"
  , "/* get_rpmdsThis_ptr */"
  , "__attribute__ ((const))"
  , "rpmds (*hs_bindgen_1c04905c2ff0f55d (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  rpmsenseFlags arg3"
  , ")"
  , "{"
  , "  return &rpmdsThis;"
  , "}"
  , "/* get_rpmdsSingle_ptr */"
  , "__attribute__ ((const))"
  , "rpmds (*hs_bindgen_5db0a26f19f7c6af (void)) ("
  , "  rpmTagVal arg1,"
  , "  char const *arg2,"
  , "  char const *arg3,"
  , "  rpmsenseFlags arg4"
  , ")"
  , "{"
  , "  return &rpmdsSingle;"
  , "}"
  , "/* get_rpmdsCurrent_ptr */"
  , "__attribute__ ((const))"
  , "rpmds (*hs_bindgen_b628ccbbc9c81cd7 (void)) ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return &rpmdsCurrent;"
  , "}"
  , "/* get_rpmdsPutToHeader_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_f7a5b99db41e279f (void)) ("
  , "  rpmds arg1,"
  , "  Header arg2"
  , ")"
  , "{"
  , "  return &rpmdsPutToHeader;"
  , "}"
  , "/* get_rpmdsCount_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_28aa761f380b8e89 (void)) ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return &rpmdsCount;"
  , "}"
  , "/* get_rpmdsIx_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_d1ecbc59c24ed321 (void)) ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return &rpmdsIx;"
  , "}"
  , "/* get_rpmdsSetIx_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8273fee94e757edf (void)) ("
  , "  rpmds arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &rpmdsSetIx;"
  , "}"
  , "/* get_rpmdsDNEVR_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_085af8c3e78e9892 (void)) ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return &rpmdsDNEVR;"
  , "}"
  , "/* get_rpmdsD_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_1e07f2e6ba506a3b (void)) ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return &rpmdsD;"
  , "}"
  , "/* get_rpmdsDToTagN_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagVal (*hs_bindgen_56754cec5cd722d9 (void)) ("
  , "  char arg1"
  , ")"
  , "{"
  , "  return &rpmdsDToTagN;"
  , "}"
  , "/* get_rpmdsN_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_c083ab0db19d645b (void)) ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return &rpmdsN;"
  , "}"
  , "/* get_rpmdsEVR_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_62676911e8ae6603 (void)) ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return &rpmdsEVR;"
  , "}"
  , "/* get_rpmdsTi_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a1eeace36a5ccdbd (void)) ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return &rpmdsTi;"
  , "}"
  , "/* get_rpmdsFlags_ptr */"
  , "__attribute__ ((const))"
  , "rpmsenseFlags (*hs_bindgen_e07f8eda1c5ec02e (void)) ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return &rpmdsFlags;"
  , "}"
  , "/* get_rpmdsTagN_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagVal (*hs_bindgen_dce146c99db99c33 (void)) ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return &rpmdsTagN;"
  , "}"
  , "/* get_rpmdsTagEVR_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagVal (*hs_bindgen_dde1532901ca3251 (void)) ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return &rpmdsTagEVR;"
  , "}"
  , "/* get_rpmdsTagF_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagVal (*hs_bindgen_1a8139a4a86b6e17 (void)) ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return &rpmdsTagF;"
  , "}"
  , "/* get_rpmdsTagTi_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagVal (*hs_bindgen_f583986dccec7423 (void)) ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return &rpmdsTagTi;"
  , "}"
  , "/* get_rpmdsInstance_ptr */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_24348152e6d02350 (void)) ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return &rpmdsInstance;"
  , "}"
  , "/* get_rpmdsIsWeak_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_6c086fba7937ff61 (void)) ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return &rpmdsIsWeak;"
  , "}"
  , "/* get_rpmdsIsReverse_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_ef220b59216f6095 (void)) ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return &rpmdsIsReverse;"
  , "}"
  , "/* get_rpmdsIsSysuser_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_5cf2919a9ed8bc62 (void)) ("
  , "  rpmds arg1,"
  , "  char **arg2"
  , ")"
  , "{"
  , "  return &rpmdsIsSysuser;"
  , "}"
  , "/* get_rpmdsColor_ptr */"
  , "__attribute__ ((const))"
  , "rpm_color_t (*hs_bindgen_b9e22ae3ef3f65e5 (void)) ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return &rpmdsColor;"
  , "}"
  , "/* get_rpmdsSetColor_ptr */"
  , "__attribute__ ((const))"
  , "rpm_color_t (*hs_bindgen_141102a401816ce0 (void)) ("
  , "  rpmds const arg1,"
  , "  rpm_color_t arg2"
  , ")"
  , "{"
  , "  return &rpmdsSetColor;"
  , "}"
  , "/* get_rpmdsNext_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_5289ec711114dc86 (void)) ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return &rpmdsNext;"
  , "}"
  , "/* get_rpmdsInit_ptr */"
  , "__attribute__ ((const))"
  , "rpmds (*hs_bindgen_343808baf0ba95e6 (void)) ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return &rpmdsInit;"
  , "}"
  , "/* get_rpmdsFind_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_f3b575ed78691d33 (void)) ("
  , "  rpmds arg1,"
  , "  rpmds const arg2"
  , ")"
  , "{"
  , "  return &rpmdsFind;"
  , "}"
  , "/* get_rpmdsMerge_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_5a28e60914fa63f1 (void)) ("
  , "  rpmds *arg1,"
  , "  rpmds arg2"
  , ")"
  , "{"
  , "  return &rpmdsMerge;"
  , "}"
  , "/* get_rpmdsSearch_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_206b8c00b64903ae (void)) ("
  , "  rpmds arg1,"
  , "  rpmds arg2"
  , ")"
  , "{"
  , "  return &rpmdsSearch;"
  , "}"
  , "/* get_rpmdsCompare_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_89b8004a2f7de52a (void)) ("
  , "  rpmds const arg1,"
  , "  rpmds const arg2"
  , ")"
  , "{"
  , "  return &rpmdsCompare;"
  , "}"
  , "/* get_rpmdsAnyMatchesDep_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_2bc3385b34c39d64 (void)) ("
  , "  Header const arg1,"
  , "  rpmds const arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &rpmdsAnyMatchesDep;"
  , "}"
  , "/* get_rpmdsMatchesDep_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_2d0ba9a4370cf663 (void)) ("
  , "  Header const arg1,"
  , "  signed int arg2,"
  , "  rpmds const arg3,"
  , "  signed int arg4"
  , ")"
  , "{"
  , "  return &rpmdsMatchesDep;"
  , "}"
  , "/* get_rpmdsNVRMatchesDep_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4148cdbb449b1b93 (void)) ("
  , "  Header const arg1,"
  , "  rpmds const arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &rpmdsNVRMatchesDep;"
  , "}"
  , "/* get_rpmdsRpmlib_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_6e7e7651468ed6a7 (void)) ("
  , "  rpmds *arg1,"
  , "  void const *arg2"
  , ")"
  , "{"
  , "  return &rpmdsRpmlib;"
  , "}"
  , "/* get_rpmdsNewPool_ptr */"
  , "__attribute__ ((const))"
  , "rpmds (*hs_bindgen_45449719b7478d0d (void)) ("
  , "  rpmstrPool arg1,"
  , "  Header arg2,"
  , "  rpmTagVal arg3,"
  , "  signed int arg4"
  , ")"
  , "{"
  , "  return &rpmdsNewPool;"
  , "}"
  , "/* get_rpmdsThisPool_ptr */"
  , "__attribute__ ((const))"
  , "rpmds (*hs_bindgen_6eae96497c98d13a (void)) ("
  , "  rpmstrPool arg1,"
  , "  Header arg2,"
  , "  rpmTagVal arg3,"
  , "  rpmsenseFlags arg4"
  , ")"
  , "{"
  , "  return &rpmdsThisPool;"
  , "}"
  , "/* get_rpmdsSinglePool_ptr */"
  , "__attribute__ ((const))"
  , "rpmds (*hs_bindgen_a25b8a8f4d722e4c (void)) ("
  , "  rpmstrPool arg1,"
  , "  rpmTagVal arg2,"
  , "  char const *arg3,"
  , "  char const *arg4,"
  , "  rpmsenseFlags arg5"
  , ")"
  , "{"
  , "  return &rpmdsSinglePool;"
  , "}"
  , "/* get_rpmdsSinglePoolTix_ptr */"
  , "__attribute__ ((const))"
  , "rpmds (*hs_bindgen_b7d864a468a9efbd (void)) ("
  , "  rpmstrPool arg1,"
  , "  rpmTagVal arg2,"
  , "  char const *arg3,"
  , "  char const *arg4,"
  , "  rpmsenseFlags arg5,"
  , "  signed int arg6"
  , ")"
  , "{"
  , "  return &rpmdsSinglePoolTix;"
  , "}"
  , "/* get_rpmdsRpmlibPool_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_6f3d61022b1b6aaa (void)) ("
  , "  rpmstrPool arg1,"
  , "  rpmds *arg2,"
  , "  void const *arg3"
  , ")"
  , "{"
  , "  return &rpmdsRpmlibPool;"
  , "}"
  , "/* get_rpmrichParse_ptr */"
  , "__attribute__ ((const))"
  , "rpmRC (*hs_bindgen_afdaf46453718193 (void)) ("
  , "  char const **arg1,"
  , "  char **arg2,"
  , "  rpmrichParseFunction arg3,"
  , "  void *arg4"
  , ")"
  , "{"
  , "  return &rpmrichParse;"
  , "}"
  , "/* get_rpmrichParseForTag_ptr */"
  , "__attribute__ ((const))"
  , "rpmRC (*hs_bindgen_4baeab0b65e36988 (void)) ("
  , "  char const **arg1,"
  , "  char **arg2,"
  , "  rpmrichParseFunction arg3,"
  , "  void *arg4,"
  , "  rpmTagVal arg5"
  , ")"
  , "{"
  , "  return &rpmrichParseForTag;"
  , "}"
  , "/* get_rpmdsIsRich_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_813829e7c480d380 (void)) ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return &rpmdsIsRich;"
  , "}"
  , "/* get_rpmrichOpStr_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_0f77767012fdb62e (void)) ("
  , "  rpmrichOp arg1"
  , ")"
  , "{"
  , "  return &rpmrichOpStr;"
  , "}"
  , "/* get_rpmdsParseRichDep_ptr */"
  , "__attribute__ ((const))"
  , "rpmRC (*hs_bindgen_c1d7493614d7d82f (void)) ("
  , "  rpmds arg1,"
  , "  rpmds *arg2,"
  , "  rpmds *arg3,"
  , "  rpmrichOp *arg4,"
  , "  char **arg5"
  , ")"
  , "{"
  , "  return &rpmdsParseRichDep;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_5cd1f3846ae5fab6" hs_bindgen_5cd1f3846ae5fab6 ::
     IO (Ptr.FunPtr (RPM.Types.RpmTagVal -> RpmsenseFlags -> IO RpmsenseFlags))

{-# NOINLINE rpmSanitizeDSFlags_ptr #-}

{-|

  > rpmds

  Return only those flags allowed for given type of dependencies

  [__@tagN@ /(input)/__]: type of dependency

  [__@Flags@ /(input)/__]: flags

  __returns:__ flags filtered to allowed bits

__C declaration:__ @rpmSanitizeDSFlags@

__defined at:__ @rpm\/rpmds.h:106:15@

__exported by:__ @rpm\/rpmds.h@
-}
rpmSanitizeDSFlags_ptr :: Ptr.FunPtr (RPM.Types.RpmTagVal -> RpmsenseFlags -> IO RpmsenseFlags)
rpmSanitizeDSFlags_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5cd1f3846ae5fab6

foreign import ccall unsafe "hs_bindgen_bd4e0ad082d4504f" hs_bindgen_bd4e0ad082d4504f ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO RpmsenseFlags))

{-# NOINLINE rpmParseDSFlags_ptr #-}

{-|

  > rpmds

  Convert a string to the sense flags

  [__@str@ /(input)/__]: the string

  [__@len@ /(input)/__]: length of the string

  __returns:__ flags, zero for unknown relations

__C declaration:__ @rpmParseDSFlags@

__defined at:__ @rpm\/rpmds.h:114:15@

__exported by:__ @rpm\/rpmds.h@
-}
rpmParseDSFlags_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO RpmsenseFlags)
rpmParseDSFlags_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bd4e0ad082d4504f

foreign import ccall unsafe "hs_bindgen_74242d4a20a09854" hs_bindgen_74242d4a20a09854 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.Rpmds))

{-# NOINLINE rpmdsLink_ptr #-}

{-|

  > rpmds

  Reference a dependency set instance.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ new dependency set reference

__C declaration:__ @rpmdsLink@

__defined at:__ @rpm\/rpmds.h:121:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsLink_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.Rpmds)
rpmdsLink_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_74242d4a20a09854

foreign import ccall unsafe "hs_bindgen_020b24cf456d307e" hs_bindgen_020b24cf456d307e ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.Rpmds))

{-# NOINLINE rpmdsFree_ptr #-}

{-|

  > rpmds

  Destroy a dependency set.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ NULL always

__C declaration:__ @rpmdsFree@

__defined at:__ @rpm\/rpmds.h:128:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsFree_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.Rpmds)
rpmdsFree_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_020b24cf456d307e

foreign import ccall unsafe "hs_bindgen_fd739d48476ccd4c" hs_bindgen_fd739d48476ccd4c ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Types.RpmTagVal -> FC.CInt -> IO RPM.Types.Rpmds))

{-# NOINLINE rpmdsNew_ptr #-}

{-|

  > rpmds

  Create and load a dependency set.

  [__@h@ /(input)/__]: header

  [__@tagN@ /(input)/__]: type of dependency

  [__@flags@ /(input)/__]: unused

  __returns:__ new dependency set

__C declaration:__ @rpmdsNew@

__defined at:__ @rpm\/rpmds.h:137:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsNew_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Types.RpmTagVal -> FC.CInt -> IO RPM.Types.Rpmds)
rpmdsNew_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fd739d48476ccd4c

foreign import ccall unsafe "hs_bindgen_b95777ef4358f873" hs_bindgen_b95777ef4358f873 ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> RPM.Types.Rpmds -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmdsNewDNEVR_ptr #-}

{-|

  > rpmds

  Return new formatted dependency string.

  [__@dspfx@ /(input)/__]: formatted dependency string prefix

  [__@ds@ /(input)/__]: dependency set

  __returns:__ new formatted dependency (malloc'ed)

__C declaration:__ @rpmdsNewDNEVR@

__defined at:__ @rpm\/rpmds.h:145:8@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsNewDNEVR_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> RPM.Types.Rpmds -> IO (Ptr.Ptr FC.CChar))
rpmdsNewDNEVR_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b95777ef4358f873

foreign import ccall unsafe "hs_bindgen_1c04905c2ff0f55d" hs_bindgen_1c04905c2ff0f55d ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Types.RpmTagVal -> RpmsenseFlags -> IO RPM.Types.Rpmds))

{-# NOINLINE rpmdsThis_ptr #-}

{-|

  > rpmds

  Create, load and initialize a dependency for this header.

  [__@h@ /(input)/__]: header

  [__@tagN@ /(input)/__]: type of dependency

  [__@Flags@ /(input)/__]: comparison flags

  __returns:__ new dependency set

__C declaration:__ @rpmdsThis@

__defined at:__ @rpm\/rpmds.h:154:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsThis_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Types.RpmTagVal -> RpmsenseFlags -> IO RPM.Types.Rpmds)
rpmdsThis_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1c04905c2ff0f55d

foreign import ccall unsafe "hs_bindgen_5db0a26f19f7c6af" hs_bindgen_5db0a26f19f7c6af ::
     IO (Ptr.FunPtr (RPM.Types.RpmTagVal -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> RpmsenseFlags -> IO RPM.Types.Rpmds))

{-# NOINLINE rpmdsSingle_ptr #-}

{-|

  > rpmds

  Create, load and initialize a dependency set of size 1.

  [__@tagN@ /(input)/__]: type of dependency

  [__@N@ /(input)/__]: name

  [__@EVR@ /(input)/__]: epoch:version-release

  [__@Flags@ /(input)/__]: comparison flags

  __returns:__ new dependency set

__C declaration:__ @rpmdsSingle@

__defined at:__ @rpm\/rpmds.h:164:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsSingle_ptr :: Ptr.FunPtr (RPM.Types.RpmTagVal -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> RpmsenseFlags -> IO RPM.Types.Rpmds)
rpmdsSingle_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5db0a26f19f7c6af

foreign import ccall unsafe "hs_bindgen_b628ccbbc9c81cd7" hs_bindgen_b628ccbbc9c81cd7 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.Rpmds))

{-# NOINLINE rpmdsCurrent_ptr #-}

{-|

  > rpmds

  Return a new dependency set of size 1 from the current iteration index

  [__@ds@ /(input)/__]: dependency set

  __returns:__ new dependency set

__C declaration:__ @rpmdsCurrent@

__defined at:__ @rpm\/rpmds.h:171:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsCurrent_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.Rpmds)
rpmdsCurrent_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b628ccbbc9c81cd7

foreign import ccall unsafe "hs_bindgen_f7a5b99db41e279f" hs_bindgen_f7a5b99db41e279f ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> RPM.Types.Header -> IO FC.CInt))

{-# NOINLINE rpmdsPutToHeader_ptr #-}

{-|

  > rpmds

  Write content of the dependency set to the header

  [__@ds@ /(input)/__]: dependency set

  [__@h@ /(input)/__]: header

  __returns:__ 0 on success

__C declaration:__ @rpmdsPutToHeader@

__defined at:__ @rpm\/rpmds.h:179:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsPutToHeader_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> RPM.Types.Header -> IO FC.CInt)
rpmdsPutToHeader_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f7a5b99db41e279f

foreign import ccall unsafe "hs_bindgen_28aa761f380b8e89" hs_bindgen_28aa761f380b8e89 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt))

{-# NOINLINE rpmdsCount_ptr #-}

{-|

  > rpmds

  Return dependency set count.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current count

__C declaration:__ @rpmdsCount@

__defined at:__ @rpm\/rpmds.h:186:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsCount_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt)
rpmdsCount_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_28aa761f380b8e89

foreign import ccall unsafe "hs_bindgen_d1ecbc59c24ed321" hs_bindgen_d1ecbc59c24ed321 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt))

{-# NOINLINE rpmdsIx_ptr #-}

{-|

  > rpmds

  Return dependency set index.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current index

__C declaration:__ @rpmdsIx@

__defined at:__ @rpm\/rpmds.h:193:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsIx_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt)
rpmdsIx_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d1ecbc59c24ed321

foreign import ccall unsafe "hs_bindgen_8273fee94e757edf" hs_bindgen_8273fee94e757edf ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> FC.CInt -> IO FC.CInt))

{-# NOINLINE rpmdsSetIx_ptr #-}

{-|

  > rpmds

  Set dependency set index.

  [__@ds@ /(input)/__]: dependency set

  [__@ix@ /(input)/__]: new index

  __returns:__ new index, -1 on error

__C declaration:__ @rpmdsSetIx@

__defined at:__ @rpm\/rpmds.h:201:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsSetIx_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> FC.CInt -> IO FC.CInt)
rpmdsSetIx_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8273fee94e757edf

foreign import ccall unsafe "hs_bindgen_085af8c3e78e9892" hs_bindgen_085af8c3e78e9892 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmdsDNEVR_ptr #-}

{-|

  > rpmds

  Return current formatted dependency string.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency DNEVR, NULL on invalid

__C declaration:__ @rpmdsDNEVR@

__defined at:__ @rpm\/rpmds.h:208:14@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsDNEVR_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO (Ptr.Ptr FC.CChar))
rpmdsDNEVR_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_085af8c3e78e9892

foreign import ccall unsafe "hs_bindgen_1e07f2e6ba506a3b" hs_bindgen_1e07f2e6ba506a3b ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CChar))

{-# NOINLINE rpmdsD_ptr #-}

{-|

  > rpmds

  Return one char indicating the type of the dependency.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ character

__C declaration:__ @rpmdsD@

__defined at:__ @rpm\/rpmds.h:215:6@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsD_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CChar)
rpmdsD_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1e07f2e6ba506a3b

foreign import ccall unsafe "hs_bindgen_56754cec5cd722d9" hs_bindgen_56754cec5cd722d9 ::
     IO (Ptr.FunPtr (FC.CChar -> IO RPM.Types.RpmTagVal))

{-# NOINLINE rpmdsDToTagN_ptr #-}

{-|

  > rpmds

  Return matching tagN for one char dependency type description.

  [__@deptype@ /(input)/__]: character

  __returns:__ type of dependency

__C declaration:__ @rpmdsDToTagN@

__defined at:__ @rpm\/rpmds.h:222:11@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsDToTagN_ptr :: Ptr.FunPtr (FC.CChar -> IO RPM.Types.RpmTagVal)
rpmdsDToTagN_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_56754cec5cd722d9

foreign import ccall unsafe "hs_bindgen_c083ab0db19d645b" hs_bindgen_c083ab0db19d645b ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmdsN_ptr #-}

{-|

  > rpmds

  Return current dependency name.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency name, NULL on invalid

__C declaration:__ @rpmdsN@

__defined at:__ @rpm\/rpmds.h:229:14@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsN_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO (Ptr.Ptr FC.CChar))
rpmdsN_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c083ab0db19d645b

foreign import ccall unsafe "hs_bindgen_62676911e8ae6603" hs_bindgen_62676911e8ae6603 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmdsEVR_ptr #-}

{-|

  > rpmds

  Return current dependency epoch-version-release.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency EVR, NULL on invalid

__C declaration:__ @rpmdsEVR@

__defined at:__ @rpm\/rpmds.h:236:14@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsEVR_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO (Ptr.Ptr FC.CChar))
rpmdsEVR_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_62676911e8ae6603

foreign import ccall unsafe "hs_bindgen_a1eeace36a5ccdbd" hs_bindgen_a1eeace36a5ccdbd ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt))

{-# NOINLINE rpmdsTi_ptr #-}

{-|

  > rpmds

  Return current dependency triggerindex.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency trigger index, -1 on invalid

__C declaration:__ @rpmdsTi@

__defined at:__ @rpm\/rpmds.h:243:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsTi_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt)
rpmdsTi_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a1eeace36a5ccdbd

foreign import ccall unsafe "hs_bindgen_e07f8eda1c5ec02e" hs_bindgen_e07f8eda1c5ec02e ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO RpmsenseFlags))

{-# NOINLINE rpmdsFlags_ptr #-}

{-|

  > rpmds

  Return current dependency flags.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency flags, 0 on invalid

__C declaration:__ @rpmdsFlags@

__defined at:__ @rpm\/rpmds.h:250:15@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsFlags_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO RpmsenseFlags)
rpmdsFlags_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e07f8eda1c5ec02e

foreign import ccall unsafe "hs_bindgen_dce146c99db99c33" hs_bindgen_dce146c99db99c33 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.RpmTagVal))

{-# NOINLINE rpmdsTagN_ptr #-}

{-|

  > rpmds

  Return current dependency type.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency type, 0 on invalid

__C declaration:__ @rpmdsTagN@

__defined at:__ @rpm\/rpmds.h:257:11@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsTagN_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.RpmTagVal)
rpmdsTagN_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dce146c99db99c33

foreign import ccall unsafe "hs_bindgen_dde1532901ca3251" hs_bindgen_dde1532901ca3251 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.RpmTagVal))

{-# NOINLINE rpmdsTagEVR_ptr #-}

{-|

  > rpmds

  Return current dependency type.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency type version tag, 0 on invalid

__C declaration:__ @rpmdsTagEVR@

__defined at:__ @rpm\/rpmds.h:264:11@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsTagEVR_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.RpmTagVal)
rpmdsTagEVR_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dde1532901ca3251

foreign import ccall unsafe "hs_bindgen_1a8139a4a86b6e17" hs_bindgen_1a8139a4a86b6e17 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.RpmTagVal))

{-# NOINLINE rpmdsTagF_ptr #-}

{-|

  > rpmds

  Return current dependency type.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency type flags tag, 0 on invalid

__C declaration:__ @rpmdsTagF@

__defined at:__ @rpm\/rpmds.h:271:11@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsTagF_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.RpmTagVal)
rpmdsTagF_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1a8139a4a86b6e17

foreign import ccall unsafe "hs_bindgen_f583986dccec7423" hs_bindgen_f583986dccec7423 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.RpmTagVal))

{-# NOINLINE rpmdsTagTi_ptr #-}

{-|

  > rpmds

  Return current dependency type.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency type trigger index tag, 0 on invalid

__C declaration:__ @rpmdsTagTi@

__defined at:__ @rpm\/rpmds.h:278:11@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsTagTi_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.RpmTagVal)
rpmdsTagTi_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f583986dccec7423

foreign import ccall unsafe "hs_bindgen_24348152e6d02350" hs_bindgen_24348152e6d02350 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CUInt))

{-# NOINLINE rpmdsInstance_ptr #-}

{-|

  > rpmds

  Return dependency header instance, ie whether the dependency comes from an installed header or not.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ header instance of dependency (0 for not installed)

__C declaration:__ @rpmdsInstance@

__defined at:__ @rpm\/rpmds.h:286:14@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsInstance_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CUInt)
rpmdsInstance_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_24348152e6d02350

foreign import ccall unsafe "hs_bindgen_6c086fba7937ff61" hs_bindgen_6c086fba7937ff61 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt))

{-# NOINLINE rpmdsIsWeak_ptr #-}

{-|

  > rpmds

  Return whether dependency is weak

  [__@ds@ /(input)/__]: dependency set

  __returns:__ 1 if weak, 0 if not

__C declaration:__ @rpmdsIsWeak@

__defined at:__ @rpm\/rpmds.h:293:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsIsWeak_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt)
rpmdsIsWeak_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6c086fba7937ff61

foreign import ccall unsafe "hs_bindgen_ef220b59216f6095" hs_bindgen_ef220b59216f6095 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt))

{-# NOINLINE rpmdsIsReverse_ptr #-}

{-|

  > rpmds

  Return whether dependency is reversed

  [__@ds@ /(input)/__]: dependency set

  __returns:__ 1 if reversed, 0 if not

__C declaration:__ @rpmdsIsReverse@

__defined at:__ @rpm\/rpmds.h:300:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsIsReverse_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt)
rpmdsIsReverse_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ef220b59216f6095

foreign import ccall unsafe "hs_bindgen_5cf2919a9ed8bc62" hs_bindgen_5cf2919a9ed8bc62 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO FC.CInt))

{-# NOINLINE rpmdsIsSysuser_ptr #-}

{-|

  > rpmds

  Return whether dependency represents a sysusers.d entry

  [__@ds@ /(input)/__]: dependency set

  [__@sysuser@ /(output)/__]: sysusers.d line if true (malloced), may be NULL

  __returns:__ 1 if reversed, 0 if not

__C declaration:__ @rpmdsIsSysuser@

__defined at:__ @rpm\/rpmds.h:308:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsIsSysuser_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO FC.CInt)
rpmdsIsSysuser_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5cf2919a9ed8bc62

foreign import ccall unsafe "hs_bindgen_b9e22ae3ef3f65e5" hs_bindgen_b9e22ae3ef3f65e5 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.Rpm_color_t))

{-# NOINLINE rpmdsColor_ptr #-}

{-|

  > rpmds

  Return current dependency color.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency color

__C declaration:__ @rpmdsColor@

__defined at:__ @rpm\/rpmds.h:315:13@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsColor_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.Rpm_color_t)
rpmdsColor_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b9e22ae3ef3f65e5

foreign import ccall unsafe "hs_bindgen_141102a401816ce0" hs_bindgen_141102a401816ce0 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> RPM.Types.Rpm_color_t -> IO RPM.Types.Rpm_color_t))

{-# NOINLINE rpmdsSetColor_ptr #-}

{-|

  > rpmds

  Return current dependency color.

  [__@ds@ /(input)/__]: dependency set

  [__@color@ /(input)/__]: new dependency color

  __returns:__ previous dependency color

__C declaration:__ @rpmdsSetColor@

__defined at:__ @rpm\/rpmds.h:323:13@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsSetColor_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> RPM.Types.Rpm_color_t -> IO RPM.Types.Rpm_color_t)
rpmdsSetColor_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_141102a401816ce0

foreign import ccall unsafe "hs_bindgen_5289ec711114dc86" hs_bindgen_5289ec711114dc86 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt))

{-# NOINLINE rpmdsNext_ptr #-}

{-|

  > rpmds

  Return next dependency set iterator index.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ dependency set iterator index, -1 on termination

__C declaration:__ @rpmdsNext@

__defined at:__ @rpm\/rpmds.h:330:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsNext_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt)
rpmdsNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5289ec711114dc86

foreign import ccall unsafe "hs_bindgen_343808baf0ba95e6" hs_bindgen_343808baf0ba95e6 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.Rpmds))

{-# NOINLINE rpmdsInit_ptr #-}

{-|

  > rpmds

  Initialize dependency set iterator.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ dependency set

__C declaration:__ @rpmdsInit@

__defined at:__ @rpm\/rpmds.h:337:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsInit_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO RPM.Types.Rpmds)
rpmdsInit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_343808baf0ba95e6

foreign import ccall unsafe "hs_bindgen_f3b575ed78691d33" hs_bindgen_f3b575ed78691d33 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> RPM.Types.Rpmds -> IO FC.CInt))

{-# NOINLINE rpmdsFind_ptr #-}

{-|

  > rpmds

  Find a dependency set element using binary search.

  [__@ds@ /(input)/__]: dependency set to search

  [__@ods@ /(input)/__]: dependency set element to find.

  __returns:__ dependency index (or -1 if not found)

__C declaration:__ @rpmdsFind@

__defined at:__ @rpm\/rpmds.h:345:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsFind_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> RPM.Types.Rpmds -> IO FC.CInt)
rpmdsFind_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f3b575ed78691d33

foreign import ccall unsafe "hs_bindgen_5a28e60914fa63f1" hs_bindgen_5a28e60914fa63f1 ::
     IO (Ptr.FunPtr ((Ptr.Ptr RPM.Types.Rpmds) -> RPM.Types.Rpmds -> IO FC.CInt))

{-# NOINLINE rpmdsMerge_ptr #-}

{-|

  > rpmds

  Merge a dependency set maintaining (N,EVR,Flags) sorted order.

  [__@*dsp@ /(output)/__]: (merged) dependency set

  [__@ods@ /(input)/__]: dependency set to merge

  __returns:__ number of merged dependencies, -1 on error

__C declaration:__ @rpmdsMerge@

__defined at:__ @rpm\/rpmds.h:353:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsMerge_ptr :: Ptr.FunPtr ((Ptr.Ptr RPM.Types.Rpmds) -> RPM.Types.Rpmds -> IO FC.CInt)
rpmdsMerge_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5a28e60914fa63f1

foreign import ccall unsafe "hs_bindgen_206b8c00b64903ae" hs_bindgen_206b8c00b64903ae ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> RPM.Types.Rpmds -> IO FC.CInt))

{-# NOINLINE rpmdsSearch_ptr #-}

{-|

  > rpmds

  Search a sorted dependency set for an element that overlaps. A boolean result is saved (if allocated) and accessible through rpmdsResult(ods) afterwards.

  [__@ds@ /(input)/__]: dependency set to search

  [__@ods@ /(input)/__]: dependency set element to find.

  __returns:__ dependency index (or -1 if not found)

__C declaration:__ @rpmdsSearch@

__defined at:__ @rpm\/rpmds.h:363:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsSearch_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> RPM.Types.Rpmds -> IO FC.CInt)
rpmdsSearch_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_206b8c00b64903ae

foreign import ccall unsafe "hs_bindgen_89b8004a2f7de52a" hs_bindgen_89b8004a2f7de52a ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> RPM.Types.Rpmds -> IO FC.CInt))

{-# NOINLINE rpmdsCompare_ptr #-}

{-|

  > rpmds

  Compare two versioned dependency ranges, looking for overlap.

  [__@A@ /(input)/__]: 1st dependency

  [__@B@ /(input)/__]: 2nd dependency

  __returns:__ 1 if dependencies overlap, 0 otherwise

__C declaration:__ @rpmdsCompare@

__defined at:__ @rpm\/rpmds.h:371:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsCompare_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> RPM.Types.Rpmds -> IO FC.CInt)
rpmdsCompare_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_89b8004a2f7de52a

foreign import ccall unsafe "hs_bindgen_2bc3385b34c39d64" hs_bindgen_2bc3385b34c39d64 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Types.Rpmds -> FC.CInt -> IO FC.CInt))

{-# NOINLINE rpmdsAnyMatchesDep_ptr #-}

{-|

  > rpmds

  Compare package provides dependencies from header with a single dependency.

  [__@h@ /(input)/__]: header

  [__@req@ /(input)/__]: dependency set

  [__@nopromote@ /(input)/__]: unused

  __returns:__ 1 if any dependency overlaps, 0 otherwise

__C declaration:__ @rpmdsAnyMatchesDep@

__defined at:__ @rpm\/rpmds.h:380:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsAnyMatchesDep_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Types.Rpmds -> FC.CInt -> IO FC.CInt)
rpmdsAnyMatchesDep_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2bc3385b34c39d64

foreign import ccall unsafe "hs_bindgen_2d0ba9a4370cf663" hs_bindgen_2d0ba9a4370cf663 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> FC.CInt -> RPM.Types.Rpmds -> FC.CInt -> IO FC.CInt))

{-# NOINLINE rpmdsMatchesDep_ptr #-}

{-|

  > rpmds

  Compare package provides dependencies from header with a single dependency.

  [__@h@ /(input)/__]: header

  [__@ix@ /(input)/__]: index in header provides

  [__@req@ /(input)/__]: dependency set

  [__@nopromote@ /(input)/__]: unused

  __returns:__ 1 if any dependency overlaps, 0 otherwise

__C declaration:__ @rpmdsMatchesDep@

__defined at:__ @rpm\/rpmds.h:390:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsMatchesDep_ptr :: Ptr.FunPtr (RPM.Types.Header -> FC.CInt -> RPM.Types.Rpmds -> FC.CInt -> IO FC.CInt)
rpmdsMatchesDep_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2d0ba9a4370cf663

foreign import ccall unsafe "hs_bindgen_4148cdbb449b1b93" hs_bindgen_4148cdbb449b1b93 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Types.Rpmds -> FC.CInt -> IO FC.CInt))

{-# NOINLINE rpmdsNVRMatchesDep_ptr #-}

{-|

  > rpmds

  Compare package name-version-release from header with a single dependency.

  [__@h@ /(input)/__]: header

  [__@req@ /(input)/__]: dependency set

  [__@nopromote@ /(input)/__]: unused

  __returns:__ 1 if dependency overlaps, 0 otherwise

__C declaration:__ @rpmdsNVRMatchesDep@

__defined at:__ @rpm\/rpmds.h:399:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsNVRMatchesDep_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Types.Rpmds -> FC.CInt -> IO FC.CInt)
rpmdsNVRMatchesDep_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4148cdbb449b1b93

foreign import ccall unsafe "hs_bindgen_6e7e7651468ed6a7" hs_bindgen_6e7e7651468ed6a7 ::
     IO (Ptr.FunPtr ((Ptr.Ptr RPM.Types.Rpmds) -> (Ptr.Ptr Void) -> IO FC.CInt))

{-# NOINLINE rpmdsRpmlib_ptr #-}

{-| Load rpmlib provides into a dependency set.

  [__@*dsp@ /(output)/__]: (loaded) dependency set

  [__@tblp@ /(input)/__]: rpmlib provides table (NULL uses internal table)

  __returns:__ 0 on success

__C declaration:__ @rpmdsRpmlib@

__defined at:__ @rpm\/rpmds.h:407:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsRpmlib_ptr :: Ptr.FunPtr ((Ptr.Ptr RPM.Types.Rpmds) -> (Ptr.Ptr Void) -> IO FC.CInt)
rpmdsRpmlib_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6e7e7651468ed6a7

foreign import ccall unsafe "hs_bindgen_45449719b7478d0d" hs_bindgen_45449719b7478d0d ::
     IO (Ptr.FunPtr (RPM.Types.RpmstrPool -> RPM.Types.Header -> RPM.Types.RpmTagVal -> FC.CInt -> IO RPM.Types.Rpmds))

{-# NOINLINE rpmdsNewPool_ptr #-}

{-|

  > rpmds

  Create and load a dependency set.

  [__@pool@ /(input)/__]: shared string pool (or NULL for private pool)

  [__@h@ /(input)/__]: header

  [__@tagN@ /(input)/__]: type of dependency

  [__@flags@ /(input)/__]: unused

  __returns:__ new dependency set

__C declaration:__ @rpmdsNewPool@

__defined at:__ @rpm\/rpmds.h:417:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsNewPool_ptr :: Ptr.FunPtr (RPM.Types.RpmstrPool -> RPM.Types.Header -> RPM.Types.RpmTagVal -> FC.CInt -> IO RPM.Types.Rpmds)
rpmdsNewPool_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_45449719b7478d0d

foreign import ccall unsafe "hs_bindgen_6eae96497c98d13a" hs_bindgen_6eae96497c98d13a ::
     IO (Ptr.FunPtr (RPM.Types.RpmstrPool -> RPM.Types.Header -> RPM.Types.RpmTagVal -> RpmsenseFlags -> IO RPM.Types.Rpmds))

{-# NOINLINE rpmdsThisPool_ptr #-}

{-|

  > rpmds

  Create, load and initialize a dependency for this header.

  [__@pool@ /(input)/__]: string pool (or NULL for private pool)

  [__@h@ /(input)/__]: header

  [__@tagN@ /(input)/__]: type of dependency

  [__@Flags@ /(input)/__]: comparison flags

  __returns:__ new dependency set

__C declaration:__ @rpmdsThisPool@

__defined at:__ @rpm\/rpmds.h:427:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsThisPool_ptr :: Ptr.FunPtr (RPM.Types.RpmstrPool -> RPM.Types.Header -> RPM.Types.RpmTagVal -> RpmsenseFlags -> IO RPM.Types.Rpmds)
rpmdsThisPool_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6eae96497c98d13a

foreign import ccall unsafe "hs_bindgen_a25b8a8f4d722e4c" hs_bindgen_a25b8a8f4d722e4c ::
     IO (Ptr.FunPtr (RPM.Types.RpmstrPool -> RPM.Types.RpmTagVal -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> RpmsenseFlags -> IO RPM.Types.Rpmds))

{-# NOINLINE rpmdsSinglePool_ptr #-}

{-|

  > rpmds

  Create, load and initialize a dependency set of size 1.

  [__@pool@ /(input)/__]: string pool (or NULL for private pool)

  [__@tagN@ /(input)/__]: type of dependency

  [__@N@ /(input)/__]: name

  [__@EVR@ /(input)/__]: epoch:version-release

  [__@Flags@ /(input)/__]: comparison flags

  __returns:__ new dependency set

__C declaration:__ @rpmdsSinglePool@

__defined at:__ @rpm\/rpmds.h:439:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsSinglePool_ptr :: Ptr.FunPtr (RPM.Types.RpmstrPool -> RPM.Types.RpmTagVal -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> RpmsenseFlags -> IO RPM.Types.Rpmds)
rpmdsSinglePool_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a25b8a8f4d722e4c

foreign import ccall unsafe "hs_bindgen_b7d864a468a9efbd" hs_bindgen_b7d864a468a9efbd ::
     IO (Ptr.FunPtr (RPM.Types.RpmstrPool -> RPM.Types.RpmTagVal -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> RpmsenseFlags -> FC.CInt -> IO RPM.Types.Rpmds))

{-# NOINLINE rpmdsSinglePoolTix_ptr #-}

{-|

  > rpmds

  Create, load and initialize a trigger dependency set of size 1.

  [__@pool@ /(input)/__]: string pool (or NULL for private pool)

  [__@tagN@ /(input)/__]: type of dependency

  [__@N@ /(input)/__]: name

  [__@EVR@ /(input)/__]: epoch:version-release

  [__@Flags@ /(input)/__]: comparison flags

  [__@triggerIndex@ /(input)/__]: trigger index

  __returns:__ new dependency set

__C declaration:__ @rpmdsSinglePoolTix@

__defined at:__ @rpm\/rpmds.h:452:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsSinglePoolTix_ptr :: Ptr.FunPtr (RPM.Types.RpmstrPool -> RPM.Types.RpmTagVal -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> RpmsenseFlags -> FC.CInt -> IO RPM.Types.Rpmds)
rpmdsSinglePoolTix_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b7d864a468a9efbd

foreign import ccall unsafe "hs_bindgen_6f3d61022b1b6aaa" hs_bindgen_6f3d61022b1b6aaa ::
     IO (Ptr.FunPtr (RPM.Types.RpmstrPool -> (Ptr.Ptr RPM.Types.Rpmds) -> (Ptr.Ptr Void) -> IO FC.CInt))

{-# NOINLINE rpmdsRpmlibPool_ptr #-}

{-| Load rpmlib provides into a dependency set.

  [__@pool@ /(input)/__]: shared string pool (or NULL for private pool)

  [__@*dsp@ /(output)/__]: (loaded) dependency set

  [__@tblp@ /(input)/__]: rpmlib provides table (NULL uses internal table)

  __returns:__ 0 on success

__C declaration:__ @rpmdsRpmlibPool@

__defined at:__ @rpm\/rpmds.h:463:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsRpmlibPool_ptr :: Ptr.FunPtr (RPM.Types.RpmstrPool -> (Ptr.Ptr RPM.Types.Rpmds) -> (Ptr.Ptr Void) -> IO FC.CInt)
rpmdsRpmlibPool_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6f3d61022b1b6aaa

foreign import ccall unsafe "hs_bindgen_afdaf46453718193" hs_bindgen_afdaf46453718193 ::
     IO (Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> RpmrichParseFunction -> (Ptr.Ptr Void) -> IO RpmRC))

{-# NOINLINE rpmrichParse_ptr #-}

{-| Parse a rich dependency string

  [__@dstrp@ /(input)/__]: pointer to sting, will be updated

  [__@emsg@ /(input)/__]: returns the error string, can be NULL

  [__@cb@ /(input)/__]: callback function

  [__@cbdata@ /(input)/__]: callback function data

  __returns:__ RPMRC_OK on success

__C declaration:__ @rpmrichParse@

__defined at:__ @rpm\/rpmds.h:497:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmrichParse_ptr :: Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> RpmrichParseFunction -> (Ptr.Ptr Void) -> IO RpmRC)
rpmrichParse_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_afdaf46453718193

foreign import ccall unsafe "hs_bindgen_4baeab0b65e36988" hs_bindgen_4baeab0b65e36988 ::
     IO (Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> RpmrichParseFunction -> (Ptr.Ptr Void) -> RPM.Types.RpmTagVal -> IO RpmRC))

{-# NOINLINE rpmrichParseForTag_ptr #-}

{-| Parse a rich dependency string for a specific tag

  [__@dstrp@ /(input)/__]: pointer to sting, will be updated

  [__@emsg@ /(input)/__]: returns the error string, can be NULL

  [__@cb@ /(input)/__]: callback function

  [__@cbdata@ /(input)/__]: callback function data

  [__@tagN@ /(input)/__]: type of dependency

  __returns:__ RPMRC_OK on success

__C declaration:__ @rpmrichParseForTag@

__defined at:__ @rpm\/rpmds.h:508:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmrichParseForTag_ptr :: Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> RpmrichParseFunction -> (Ptr.Ptr Void) -> RPM.Types.RpmTagVal -> IO RpmRC)
rpmrichParseForTag_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4baeab0b65e36988

foreign import ccall unsafe "hs_bindgen_813829e7c480d380" hs_bindgen_813829e7c480d380 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt))

{-# NOINLINE rpmdsIsRich_ptr #-}

{-| Return if current depenency is rich

  [__@dep@ /(input)/__]: the dependency

  __returns:__ 1 is dependency is a rich 0 otherwise

__C declaration:__ @rpmdsIsRich@

__defined at:__ @rpm\/rpmds.h:516:5@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsIsRich_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> IO FC.CInt)
rpmdsIsRich_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_813829e7c480d380

foreign import ccall unsafe "hs_bindgen_0f77767012fdb62e" hs_bindgen_0f77767012fdb62e ::
     IO (Ptr.FunPtr (RpmrichOp -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmrichOpStr_ptr #-}

{-| Return a string representation of the rich dependency op

  [__@op@ /(input)/__]: the dependency op

  __returns:__ constant string, do not free

__C declaration:__ @rpmrichOpStr@

__defined at:__ @rpm\/rpmds.h:523:13@

__exported by:__ @rpm\/rpmds.h@
-}
rpmrichOpStr_ptr :: Ptr.FunPtr (RpmrichOp -> IO (Ptr.Ptr FC.CChar))
rpmrichOpStr_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0f77767012fdb62e

foreign import ccall unsafe "hs_bindgen_c1d7493614d7d82f" hs_bindgen_c1d7493614d7d82f ::
     IO (Ptr.FunPtr (RPM.Types.Rpmds -> (Ptr.Ptr RPM.Types.Rpmds) -> (Ptr.Ptr RPM.Types.Rpmds) -> (Ptr.Ptr RpmrichOp) -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RpmRC))

{-# NOINLINE rpmdsParseRichDep_ptr #-}

{-| Parse a rich dependency string

  [__@dep@ /(input)/__]: the dependency

  [__@leftds@ /(input)/__]: returns the left dependency

  [__@rightds@ /(input)/__]: returns the right dependency

  [__@op@ /(input)/__]: returns the rich dep op

  [__@emsg@ /(input)/__]: returns the error string

  __returns:__ RPMRC_OK on success

__C declaration:__ @rpmdsParseRichDep@

__defined at:__ @rpm\/rpmds.h:534:7@

__exported by:__ @rpm\/rpmds.h@
-}
rpmdsParseRichDep_ptr :: Ptr.FunPtr (RPM.Types.Rpmds -> (Ptr.Ptr RPM.Types.Rpmds) -> (Ptr.Ptr RPM.Types.Rpmds) -> (Ptr.Ptr RpmrichOp) -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RpmRC)
rpmdsParseRichDep_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c1d7493614d7d82f
