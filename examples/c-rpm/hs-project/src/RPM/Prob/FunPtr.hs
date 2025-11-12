{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Prob.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Types
import Prelude (IO)
import RPM.Prob

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmprob.h>"
  , "/* get_rpmProblemCreate_ptr */"
  , "__attribute__ ((const))"
  , "rpmProblem (*hs_bindgen_a024619c77ed531a (void)) ("
  , "  rpmProblemType arg1,"
  , "  char const *arg2,"
  , "  fnpyKey arg3,"
  , "  char const *arg4,"
  , "  char const *arg5,"
  , "  uint64_t arg6"
  , ")"
  , "{"
  , "  return &rpmProblemCreate;"
  , "}"
  , "/* get_rpmProblemFree_ptr */"
  , "__attribute__ ((const))"
  , "rpmProblem (*hs_bindgen_3ee47fbed8530037 (void)) ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return &rpmProblemFree;"
  , "}"
  , "/* get_rpmProblemLink_ptr */"
  , "__attribute__ ((const))"
  , "rpmProblem (*hs_bindgen_28d6fb26de1a1d84 (void)) ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return &rpmProblemLink;"
  , "}"
  , "/* get_rpmProblemCompare_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_d85d3f8ce8788daa (void)) ("
  , "  rpmProblem arg1,"
  , "  rpmProblem arg2"
  , ")"
  , "{"
  , "  return &rpmProblemCompare;"
  , "}"
  , "/* get_rpmProblemGetPkgNEVR_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_5a6556e5250be161 (void)) ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return &rpmProblemGetPkgNEVR;"
  , "}"
  , "/* get_rpmProblemGetAltNEVR_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_6504e3fe65e89c7a (void)) ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return &rpmProblemGetAltNEVR;"
  , "}"
  , "/* get_rpmProblemGetType_ptr */"
  , "__attribute__ ((const))"
  , "rpmProblemType (*hs_bindgen_9e1b1f9492c78fa2 (void)) ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return &rpmProblemGetType;"
  , "}"
  , "/* get_rpmProblemGetKey_ptr */"
  , "__attribute__ ((const))"
  , "fnpyKey (*hs_bindgen_265c42dd06ba6203 (void)) ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return &rpmProblemGetKey;"
  , "}"
  , "/* get_rpmProblemGetStr_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_6249fd913b14c95a (void)) ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return &rpmProblemGetStr;"
  , "}"
  , "/* get_rpmProblemGetDiskNeed_ptr */"
  , "__attribute__ ((const))"
  , "rpm_loff_t (*hs_bindgen_7ec08bf6b0e35bc6 (void)) ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return &rpmProblemGetDiskNeed;"
  , "}"
  , "/* get_rpmProblemString_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_6703ac5a5d5bb51d (void)) ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return &rpmProblemString;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_a024619c77ed531a" hs_bindgen_a024619c77ed531a ::
     IO (Ptr.FunPtr (RpmProblemType -> (Ptr.Ptr FC.CChar) -> RPM.Types.FnpyKey -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> HsBindgen.Runtime.Prelude.Word64 -> IO RpmProblem))

{-# NOINLINE rpmProblemCreate_ptr #-}

{-|

  > rpmprob

  Create a problem item.

  [__@type@ /(input)/__]: type of problem

  [__@pkgNEVR@ /(input)/__]: package name

  [__@key@ /(input)/__]: filename or python object address

  [__@altNEVR@ /(input)/__]: related (e.g. through a dependency) package name

  [__@str@ /(input)/__]: generic string attribute

  [__@number@ /(input)/__]: generic number attribute

  __returns:__ rpmProblem

__C declaration:__ @rpmProblemCreate@

__defined at:__ @rpm\/rpmprob.h:66:12@

__exported by:__ @rpm\/rpmprob.h@
-}
rpmProblemCreate_ptr :: Ptr.FunPtr (RpmProblemType -> (Ptr.Ptr FC.CChar) -> RPM.Types.FnpyKey -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> HsBindgen.Runtime.Prelude.Word64 -> IO RpmProblem)
rpmProblemCreate_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a024619c77ed531a

foreign import ccall unsafe "hs_bindgen_3ee47fbed8530037" hs_bindgen_3ee47fbed8530037 ::
     IO (Ptr.FunPtr (RpmProblem -> IO RpmProblem))

{-# NOINLINE rpmProblemFree_ptr #-}

{-|

  > rpmprob

  Destroy a problem item.

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ rpm problem (NULL)

__C declaration:__ @rpmProblemFree@

__defined at:__ @rpm\/rpmprob.h:76:12@

__exported by:__ @rpm\/rpmprob.h@
-}
rpmProblemFree_ptr :: Ptr.FunPtr (RpmProblem -> IO RpmProblem)
rpmProblemFree_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3ee47fbed8530037

foreign import ccall unsafe "hs_bindgen_28d6fb26de1a1d84" hs_bindgen_28d6fb26de1a1d84 ::
     IO (Ptr.FunPtr (RpmProblem -> IO RpmProblem))

{-# NOINLINE rpmProblemLink_ptr #-}

{-|

  > rpmprob

  Reference an rpmProblem instance

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ rpm problem

__C declaration:__ @rpmProblemLink@

__defined at:__ @rpm\/rpmprob.h:83:12@

__exported by:__ @rpm\/rpmprob.h@
-}
rpmProblemLink_ptr :: Ptr.FunPtr (RpmProblem -> IO RpmProblem)
rpmProblemLink_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_28d6fb26de1a1d84

foreign import ccall unsafe "hs_bindgen_d85d3f8ce8788daa" hs_bindgen_d85d3f8ce8788daa ::
     IO (Ptr.FunPtr (RpmProblem -> RpmProblem -> IO FC.CInt))

{-# NOINLINE rpmProblemCompare_ptr #-}

{-|

  > rpmprob

  Compare two problems for equality.

  [__@ap@ /(input)/__]: 1st problem

  [__@bp@ /(input)/__]: 2nd problem

  __returns:__ 1 if the problems differ, 0 otherwise

__C declaration:__ @rpmProblemCompare@

__defined at:__ @rpm\/rpmprob.h:91:5@

__exported by:__ @rpm\/rpmprob.h@
-}
rpmProblemCompare_ptr :: Ptr.FunPtr (RpmProblem -> RpmProblem -> IO FC.CInt)
rpmProblemCompare_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d85d3f8ce8788daa

foreign import ccall unsafe "hs_bindgen_5a6556e5250be161" hs_bindgen_5a6556e5250be161 ::
     IO (Ptr.FunPtr (RpmProblem -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmProblemGetPkgNEVR_ptr #-}

{-|

  > rpmprob

  Return package NEVR

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ package NEVR

__C declaration:__ @rpmProblemGetPkgNEVR@

__defined at:__ @rpm\/rpmprob.h:99:14@

__exported by:__ @rpm\/rpmprob.h@
-}
rpmProblemGetPkgNEVR_ptr :: Ptr.FunPtr (RpmProblem -> IO (Ptr.Ptr FC.CChar))
rpmProblemGetPkgNEVR_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5a6556e5250be161

foreign import ccall unsafe "hs_bindgen_6504e3fe65e89c7a" hs_bindgen_6504e3fe65e89c7a ::
     IO (Ptr.FunPtr (RpmProblem -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmProblemGetAltNEVR_ptr #-}

{-|

  > rpmprob

  Return related (e.g. through a dependency) package NEVR

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ related (e.g. through a dependency) package NEVR

__C declaration:__ @rpmProblemGetAltNEVR@

__defined at:__ @rpm\/rpmprob.h:105:14@

__exported by:__ @rpm\/rpmprob.h@
-}
rpmProblemGetAltNEVR_ptr :: Ptr.FunPtr (RpmProblem -> IO (Ptr.Ptr FC.CChar))
rpmProblemGetAltNEVR_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6504e3fe65e89c7a

foreign import ccall unsafe "hs_bindgen_9e1b1f9492c78fa2" hs_bindgen_9e1b1f9492c78fa2 ::
     IO (Ptr.FunPtr (RpmProblem -> IO RpmProblemType))

{-# NOINLINE rpmProblemGetType_ptr #-}

{-|

  > rpmprob

  Return type of problem (dependency, diskpace etc)

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ type of problem

__C declaration:__ @rpmProblemGetType@

__defined at:__ @rpm\/rpmprob.h:113:16@

__exported by:__ @rpm\/rpmprob.h@
-}
rpmProblemGetType_ptr :: Ptr.FunPtr (RpmProblem -> IO RpmProblemType)
rpmProblemGetType_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9e1b1f9492c78fa2

foreign import ccall unsafe "hs_bindgen_265c42dd06ba6203" hs_bindgen_265c42dd06ba6203 ::
     IO (Ptr.FunPtr (RpmProblem -> IO RPM.Types.FnpyKey))

{-# NOINLINE rpmProblemGetKey_ptr #-}

{-|

  > rpmprob

  Return filename or python object address of a problem

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ filename or python object address

__C declaration:__ @rpmProblemGetKey@

__defined at:__ @rpm\/rpmprob.h:120:9@

__exported by:__ @rpm\/rpmprob.h@
-}
rpmProblemGetKey_ptr :: Ptr.FunPtr (RpmProblem -> IO RPM.Types.FnpyKey)
rpmProblemGetKey_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_265c42dd06ba6203

foreign import ccall unsafe "hs_bindgen_6249fd913b14c95a" hs_bindgen_6249fd913b14c95a ::
     IO (Ptr.FunPtr (RpmProblem -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmProblemGetStr_ptr #-}

{-|

  > rpmprob

  Return a generic data string from a problem

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ a generic data string

  __TODO:__

  needs a better name

__C declaration:__ @rpmProblemGetStr@

__defined at:__ @rpm\/rpmprob.h:128:14@

__exported by:__ @rpm\/rpmprob.h@
-}
rpmProblemGetStr_ptr :: Ptr.FunPtr (RpmProblem -> IO (Ptr.Ptr FC.CChar))
rpmProblemGetStr_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6249fd913b14c95a

foreign import ccall unsafe "hs_bindgen_7ec08bf6b0e35bc6" hs_bindgen_7ec08bf6b0e35bc6 ::
     IO (Ptr.FunPtr (RpmProblem -> IO RPM.Types.Rpm_loff_t))

{-# NOINLINE rpmProblemGetDiskNeed_ptr #-}

{-|

  > rpmprob

  Return disk requirement (needed disk space / number of inodes) depending on problem type. On problem types other than RPMPROB_DISKSPACE and RPMPROB_DISKNODES return value is undefined.

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ disk requirement

__C declaration:__ @rpmProblemGetDiskNeed@

__defined at:__ @rpm\/rpmprob.h:137:12@

__exported by:__ @rpm\/rpmprob.h@
-}
rpmProblemGetDiskNeed_ptr :: Ptr.FunPtr (RpmProblem -> IO RPM.Types.Rpm_loff_t)
rpmProblemGetDiskNeed_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7ec08bf6b0e35bc6

foreign import ccall unsafe "hs_bindgen_6703ac5a5d5bb51d" hs_bindgen_6703ac5a5d5bb51d ::
     IO (Ptr.FunPtr (RpmProblem -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmProblemString_ptr #-}

{-|

  > rpmprob

  Return formatted string representation of a problem.

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ formatted string (malloc'd)

__C declaration:__ @rpmProblemString@

__defined at:__ @rpm\/rpmprob.h:144:8@

__exported by:__ @rpm\/rpmprob.h@
-}
rpmProblemString_ptr :: Ptr.FunPtr (RpmProblem -> IO (Ptr.Ptr FC.CChar))
rpmProblemString_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6703ac5a5d5bb51d
