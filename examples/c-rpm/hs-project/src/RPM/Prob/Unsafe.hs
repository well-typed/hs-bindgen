{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Prob.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Types
import Prelude (IO)
import RPM.Prob

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmprob.h>"
  , "rpmProblem hs_bindgen_1a12c0c62674639a ("
  , "  rpmProblemType arg1,"
  , "  char const *arg2,"
  , "  fnpyKey arg3,"
  , "  char const *arg4,"
  , "  char const *arg5,"
  , "  uint64_t arg6"
  , ")"
  , "{"
  , "  return rpmProblemCreate(arg1, arg2, arg3, arg4, arg5, arg6);"
  , "}"
  , "rpmProblem hs_bindgen_23c7e559eb25ae6e ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemFree(arg1);"
  , "}"
  , "rpmProblem hs_bindgen_68de91a144a6f16c ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemLink(arg1);"
  , "}"
  , "signed int hs_bindgen_b34b5e2f621ee730 ("
  , "  rpmProblem arg1,"
  , "  rpmProblem arg2"
  , ")"
  , "{"
  , "  return rpmProblemCompare(arg1, arg2);"
  , "}"
  , "char const *hs_bindgen_ba56520cfee91be8 ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemGetPkgNEVR(arg1);"
  , "}"
  , "char const *hs_bindgen_61561e7c29b1cb4b ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemGetAltNEVR(arg1);"
  , "}"
  , "rpmProblemType hs_bindgen_fd7bf8dbfa2f2b3b ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemGetType(arg1);"
  , "}"
  , "fnpyKey hs_bindgen_072f94734f2534bf ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemGetKey(arg1);"
  , "}"
  , "char const *hs_bindgen_455d57ca060b5818 ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemGetStr(arg1);"
  , "}"
  , "rpm_loff_t hs_bindgen_ea7fa9393a04a36e ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemGetDiskNeed(arg1);"
  , "}"
  , "char *hs_bindgen_27f7ccc2841e28ff ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemString(arg1);"
  , "}"
  ]))

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
foreign import ccall unsafe "hs_bindgen_1a12c0c62674639a" rpmProblemCreate ::
     RpmProblemType
     {- ^ __C declaration:__ @type'@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@pkgNEVR@ /(input)/__]: package name

     __C declaration:__ @pkgNEVR@
     -}
  -> RPM.Types.FnpyKey
     {- ^

        [__@key@ /(input)/__]: filename or python object address

     __C declaration:__ @key@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@altNEVR@ /(input)/__]: related (e.g. through a dependency) package name

     __C declaration:__ @altNEVR@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@str@ /(input)/__]: generic string attribute

     __C declaration:__ @str@
     -}
  -> HsBindgen.Runtime.Prelude.Word64
     {- ^

        [__@number@ /(input)/__]: generic number attribute

     __C declaration:__ @number@
     -}
  -> IO RpmProblem

{-|

  > rpmprob

  Destroy a problem item.

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ rpm problem (NULL)

__C declaration:__ @rpmProblemFree@

__defined at:__ @rpm\/rpmprob.h:76:12@

__exported by:__ @rpm\/rpmprob.h@
-}
foreign import ccall unsafe "hs_bindgen_23c7e559eb25ae6e" rpmProblemFree ::
     RpmProblem
     {- ^

        [__@prob@ /(input)/__]: rpm problem

     __C declaration:__ @prob@
     -}
  -> IO RpmProblem

{-|

  > rpmprob

  Reference an rpmProblem instance

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ rpm problem

__C declaration:__ @rpmProblemLink@

__defined at:__ @rpm\/rpmprob.h:83:12@

__exported by:__ @rpm\/rpmprob.h@
-}
foreign import ccall unsafe "hs_bindgen_68de91a144a6f16c" rpmProblemLink ::
     RpmProblem
     {- ^

        [__@prob@ /(input)/__]: rpm problem

     __C declaration:__ @prob@
     -}
  -> IO RpmProblem

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
foreign import ccall unsafe "hs_bindgen_b34b5e2f621ee730" rpmProblemCompare ::
     RpmProblem
     {- ^

        [__@ap@ /(input)/__]: 1st problem

     __C declaration:__ @ap@
     -}
  -> RpmProblem
     {- ^

        [__@bp@ /(input)/__]: 2nd problem

     __C declaration:__ @bp@
     -}
  -> IO FC.CInt

{-|

  > rpmprob

  Return package NEVR

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ package NEVR

__C declaration:__ @rpmProblemGetPkgNEVR@

__defined at:__ @rpm\/rpmprob.h:99:14@

__exported by:__ @rpm\/rpmprob.h@
-}
foreign import ccall unsafe "hs_bindgen_ba56520cfee91be8" rpmProblemGetPkgNEVR ::
     RpmProblem
     {- ^

        [__@prob@ /(input)/__]: rpm problem

     __C declaration:__ @prob@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmprob

  Return related (e.g. through a dependency) package NEVR

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ related (e.g. through a dependency) package NEVR

__C declaration:__ @rpmProblemGetAltNEVR@

__defined at:__ @rpm\/rpmprob.h:105:14@

__exported by:__ @rpm\/rpmprob.h@
-}
foreign import ccall unsafe "hs_bindgen_61561e7c29b1cb4b" rpmProblemGetAltNEVR ::
     RpmProblem
     {- ^

        [__@prob@ /(input)/__]: rpm problem

     __C declaration:__ @prob@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmprob

  Return type of problem (dependency, diskpace etc)

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ type of problem

__C declaration:__ @rpmProblemGetType@

__defined at:__ @rpm\/rpmprob.h:113:16@

__exported by:__ @rpm\/rpmprob.h@
-}
foreign import ccall unsafe "hs_bindgen_fd7bf8dbfa2f2b3b" rpmProblemGetType ::
     RpmProblem
     {- ^

        [__@prob@ /(input)/__]: rpm problem

     __C declaration:__ @prob@
     -}
  -> IO RpmProblemType

{-|

  > rpmprob

  Return filename or python object address of a problem

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ filename or python object address

__C declaration:__ @rpmProblemGetKey@

__defined at:__ @rpm\/rpmprob.h:120:9@

__exported by:__ @rpm\/rpmprob.h@
-}
foreign import ccall unsafe "hs_bindgen_072f94734f2534bf" rpmProblemGetKey ::
     RpmProblem
     {- ^

        [__@prob@ /(input)/__]: rpm problem

     __C declaration:__ @prob@
     -}
  -> IO RPM.Types.FnpyKey

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
foreign import ccall unsafe "hs_bindgen_455d57ca060b5818" rpmProblemGetStr ::
     RpmProblem
     {- ^

        [__@prob@ /(input)/__]: rpm problem

     __C declaration:__ @prob@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmprob

  Return disk requirement (needed disk space / number of inodes) depending on problem type. On problem types other than RPMPROB_DISKSPACE and RPMPROB_DISKNODES return value is undefined.

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ disk requirement

__C declaration:__ @rpmProblemGetDiskNeed@

__defined at:__ @rpm\/rpmprob.h:137:12@

__exported by:__ @rpm\/rpmprob.h@
-}
foreign import ccall unsafe "hs_bindgen_ea7fa9393a04a36e" rpmProblemGetDiskNeed ::
     RpmProblem
     {- ^

        [__@prob@ /(input)/__]: rpm problem

     __C declaration:__ @prob@
     -}
  -> IO RPM.Types.Rpm_loff_t

{-|

  > rpmprob

  Return formatted string representation of a problem.

  [__@prob@ /(input)/__]: rpm problem

  __returns:__ formatted string (malloc'd)

__C declaration:__ @rpmProblemString@

__defined at:__ @rpm\/rpmprob.h:144:8@

__exported by:__ @rpm\/rpmprob.h@
-}
foreign import ccall unsafe "hs_bindgen_27f7ccc2841e28ff" rpmProblemString ::
     RpmProblem
     {- ^

        [__@prob@ /(input)/__]: rpm problem

     __C declaration:__ @prob@
     -}
  -> IO (Ptr.Ptr FC.CChar)
