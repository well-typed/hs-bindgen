{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Prob.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Types
import Prelude (IO)
import RPM.Prob

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmprob.h>"
  , "rpmProblem hs_bindgen_462151c35c188f7f ("
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
  , "rpmProblem hs_bindgen_8dd2f46cb4c7a2d7 ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemFree(arg1);"
  , "}"
  , "rpmProblem hs_bindgen_2da2de9aceb79650 ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemLink(arg1);"
  , "}"
  , "signed int hs_bindgen_c4fb3015155ce833 ("
  , "  rpmProblem arg1,"
  , "  rpmProblem arg2"
  , ")"
  , "{"
  , "  return rpmProblemCompare(arg1, arg2);"
  , "}"
  , "char const *hs_bindgen_aa6dd9aba5d6ef5b ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemGetPkgNEVR(arg1);"
  , "}"
  , "char const *hs_bindgen_1f7ac7271def0d52 ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemGetAltNEVR(arg1);"
  , "}"
  , "rpmProblemType hs_bindgen_feb27afd51f3a1bc ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemGetType(arg1);"
  , "}"
  , "fnpyKey hs_bindgen_c3f290389643d055 ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemGetKey(arg1);"
  , "}"
  , "char const *hs_bindgen_729a65ced8c316ed ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemGetStr(arg1);"
  , "}"
  , "rpm_loff_t hs_bindgen_2506fe8b9a2ed2d6 ("
  , "  rpmProblem arg1"
  , ")"
  , "{"
  , "  return rpmProblemGetDiskNeed(arg1);"
  , "}"
  , "char *hs_bindgen_db4bd9609a89951f ("
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
foreign import ccall safe "hs_bindgen_462151c35c188f7f" rpmProblemCreate ::
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
foreign import ccall safe "hs_bindgen_8dd2f46cb4c7a2d7" rpmProblemFree ::
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
foreign import ccall safe "hs_bindgen_2da2de9aceb79650" rpmProblemLink ::
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
foreign import ccall safe "hs_bindgen_c4fb3015155ce833" rpmProblemCompare ::
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
foreign import ccall safe "hs_bindgen_aa6dd9aba5d6ef5b" rpmProblemGetPkgNEVR ::
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
foreign import ccall safe "hs_bindgen_1f7ac7271def0d52" rpmProblemGetAltNEVR ::
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
foreign import ccall safe "hs_bindgen_feb27afd51f3a1bc" rpmProblemGetType ::
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
foreign import ccall safe "hs_bindgen_c3f290389643d055" rpmProblemGetKey ::
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
foreign import ccall safe "hs_bindgen_729a65ced8c316ed" rpmProblemGetStr ::
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
foreign import ccall safe "hs_bindgen_2506fe8b9a2ed2d6" rpmProblemGetDiskNeed ::
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
foreign import ccall safe "hs_bindgen_db4bd9609a89951f" rpmProblemString ::
     RpmProblem
     {- ^

        [__@prob@ /(input)/__]: rpm problem

     __C declaration:__ @prob@
     -}
  -> IO (Ptr.Ptr FC.CChar)
