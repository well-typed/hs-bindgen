{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Ds.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Types
import Data.Void (Void)
import Prelude (IO)
import RPM.Ds

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmds.h>"
  , "rpmsenseFlags hs_bindgen_69ef4baa7439dc61 ("
  , "  rpmTagVal arg1,"
  , "  rpmsenseFlags arg2"
  , ")"
  , "{"
  , "  return rpmSanitizeDSFlags(arg1, arg2);"
  , "}"
  , "rpmsenseFlags hs_bindgen_3f0c5419ec63626f ("
  , "  char const *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return rpmParseDSFlags(arg1, arg2);"
  , "}"
  , "rpmds hs_bindgen_1bc4ab05f0edb9c9 ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return rpmdsLink(arg1);"
  , "}"
  , "rpmds hs_bindgen_a3420c537272d5f2 ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return rpmdsFree(arg1);"
  , "}"
  , "rpmds hs_bindgen_9a6549070630ba09 ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return rpmdsNew(arg1, arg2, arg3);"
  , "}"
  , "char *hs_bindgen_752251de2b69a822 ("
  , "  char const *arg1,"
  , "  rpmds const arg2"
  , ")"
  , "{"
  , "  return rpmdsNewDNEVR(arg1, arg2);"
  , "}"
  , "rpmds hs_bindgen_c4b25c962012780a ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  rpmsenseFlags arg3"
  , ")"
  , "{"
  , "  return rpmdsThis(arg1, arg2, arg3);"
  , "}"
  , "rpmds hs_bindgen_3b07468c4510d0e2 ("
  , "  rpmTagVal arg1,"
  , "  char const *arg2,"
  , "  char const *arg3,"
  , "  rpmsenseFlags arg4"
  , ")"
  , "{"
  , "  return rpmdsSingle(arg1, arg2, arg3, arg4);"
  , "}"
  , "rpmds hs_bindgen_43a7a7a73da8ebee ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return rpmdsCurrent(arg1);"
  , "}"
  , "signed int hs_bindgen_3192b159b3c073ea ("
  , "  rpmds arg1,"
  , "  Header arg2"
  , ")"
  , "{"
  , "  return rpmdsPutToHeader(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_508de10b6846b58d ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return rpmdsCount(arg1);"
  , "}"
  , "signed int hs_bindgen_f201260148336b0d ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return rpmdsIx(arg1);"
  , "}"
  , "signed int hs_bindgen_4950f3ab1bc28c75 ("
  , "  rpmds arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return rpmdsSetIx(arg1, arg2);"
  , "}"
  , "char const *hs_bindgen_5469c9820b6dd5e2 ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return rpmdsDNEVR(arg1);"
  , "}"
  , "char hs_bindgen_57c06ab4b2df8f10 ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return rpmdsD(arg1);"
  , "}"
  , "rpmTagVal hs_bindgen_cef508f495fc4b9a ("
  , "  char arg1"
  , ")"
  , "{"
  , "  return rpmdsDToTagN(arg1);"
  , "}"
  , "char const *hs_bindgen_4f9da71729e09e34 ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return rpmdsN(arg1);"
  , "}"
  , "char const *hs_bindgen_79bf44f557e5877b ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return rpmdsEVR(arg1);"
  , "}"
  , "signed int hs_bindgen_44d733e199d1139a ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return rpmdsTi(arg1);"
  , "}"
  , "rpmsenseFlags hs_bindgen_d40455c2e1141e86 ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return rpmdsFlags(arg1);"
  , "}"
  , "rpmTagVal hs_bindgen_c5d586fb89fa87e4 ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return rpmdsTagN(arg1);"
  , "}"
  , "rpmTagVal hs_bindgen_5e9a307330f501af ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return rpmdsTagEVR(arg1);"
  , "}"
  , "rpmTagVal hs_bindgen_4ad39e4887d24666 ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return rpmdsTagF(arg1);"
  , "}"
  , "rpmTagVal hs_bindgen_10489a1cd68abe6d ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return rpmdsTagTi(arg1);"
  , "}"
  , "unsigned int hs_bindgen_39d801d0f4ce1cbe ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return rpmdsInstance(arg1);"
  , "}"
  , "signed int hs_bindgen_2bee558755ddb170 ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return rpmdsIsWeak(arg1);"
  , "}"
  , "signed int hs_bindgen_5083cc0bd06eba2b ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return rpmdsIsReverse(arg1);"
  , "}"
  , "signed int hs_bindgen_58eea97e0081b91b ("
  , "  rpmds arg1,"
  , "  char **arg2"
  , ")"
  , "{"
  , "  return rpmdsIsSysuser(arg1, arg2);"
  , "}"
  , "rpm_color_t hs_bindgen_b9409e41897ba84b ("
  , "  rpmds const arg1"
  , ")"
  , "{"
  , "  return rpmdsColor(arg1);"
  , "}"
  , "rpm_color_t hs_bindgen_d5f45a8507377206 ("
  , "  rpmds const arg1,"
  , "  rpm_color_t arg2"
  , ")"
  , "{"
  , "  return rpmdsSetColor(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_43a0184dcb9cc67a ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return rpmdsNext(arg1);"
  , "}"
  , "rpmds hs_bindgen_6fb494a0176d7b20 ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return rpmdsInit(arg1);"
  , "}"
  , "signed int hs_bindgen_426ced0e04a70163 ("
  , "  rpmds arg1,"
  , "  rpmds const arg2"
  , ")"
  , "{"
  , "  return rpmdsFind(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_e277ecc0f34b86d5 ("
  , "  rpmds *arg1,"
  , "  rpmds arg2"
  , ")"
  , "{"
  , "  return rpmdsMerge(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_d532422fa8af57af ("
  , "  rpmds arg1,"
  , "  rpmds arg2"
  , ")"
  , "{"
  , "  return rpmdsSearch(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_b93fec67049f804b ("
  , "  rpmds const arg1,"
  , "  rpmds const arg2"
  , ")"
  , "{"
  , "  return rpmdsCompare(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_30afc8c826a91d51 ("
  , "  Header const arg1,"
  , "  rpmds const arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return rpmdsAnyMatchesDep(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_4ea1c37d20673aa3 ("
  , "  Header const arg1,"
  , "  signed int arg2,"
  , "  rpmds const arg3,"
  , "  signed int arg4"
  , ")"
  , "{"
  , "  return rpmdsMatchesDep(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_6000edc54a11d65e ("
  , "  Header const arg1,"
  , "  rpmds const arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return rpmdsNVRMatchesDep(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_0f20b6fb9e6691af ("
  , "  rpmds *arg1,"
  , "  void const *arg2"
  , ")"
  , "{"
  , "  return rpmdsRpmlib(arg1, arg2);"
  , "}"
  , "rpmds hs_bindgen_d3e73232f24348de ("
  , "  rpmstrPool arg1,"
  , "  Header arg2,"
  , "  rpmTagVal arg3,"
  , "  signed int arg4"
  , ")"
  , "{"
  , "  return rpmdsNewPool(arg1, arg2, arg3, arg4);"
  , "}"
  , "rpmds hs_bindgen_1b214d6d8d8f398b ("
  , "  rpmstrPool arg1,"
  , "  Header arg2,"
  , "  rpmTagVal arg3,"
  , "  rpmsenseFlags arg4"
  , ")"
  , "{"
  , "  return rpmdsThisPool(arg1, arg2, arg3, arg4);"
  , "}"
  , "rpmds hs_bindgen_d4ed0ea9ebcfc454 ("
  , "  rpmstrPool arg1,"
  , "  rpmTagVal arg2,"
  , "  char const *arg3,"
  , "  char const *arg4,"
  , "  rpmsenseFlags arg5"
  , ")"
  , "{"
  , "  return rpmdsSinglePool(arg1, arg2, arg3, arg4, arg5);"
  , "}"
  , "rpmds hs_bindgen_c973e5fdbb14c650 ("
  , "  rpmstrPool arg1,"
  , "  rpmTagVal arg2,"
  , "  char const *arg3,"
  , "  char const *arg4,"
  , "  rpmsenseFlags arg5,"
  , "  signed int arg6"
  , ")"
  , "{"
  , "  return rpmdsSinglePoolTix(arg1, arg2, arg3, arg4, arg5, arg6);"
  , "}"
  , "signed int hs_bindgen_85a8400759ff705b ("
  , "  rpmstrPool arg1,"
  , "  rpmds *arg2,"
  , "  void const *arg3"
  , ")"
  , "{"
  , "  return rpmdsRpmlibPool(arg1, arg2, arg3);"
  , "}"
  , "rpmRC hs_bindgen_a10227a2effa0f3e ("
  , "  char const **arg1,"
  , "  char **arg2,"
  , "  rpmrichParseFunction arg3,"
  , "  void *arg4"
  , ")"
  , "{"
  , "  return rpmrichParse(arg1, arg2, arg3, arg4);"
  , "}"
  , "rpmRC hs_bindgen_c06eace8272e2e00 ("
  , "  char const **arg1,"
  , "  char **arg2,"
  , "  rpmrichParseFunction arg3,"
  , "  void *arg4,"
  , "  rpmTagVal arg5"
  , ")"
  , "{"
  , "  return rpmrichParseForTag(arg1, arg2, arg3, arg4, arg5);"
  , "}"
  , "signed int hs_bindgen_29b3e3541fc0ddca ("
  , "  rpmds arg1"
  , ")"
  , "{"
  , "  return rpmdsIsRich(arg1);"
  , "}"
  , "char const *hs_bindgen_94e5d78fe9b6af2a ("
  , "  rpmrichOp arg1"
  , ")"
  , "{"
  , "  return rpmrichOpStr(arg1);"
  , "}"
  , "rpmRC hs_bindgen_505626fa82b309b2 ("
  , "  rpmds arg1,"
  , "  rpmds *arg2,"
  , "  rpmds *arg3,"
  , "  rpmrichOp *arg4,"
  , "  char **arg5"
  , ")"
  , "{"
  , "  return rpmdsParseRichDep(arg1, arg2, arg3, arg4, arg5);"
  , "}"
  ]))

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
foreign import ccall unsafe "hs_bindgen_69ef4baa7439dc61" rpmSanitizeDSFlags ::
     RPM.Types.RpmTagVal
     {- ^

        [__@tagN@ /(input)/__]: type of dependency

     __C declaration:__ @tagN@
     -}
  -> RpmsenseFlags
     {- ^ __C declaration:__ @flags@
     -}
  -> IO RpmsenseFlags

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
foreign import ccall unsafe "hs_bindgen_3f0c5419ec63626f" rpmParseDSFlags ::
     Ptr.Ptr FC.CChar
     {- ^

        [__@str@ /(input)/__]: the string

     __C declaration:__ @str@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^

        [__@len@ /(input)/__]: length of the string

     __C declaration:__ @len@
     -}
  -> IO RpmsenseFlags

{-|

  > rpmds

  Reference a dependency set instance.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ new dependency set reference

__C declaration:__ @rpmdsLink@

__defined at:__ @rpm\/rpmds.h:121:7@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_1bc4ab05f0edb9c9" rpmdsLink ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO RPM.Types.Rpmds

{-|

  > rpmds

  Destroy a dependency set.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ NULL always

__C declaration:__ @rpmdsFree@

__defined at:__ @rpm\/rpmds.h:128:7@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_a3420c537272d5f2" rpmdsFree ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO RPM.Types.Rpmds

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
foreign import ccall unsafe "hs_bindgen_9a6549070630ba09" rpmdsNew ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Types.RpmTagVal
     {- ^

        [__@tagN@ /(input)/__]: type of dependency

     __C declaration:__ @tagN@
     -}
  -> FC.CInt
     {- ^

        [__@flags@ /(input)/__]: unused

     __C declaration:__ @flags@
     -}
  -> IO RPM.Types.Rpmds

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
foreign import ccall unsafe "hs_bindgen_752251de2b69a822" rpmdsNewDNEVR ::
     Ptr.Ptr FC.CChar
     {- ^

        [__@dspfx@ /(input)/__]: formatted dependency string prefix

     __C declaration:__ @dspfx@
     -}
  -> RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO (Ptr.Ptr FC.CChar)

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
foreign import ccall unsafe "hs_bindgen_c4b25c962012780a" rpmdsThis ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Types.RpmTagVal
     {- ^

        [__@tagN@ /(input)/__]: type of dependency

     __C declaration:__ @tagN@
     -}
  -> RpmsenseFlags
     {- ^ __C declaration:__ @flags@
     -}
  -> IO RPM.Types.Rpmds

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
foreign import ccall unsafe "hs_bindgen_3b07468c4510d0e2" rpmdsSingle ::
     RPM.Types.RpmTagVal
     {- ^

        [__@tagN@ /(input)/__]: type of dependency

     __C declaration:__ @tagN@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @n@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @eVR@
     -}
  -> RpmsenseFlags
     {- ^ __C declaration:__ @flags@
     -}
  -> IO RPM.Types.Rpmds

{-|

  > rpmds

  Return a new dependency set of size 1 from the current iteration index

  [__@ds@ /(input)/__]: dependency set

  __returns:__ new dependency set

__C declaration:__ @rpmdsCurrent@

__defined at:__ @rpm\/rpmds.h:171:7@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_43a7a7a73da8ebee" rpmdsCurrent ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO RPM.Types.Rpmds

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
foreign import ccall unsafe "hs_bindgen_3192b159b3c073ea" rpmdsPutToHeader ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> IO FC.CInt

{-|

  > rpmds

  Return dependency set count.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current count

__C declaration:__ @rpmdsCount@

__defined at:__ @rpm\/rpmds.h:186:5@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_508de10b6846b58d" rpmdsCount ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO FC.CInt

{-|

  > rpmds

  Return dependency set index.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current index

__C declaration:__ @rpmdsIx@

__defined at:__ @rpm\/rpmds.h:193:5@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_f201260148336b0d" rpmdsIx ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_4950f3ab1bc28c75" rpmdsSetIx ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> FC.CInt
     {- ^

        [__@ix@ /(input)/__]: new index

     __C declaration:__ @ix@
     -}
  -> IO FC.CInt

{-|

  > rpmds

  Return current formatted dependency string.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency DNEVR, NULL on invalid

__C declaration:__ @rpmdsDNEVR@

__defined at:__ @rpm\/rpmds.h:208:14@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_5469c9820b6dd5e2" rpmdsDNEVR ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmds

  Return one char indicating the type of the dependency.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ character

__C declaration:__ @rpmdsD@

__defined at:__ @rpm\/rpmds.h:215:6@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_57c06ab4b2df8f10" rpmdsD ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO FC.CChar

{-|

  > rpmds

  Return matching tagN for one char dependency type description.

  [__@deptype@ /(input)/__]: character

  __returns:__ type of dependency

__C declaration:__ @rpmdsDToTagN@

__defined at:__ @rpm\/rpmds.h:222:11@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_cef508f495fc4b9a" rpmdsDToTagN ::
     FC.CChar
     {- ^

        [__@deptype@ /(input)/__]: character

     __C declaration:__ @deptype@
     -}
  -> IO RPM.Types.RpmTagVal

{-|

  > rpmds

  Return current dependency name.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency name, NULL on invalid

__C declaration:__ @rpmdsN@

__defined at:__ @rpm\/rpmds.h:229:14@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_4f9da71729e09e34" rpmdsN ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmds

  Return current dependency epoch-version-release.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency EVR, NULL on invalid

__C declaration:__ @rpmdsEVR@

__defined at:__ @rpm\/rpmds.h:236:14@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_79bf44f557e5877b" rpmdsEVR ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmds

  Return current dependency triggerindex.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency trigger index, -1 on invalid

__C declaration:__ @rpmdsTi@

__defined at:__ @rpm\/rpmds.h:243:5@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_44d733e199d1139a" rpmdsTi ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO FC.CInt

{-|

  > rpmds

  Return current dependency flags.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency flags, 0 on invalid

__C declaration:__ @rpmdsFlags@

__defined at:__ @rpm\/rpmds.h:250:15@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_d40455c2e1141e86" rpmdsFlags ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO RpmsenseFlags

{-|

  > rpmds

  Return current dependency type.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency type, 0 on invalid

__C declaration:__ @rpmdsTagN@

__defined at:__ @rpm\/rpmds.h:257:11@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_c5d586fb89fa87e4" rpmdsTagN ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO RPM.Types.RpmTagVal

{-|

  > rpmds

  Return current dependency type.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency type version tag, 0 on invalid

__C declaration:__ @rpmdsTagEVR@

__defined at:__ @rpm\/rpmds.h:264:11@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_5e9a307330f501af" rpmdsTagEVR ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO RPM.Types.RpmTagVal

{-|

  > rpmds

  Return current dependency type.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency type flags tag, 0 on invalid

__C declaration:__ @rpmdsTagF@

__defined at:__ @rpm\/rpmds.h:271:11@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_4ad39e4887d24666" rpmdsTagF ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO RPM.Types.RpmTagVal

{-|

  > rpmds

  Return current dependency type.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency type trigger index tag, 0 on invalid

__C declaration:__ @rpmdsTagTi@

__defined at:__ @rpm\/rpmds.h:278:11@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_10489a1cd68abe6d" rpmdsTagTi ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO RPM.Types.RpmTagVal

{-|

  > rpmds

  Return dependency header instance, ie whether the dependency comes from an installed header or not.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ header instance of dependency (0 for not installed)

__C declaration:__ @rpmdsInstance@

__defined at:__ @rpm\/rpmds.h:286:14@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_39d801d0f4ce1cbe" rpmdsInstance ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO FC.CUInt

{-|

  > rpmds

  Return whether dependency is weak

  [__@ds@ /(input)/__]: dependency set

  __returns:__ 1 if weak, 0 if not

__C declaration:__ @rpmdsIsWeak@

__defined at:__ @rpm\/rpmds.h:293:5@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_2bee558755ddb170" rpmdsIsWeak ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO FC.CInt

{-|

  > rpmds

  Return whether dependency is reversed

  [__@ds@ /(input)/__]: dependency set

  __returns:__ 1 if reversed, 0 if not

__C declaration:__ @rpmdsIsReverse@

__defined at:__ @rpm\/rpmds.h:300:5@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_5083cc0bd06eba2b" rpmdsIsReverse ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_58eea97e0081b91b" rpmdsIsSysuser ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^

        [__@sysuser@ /(output)/__]: sysusers.d line if true (malloced), may be NULL

     __C declaration:__ @sysuser@
     -}
  -> IO FC.CInt

{-|

  > rpmds

  Return current dependency color.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ current dependency color

__C declaration:__ @rpmdsColor@

__defined at:__ @rpm\/rpmds.h:315:13@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_b9409e41897ba84b" rpmdsColor ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO RPM.Types.Rpm_color_t

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
foreign import ccall unsafe "hs_bindgen_d5f45a8507377206" rpmdsSetColor ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> RPM.Types.Rpm_color_t
     {- ^

        [__@color@ /(input)/__]: new dependency color

     __C declaration:__ @color@
     -}
  -> IO RPM.Types.Rpm_color_t

{-|

  > rpmds

  Return next dependency set iterator index.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ dependency set iterator index, -1 on termination

__C declaration:__ @rpmdsNext@

__defined at:__ @rpm\/rpmds.h:330:5@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_43a0184dcb9cc67a" rpmdsNext ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO FC.CInt

{-|

  > rpmds

  Initialize dependency set iterator.

  [__@ds@ /(input)/__]: dependency set

  __returns:__ dependency set

__C declaration:__ @rpmdsInit@

__defined at:__ @rpm\/rpmds.h:337:7@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_6fb494a0176d7b20" rpmdsInit ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set

     __C declaration:__ @ds@
     -}
  -> IO RPM.Types.Rpmds

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
foreign import ccall unsafe "hs_bindgen_426ced0e04a70163" rpmdsFind ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set to search

     __C declaration:__ @ds@
     -}
  -> RPM.Types.Rpmds
     {- ^

        [__@ods@ /(input)/__]: dependency set element to find.

     __C declaration:__ @ods@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_e277ecc0f34b86d5" rpmdsMerge ::
     Ptr.Ptr RPM.Types.Rpmds
     {- ^ __C declaration:__ @dsp@
     -}
  -> RPM.Types.Rpmds
     {- ^

        [__@ods@ /(input)/__]: dependency set to merge

     __C declaration:__ @ods@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_d532422fa8af57af" rpmdsSearch ::
     RPM.Types.Rpmds
     {- ^

        [__@ds@ /(input)/__]: dependency set to search

     __C declaration:__ @ds@
     -}
  -> RPM.Types.Rpmds
     {- ^

        [__@ods@ /(input)/__]: dependency set element to find.

     __C declaration:__ @ods@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_b93fec67049f804b" rpmdsCompare ::
     RPM.Types.Rpmds
     {- ^ __C declaration:__ @a@
     -}
  -> RPM.Types.Rpmds
     {- ^ __C declaration:__ @b@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_30afc8c826a91d51" rpmdsAnyMatchesDep ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Types.Rpmds
     {- ^

        [__@req@ /(input)/__]: dependency set

     __C declaration:__ @req@
     -}
  -> FC.CInt
     {- ^

        [__@nopromote@ /(input)/__]: unused

     __C declaration:__ @nopromote@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_4ea1c37d20673aa3" rpmdsMatchesDep ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> FC.CInt
     {- ^

        [__@ix@ /(input)/__]: index in header provides

     __C declaration:__ @ix@
     -}
  -> RPM.Types.Rpmds
     {- ^

        [__@req@ /(input)/__]: dependency set

     __C declaration:__ @req@
     -}
  -> FC.CInt
     {- ^

        [__@nopromote@ /(input)/__]: unused

     __C declaration:__ @nopromote@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_6000edc54a11d65e" rpmdsNVRMatchesDep ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Types.Rpmds
     {- ^

        [__@req@ /(input)/__]: dependency set

     __C declaration:__ @req@
     -}
  -> FC.CInt
     {- ^

        [__@nopromote@ /(input)/__]: unused

     __C declaration:__ @nopromote@
     -}
  -> IO FC.CInt

{-| Load rpmlib provides into a dependency set.

  [__@*dsp@ /(output)/__]: (loaded) dependency set

  [__@tblp@ /(input)/__]: rpmlib provides table (NULL uses internal table)

  __returns:__ 0 on success

__C declaration:__ @rpmdsRpmlib@

__defined at:__ @rpm\/rpmds.h:407:5@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_0f20b6fb9e6691af" rpmdsRpmlib ::
     Ptr.Ptr RPM.Types.Rpmds
     {- ^ __C declaration:__ @dsp@
     -}
  -> Ptr.Ptr Void
     {- ^

        [__@tblp@ /(input)/__]: rpmlib provides table (NULL uses internal table)

     __C declaration:__ @tblp@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_d3e73232f24348de" rpmdsNewPool ::
     RPM.Types.RpmstrPool
     {- ^

        [__@pool@ /(input)/__]: shared string pool (or NULL for private pool)

     __C declaration:__ @pool@
     -}
  -> RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Types.RpmTagVal
     {- ^

        [__@tagN@ /(input)/__]: type of dependency

     __C declaration:__ @tagN@
     -}
  -> FC.CInt
     {- ^

        [__@flags@ /(input)/__]: unused

     __C declaration:__ @flags@
     -}
  -> IO RPM.Types.Rpmds

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
foreign import ccall unsafe "hs_bindgen_1b214d6d8d8f398b" rpmdsThisPool ::
     RPM.Types.RpmstrPool
     {- ^

        [__@pool@ /(input)/__]: string pool (or NULL for private pool)

     __C declaration:__ @pool@
     -}
  -> RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Types.RpmTagVal
     {- ^

        [__@tagN@ /(input)/__]: type of dependency

     __C declaration:__ @tagN@
     -}
  -> RpmsenseFlags
     {- ^ __C declaration:__ @flags@
     -}
  -> IO RPM.Types.Rpmds

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
foreign import ccall unsafe "hs_bindgen_d4ed0ea9ebcfc454" rpmdsSinglePool ::
     RPM.Types.RpmstrPool
     {- ^

        [__@pool@ /(input)/__]: string pool (or NULL for private pool)

     __C declaration:__ @pool@
     -}
  -> RPM.Types.RpmTagVal
     {- ^

        [__@tagN@ /(input)/__]: type of dependency

     __C declaration:__ @tagN@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @n@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @eVR@
     -}
  -> RpmsenseFlags
     {- ^ __C declaration:__ @flags@
     -}
  -> IO RPM.Types.Rpmds

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
foreign import ccall unsafe "hs_bindgen_c973e5fdbb14c650" rpmdsSinglePoolTix ::
     RPM.Types.RpmstrPool
     {- ^

        [__@pool@ /(input)/__]: string pool (or NULL for private pool)

     __C declaration:__ @pool@
     -}
  -> RPM.Types.RpmTagVal
     {- ^

        [__@tagN@ /(input)/__]: type of dependency

     __C declaration:__ @tagN@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @n@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @eVR@
     -}
  -> RpmsenseFlags
     {- ^ __C declaration:__ @flags@
     -}
  -> FC.CInt
     {- ^

        [__@triggerIndex@ /(input)/__]: trigger index

     __C declaration:__ @triggerIndex@
     -}
  -> IO RPM.Types.Rpmds

{-| Load rpmlib provides into a dependency set.

  [__@pool@ /(input)/__]: shared string pool (or NULL for private pool)

  [__@*dsp@ /(output)/__]: (loaded) dependency set

  [__@tblp@ /(input)/__]: rpmlib provides table (NULL uses internal table)

  __returns:__ 0 on success

__C declaration:__ @rpmdsRpmlibPool@

__defined at:__ @rpm\/rpmds.h:463:5@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_85a8400759ff705b" rpmdsRpmlibPool ::
     RPM.Types.RpmstrPool
     {- ^

        [__@pool@ /(input)/__]: shared string pool (or NULL for private pool)

     __C declaration:__ @pool@
     -}
  -> Ptr.Ptr RPM.Types.Rpmds
     {- ^ __C declaration:__ @dsp@
     -}
  -> Ptr.Ptr Void
     {- ^

        [__@tblp@ /(input)/__]: rpmlib provides table (NULL uses internal table)

     __C declaration:__ @tblp@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_a10227a2effa0f3e" rpmrichParse ::
     Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^

        [__@dstrp@ /(input)/__]: pointer to sting, will be updated

     __C declaration:__ @dstrp@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^

        [__@emsg@ /(input)/__]: returns the error string, can be NULL

     __C declaration:__ @emsg@
     -}
  -> RpmrichParseFunction
     {- ^

        [__@cb@ /(input)/__]: callback function

     __C declaration:__ @cb@
     -}
  -> Ptr.Ptr Void
     {- ^

        [__@cbdata@ /(input)/__]: callback function data

     __C declaration:__ @cbdata@
     -}
  -> IO RpmRC

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
foreign import ccall unsafe "hs_bindgen_c06eace8272e2e00" rpmrichParseForTag ::
     Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^

        [__@dstrp@ /(input)/__]: pointer to sting, will be updated

     __C declaration:__ @dstrp@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^

        [__@emsg@ /(input)/__]: returns the error string, can be NULL

     __C declaration:__ @emsg@
     -}
  -> RpmrichParseFunction
     {- ^

        [__@cb@ /(input)/__]: callback function

     __C declaration:__ @cb@
     -}
  -> Ptr.Ptr Void
     {- ^

        [__@cbdata@ /(input)/__]: callback function data

     __C declaration:__ @cbdata@
     -}
  -> RPM.Types.RpmTagVal
     {- ^

        [__@tagN@ /(input)/__]: type of dependency

     __C declaration:__ @tagN@
     -}
  -> IO RpmRC

{-| Return if current depenency is rich

  [__@dep@ /(input)/__]: the dependency

  __returns:__ 1 is dependency is a rich 0 otherwise

__C declaration:__ @rpmdsIsRich@

__defined at:__ @rpm\/rpmds.h:516:5@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_29b3e3541fc0ddca" rpmdsIsRich ::
     RPM.Types.Rpmds
     {- ^

        [__@dep@ /(input)/__]: the dependency

     __C declaration:__ @dep@
     -}
  -> IO FC.CInt

{-| Return a string representation of the rich dependency op

  [__@op@ /(input)/__]: the dependency op

  __returns:__ constant string, do not free

__C declaration:__ @rpmrichOpStr@

__defined at:__ @rpm\/rpmds.h:523:13@

__exported by:__ @rpm\/rpmds.h@
-}
foreign import ccall unsafe "hs_bindgen_94e5d78fe9b6af2a" rpmrichOpStr ::
     RpmrichOp
     {- ^

        [__@op@ /(input)/__]: the dependency op

     __C declaration:__ @op@
     -}
  -> IO (Ptr.Ptr FC.CChar)

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
foreign import ccall unsafe "hs_bindgen_505626fa82b309b2" rpmdsParseRichDep ::
     RPM.Types.Rpmds
     {- ^

        [__@dep@ /(input)/__]: the dependency

     __C declaration:__ @dep@
     -}
  -> Ptr.Ptr RPM.Types.Rpmds
     {- ^

        [__@leftds@ /(input)/__]: returns the left dependency

     __C declaration:__ @leftds@
     -}
  -> Ptr.Ptr RPM.Types.Rpmds
     {- ^

        [__@rightds@ /(input)/__]: returns the right dependency

     __C declaration:__ @rightds@
     -}
  -> Ptr.Ptr RpmrichOp
     {- ^

        [__@op@ /(input)/__]: returns the rich dep op

     __C declaration:__ @op@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^

        [__@emsg@ /(input)/__]: returns the error string

     __C declaration:__ @emsg@
     -}
  -> IO RpmRC
