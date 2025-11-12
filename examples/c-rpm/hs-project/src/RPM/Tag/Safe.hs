{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Tag.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Types
import Prelude (IO)
import RPM.Tag

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmtag.h>"
  , "char const *hs_bindgen_6005edcdcb094f62 ("
  , "  rpmTagVal arg1"
  , ")"
  , "{"
  , "  return rpmTagGetName(arg1);"
  , "}"
  , "rpmTagType hs_bindgen_654e3b68e6afa920 ("
  , "  rpmTagVal arg1"
  , ")"
  , "{"
  , "  return rpmTagGetType(arg1);"
  , "}"
  , "rpmTagType hs_bindgen_3f30ea54305075cf ("
  , "  rpmTagVal arg1"
  , ")"
  , "{"
  , "  return rpmTagGetTagType(arg1);"
  , "}"
  , "rpmTagReturnType hs_bindgen_0fe28ec2369c8238 ("
  , "  rpmTagVal arg1"
  , ")"
  , "{"
  , "  return rpmTagGetReturnType(arg1);"
  , "}"
  , "rpmTagClass hs_bindgen_a0328201bf3ce223 ("
  , "  rpmTagVal arg1"
  , ")"
  , "{"
  , "  return rpmTagGetClass(arg1);"
  , "}"
  , "rpmTagVal hs_bindgen_f189cc46226b7abf ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return rpmTagGetValue(arg1);"
  , "}"
  , "rpmTagClass hs_bindgen_7e840d9e4cc9365c ("
  , "  rpmTagType arg1"
  , ")"
  , "{"
  , "  return rpmTagTypeGetClass(arg1);"
  , "}"
  , "signed int hs_bindgen_2ae8a1c64fa883b1 ("
  , "  rpmtd arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return rpmTagGetNames(arg1, arg2);"
  , "}"
  ]))

{-|

  > rpmtag

  Return tag name from value.

  [__@tag@ /(input)/__]: tag value

  __returns:__ tag name, "(unknown)" on not found

__C declaration:__ @rpmTagGetName@

__defined at:__ @rpm\/rpmtag.h:520:14@

__exported by:__ @rpm\/rpmtag.h@
-}
foreign import ccall safe "hs_bindgen_6005edcdcb094f62" rpmTagGetName ::
     RPM.Types.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: tag value

     __C declaration:__ @tag@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmtag

  Return tag data type from value.

  [__@tag@ /(input)/__]: tag value

  __returns:__ tag data type + return type, RPM_NULL_TYPE on not found.

__C declaration:__ @rpmTagGetType@

__defined at:__ @rpm\/rpmtag.h:527:12@

__exported by:__ @rpm\/rpmtag.h@
-}
foreign import ccall safe "hs_bindgen_654e3b68e6afa920" rpmTagGetType ::
     RPM.Types.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: tag value

     __C declaration:__ @tag@
     -}
  -> IO RpmTagType

{-|

  > rpmtag

  Return tag data type from value.

  [__@tag@ /(input)/__]: tag value

  __returns:__ tag data type, RPM_NULL_TYPE on not found.

__C declaration:__ @rpmTagGetTagType@

__defined at:__ @rpm\/rpmtag.h:534:12@

__exported by:__ @rpm\/rpmtag.h@
-}
foreign import ccall safe "hs_bindgen_3f30ea54305075cf" rpmTagGetTagType ::
     RPM.Types.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: tag value

     __C declaration:__ @tag@
     -}
  -> IO RpmTagType

{-|

  > rpmtag

  Return tag data type from value.

  [__@tag@ /(input)/__]: tag value

  __returns:__ tag data return type, RPM_NULL_TYPE on not found.

__C declaration:__ @rpmTagGetReturnType@

__defined at:__ @rpm\/rpmtag.h:541:18@

__exported by:__ @rpm\/rpmtag.h@
-}
foreign import ccall safe "hs_bindgen_0fe28ec2369c8238" rpmTagGetReturnType ::
     RPM.Types.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: tag value

     __C declaration:__ @tag@
     -}
  -> IO RpmTagReturnType

{-|

  > rpmtag

  Return tag data class from value.

  [__@tag@ /(input)/__]: tag value

  __returns:__ tag data class, RPM_NULL_CLASS on not found.

__C declaration:__ @rpmTagGetClass@

__defined at:__ @rpm\/rpmtag.h:548:13@

__exported by:__ @rpm\/rpmtag.h@
-}
foreign import ccall safe "hs_bindgen_a0328201bf3ce223" rpmTagGetClass ::
     RPM.Types.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: tag value

     __C declaration:__ @tag@
     -}
  -> IO RpmTagClass

{-|

  > rpmtag

  Return tag value from name.

  [__@tagstr@ /(input)/__]: name of tag

  __returns:__ tag value, RPMTAG_NOT_FOUND on not found

__C declaration:__ @rpmTagGetValue@

__defined at:__ @rpm\/rpmtag.h:555:11@

__exported by:__ @rpm\/rpmtag.h@
-}
foreign import ccall safe "hs_bindgen_f189cc46226b7abf" rpmTagGetValue ::
     Ptr.Ptr FC.CChar
     {- ^

        [__@tagstr@ /(input)/__]: name of tag

     __C declaration:__ @tagstr@
     -}
  -> IO RPM.Types.RpmTagVal

{-|

  > rpmtag

  Return data class of type

  [__@type@ /(input)/__]: tag type

  __returns:__ data class, RPM_NULL_CLASS on unknown.

__C declaration:__ @rpmTagTypeGetClass@

__defined at:__ @rpm\/rpmtag.h:562:13@

__exported by:__ @rpm\/rpmtag.h@
-}
foreign import ccall safe "hs_bindgen_7e840d9e4cc9365c" rpmTagTypeGetClass ::
     RpmTagType
     {- ^ __C declaration:__ @type'@
     -}
  -> IO RpmTagClass

{-|

  > rpmtag

  Return known rpm tag names, sorted by name.

  [__@tagnames@ /(output)/__]: tag container of string array type

  [__@fullname@ /(input)/__]: return short or full name

  __returns:__ number of tag names, 0 on error

__C declaration:__ @rpmTagGetNames@

__defined at:__ @rpm\/rpmtag.h:570:5@

__exported by:__ @rpm\/rpmtag.h@
-}
foreign import ccall safe "hs_bindgen_2ae8a1c64fa883b1" rpmTagGetNames ::
     RPM.Types.Rpmtd
     {- ^

        [__@tagnames@ /(output)/__]: tag container of string array type

     __C declaration:__ @tagnames@
     -}
  -> FC.CInt
     {- ^

        [__@fullname@ /(input)/__]: return short or full name

     __C declaration:__ @fullname@
     -}
  -> IO FC.CInt
