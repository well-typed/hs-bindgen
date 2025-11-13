{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Tag.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Types
import Prelude (IO)
import RPM.Tag

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmtag.h>"
  , "/* get_rpmTagGetName_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_427ea3bd12b1f589 (void)) ("
  , "  rpmTagVal arg1"
  , ")"
  , "{"
  , "  return &rpmTagGetName;"
  , "}"
  , "/* get_rpmTagGetType_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagType (*hs_bindgen_3712dc0a06c70f94 (void)) ("
  , "  rpmTagVal arg1"
  , ")"
  , "{"
  , "  return &rpmTagGetType;"
  , "}"
  , "/* get_rpmTagGetTagType_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagType (*hs_bindgen_3e4bf50b8eb87005 (void)) ("
  , "  rpmTagVal arg1"
  , ")"
  , "{"
  , "  return &rpmTagGetTagType;"
  , "}"
  , "/* get_rpmTagGetReturnType_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagReturnType (*hs_bindgen_c56aa5d453187e95 (void)) ("
  , "  rpmTagVal arg1"
  , ")"
  , "{"
  , "  return &rpmTagGetReturnType;"
  , "}"
  , "/* get_rpmTagGetClass_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagClass (*hs_bindgen_1b901c20a7b419ab (void)) ("
  , "  rpmTagVal arg1"
  , ")"
  , "{"
  , "  return &rpmTagGetClass;"
  , "}"
  , "/* get_rpmTagGetValue_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagVal (*hs_bindgen_0a0c24e9717e9a20 (void)) ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return &rpmTagGetValue;"
  , "}"
  , "/* get_rpmTagTypeGetClass_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagClass (*hs_bindgen_c4fe26e67cec3596 (void)) ("
  , "  rpmTagType arg1"
  , ")"
  , "{"
  , "  return &rpmTagTypeGetClass;"
  , "}"
  , "/* get_rpmTagGetNames_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_d6168d98f42899a9 (void)) ("
  , "  rpmtd arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &rpmTagGetNames;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_427ea3bd12b1f589" hs_bindgen_427ea3bd12b1f589 ::
     IO (Ptr.FunPtr (RPM.Types.RpmTagVal -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmTagGetName_ptr #-}

{-|

  > rpmtag

  Return tag name from value.

  [__@tag@ /(input)/__]: tag value

  __returns:__ tag name, "(unknown)" on not found

__C declaration:__ @rpmTagGetName@

__defined at:__ @rpm\/rpmtag.h:520:14@

__exported by:__ @rpm\/rpmtag.h@
-}
rpmTagGetName_ptr :: Ptr.FunPtr (RPM.Types.RpmTagVal -> IO (Ptr.Ptr FC.CChar))
rpmTagGetName_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_427ea3bd12b1f589

foreign import ccall unsafe "hs_bindgen_3712dc0a06c70f94" hs_bindgen_3712dc0a06c70f94 ::
     IO (Ptr.FunPtr (RPM.Types.RpmTagVal -> IO RpmTagType))

{-# NOINLINE rpmTagGetType_ptr #-}

{-|

  > rpmtag

  Return tag data type from value.

  [__@tag@ /(input)/__]: tag value

  __returns:__ tag data type + return type, RPM_NULL_TYPE on not found.

__C declaration:__ @rpmTagGetType@

__defined at:__ @rpm\/rpmtag.h:527:12@

__exported by:__ @rpm\/rpmtag.h@
-}
rpmTagGetType_ptr :: Ptr.FunPtr (RPM.Types.RpmTagVal -> IO RpmTagType)
rpmTagGetType_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3712dc0a06c70f94

foreign import ccall unsafe "hs_bindgen_3e4bf50b8eb87005" hs_bindgen_3e4bf50b8eb87005 ::
     IO (Ptr.FunPtr (RPM.Types.RpmTagVal -> IO RpmTagType))

{-# NOINLINE rpmTagGetTagType_ptr #-}

{-|

  > rpmtag

  Return tag data type from value.

  [__@tag@ /(input)/__]: tag value

  __returns:__ tag data type, RPM_NULL_TYPE on not found.

__C declaration:__ @rpmTagGetTagType@

__defined at:__ @rpm\/rpmtag.h:534:12@

__exported by:__ @rpm\/rpmtag.h@
-}
rpmTagGetTagType_ptr :: Ptr.FunPtr (RPM.Types.RpmTagVal -> IO RpmTagType)
rpmTagGetTagType_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3e4bf50b8eb87005

foreign import ccall unsafe "hs_bindgen_c56aa5d453187e95" hs_bindgen_c56aa5d453187e95 ::
     IO (Ptr.FunPtr (RPM.Types.RpmTagVal -> IO RpmTagReturnType))

{-# NOINLINE rpmTagGetReturnType_ptr #-}

{-|

  > rpmtag

  Return tag data type from value.

  [__@tag@ /(input)/__]: tag value

  __returns:__ tag data return type, RPM_NULL_TYPE on not found.

__C declaration:__ @rpmTagGetReturnType@

__defined at:__ @rpm\/rpmtag.h:541:18@

__exported by:__ @rpm\/rpmtag.h@
-}
rpmTagGetReturnType_ptr :: Ptr.FunPtr (RPM.Types.RpmTagVal -> IO RpmTagReturnType)
rpmTagGetReturnType_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c56aa5d453187e95

foreign import ccall unsafe "hs_bindgen_1b901c20a7b419ab" hs_bindgen_1b901c20a7b419ab ::
     IO (Ptr.FunPtr (RPM.Types.RpmTagVal -> IO RpmTagClass))

{-# NOINLINE rpmTagGetClass_ptr #-}

{-|

  > rpmtag

  Return tag data class from value.

  [__@tag@ /(input)/__]: tag value

  __returns:__ tag data class, RPM_NULL_CLASS on not found.

__C declaration:__ @rpmTagGetClass@

__defined at:__ @rpm\/rpmtag.h:548:13@

__exported by:__ @rpm\/rpmtag.h@
-}
rpmTagGetClass_ptr :: Ptr.FunPtr (RPM.Types.RpmTagVal -> IO RpmTagClass)
rpmTagGetClass_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1b901c20a7b419ab

foreign import ccall unsafe "hs_bindgen_0a0c24e9717e9a20" hs_bindgen_0a0c24e9717e9a20 ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO RPM.Types.RpmTagVal))

{-# NOINLINE rpmTagGetValue_ptr #-}

{-|

  > rpmtag

  Return tag value from name.

  [__@tagstr@ /(input)/__]: name of tag

  __returns:__ tag value, RPMTAG_NOT_FOUND on not found

__C declaration:__ @rpmTagGetValue@

__defined at:__ @rpm\/rpmtag.h:555:11@

__exported by:__ @rpm\/rpmtag.h@
-}
rpmTagGetValue_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO RPM.Types.RpmTagVal)
rpmTagGetValue_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0a0c24e9717e9a20

foreign import ccall unsafe "hs_bindgen_c4fe26e67cec3596" hs_bindgen_c4fe26e67cec3596 ::
     IO (Ptr.FunPtr (RpmTagType -> IO RpmTagClass))

{-# NOINLINE rpmTagTypeGetClass_ptr #-}

{-|

  > rpmtag

  Return data class of type

  [__@type@ /(input)/__]: tag type

  __returns:__ data class, RPM_NULL_CLASS on unknown.

__C declaration:__ @rpmTagTypeGetClass@

__defined at:__ @rpm\/rpmtag.h:562:13@

__exported by:__ @rpm\/rpmtag.h@
-}
rpmTagTypeGetClass_ptr :: Ptr.FunPtr (RpmTagType -> IO RpmTagClass)
rpmTagTypeGetClass_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c4fe26e67cec3596

foreign import ccall unsafe "hs_bindgen_d6168d98f42899a9" hs_bindgen_d6168d98f42899a9 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmtd -> FC.CInt -> IO FC.CInt))

{-# NOINLINE rpmTagGetNames_ptr #-}

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
rpmTagGetNames_ptr :: Ptr.FunPtr (RPM.Types.Rpmtd -> FC.CInt -> IO FC.CInt)
rpmTagGetNames_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d6168d98f42899a9
