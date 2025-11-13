{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Td.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Argv
import qualified RPM.Tag
import Prelude (IO)
import RPM.Td

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmtd.h>"
  , "rpmtd hs_bindgen_7508fe592d6f40f8 (void)"
  , "{"
  , "  return rpmtdNew();"
  , "}"
  , "rpmtd hs_bindgen_44c424d3add09f16 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdFree(arg1);"
  , "}"
  , "void hs_bindgen_63aeb93054b99505 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  rpmtdReset(arg1);"
  , "}"
  , "void hs_bindgen_05e2ab7a04f60682 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  rpmtdFreeData(arg1);"
  , "}"
  , "rpm_count_t hs_bindgen_a466a7e3273af063 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdCount(arg1);"
  , "}"
  , "rpm_count_t hs_bindgen_75aa6cd3ebb7153d ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdSize(arg1);"
  , "}"
  , "rpmTagVal hs_bindgen_c50b6a26409a57c6 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdTag(arg1);"
  , "}"
  , "rpmTagType hs_bindgen_983c815d3e66c8d2 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdType(arg1);"
  , "}"
  , "rpmTagClass hs_bindgen_28d7a766e6b4d987 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdClass(arg1);"
  , "}"
  , "rpmtdFlags hs_bindgen_b80791366b3b1e75 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdGetFlags(arg1);"
  , "}"
  , "signed int hs_bindgen_f5f59dff0d330c39 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdGetIndex(arg1);"
  , "}"
  , "signed int hs_bindgen_3e5f26e20edfbf08 ("
  , "  rpmtd arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return rpmtdSetIndex(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_c178542dc89eb008 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdInit(arg1);"
  , "}"
  , "signed int hs_bindgen_1d43a0c8377400ed ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdNext(arg1);"
  , "}"
  , "uint32_t *hs_bindgen_fb304ce886143e7e ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdNextUint32(arg1);"
  , "}"
  , "uint64_t *hs_bindgen_6703d569d4712c3f ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdNextUint64(arg1);"
  , "}"
  , "char const *hs_bindgen_41ec3d23ae405659 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdNextString(arg1);"
  , "}"
  , "char *hs_bindgen_69c5f09d88b32101 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdGetChar(arg1);"
  , "}"
  , "uint16_t *hs_bindgen_16bd6941bc90310e ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdGetUint16(arg1);"
  , "}"
  , "uint32_t *hs_bindgen_d6b112b8b5f10669 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdGetUint32(arg1);"
  , "}"
  , "uint64_t *hs_bindgen_b1bb383b4e856343 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdGetUint64(arg1);"
  , "}"
  , "char const *hs_bindgen_32cc8a89051ed474 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdGetString(arg1);"
  , "}"
  , "uint64_t hs_bindgen_1acef78c5f705c46 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdGetNumber(arg1);"
  , "}"
  , "char *hs_bindgen_35ec06edcc1f3bd7 ("
  , "  rpmtd arg1,"
  , "  rpmtdFormats arg2,"
  , "  char const *arg3"
  , ")"
  , "{"
  , "  return rpmtdFormat(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_009937c1a5be0386 ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return rpmtdSetTag(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_29f8c8111842c209 ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  uint8_t *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return rpmtdFromUint8(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_34e1d0051198a154 ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  uint16_t *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return rpmtdFromUint16(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_f9da49cadd41811a ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  uint32_t *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return rpmtdFromUint32(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_34ac9dcd98ab79be ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  uint64_t *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return rpmtdFromUint64(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_8cd6df9217c4aff2 ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  char const *arg3"
  , ")"
  , "{"
  , "  return rpmtdFromString(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_d71f22c4f18b6da4 ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  char const **arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return rpmtdFromStringArray(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_2a6d129e76d23672 ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  ARGV_t arg3"
  , ")"
  , "{"
  , "  return rpmtdFromArgv(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_18aebbb7ec53a7a5 ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  ARGI_t arg3"
  , ")"
  , "{"
  , "  return rpmtdFromArgi(arg1, arg2, arg3);"
  , "}"
  , "rpmtd hs_bindgen_4b45796617c933e2 ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return rpmtdDup(arg1);"
  , "}"
  , "rpmsid *hs_bindgen_0f8cfb6720ed0a06 ("
  , "  rpmtd arg1,"
  , "  rpmstrPool arg2"
  , ")"
  , "{"
  , "  return rpmtdToPool(arg1, arg2);"
  , "}"
  ]))

{-|

  > rpmtd

  Create new tag data container

  __returns:__ New, initialized tag data container.

__C declaration:__ @rpmtdNew@

__defined at:__ @rpm\/rpmtd.h:49:7@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_7508fe592d6f40f8" rpmtdNew ::
     IO Rpmtd

{-|

  > rpmtd

  Destroy tag data container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ NULL always

__C declaration:__ @rpmtdFree@

__defined at:__ @rpm\/rpmtd.h:56:7@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_44c424d3add09f16" rpmtdFree ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO Rpmtd

{-|

  > rpmtd

  (Re-)initialize tag data container. Contents will be zeroed out and iteration index reset.

  [__@td@ /(input)/__]: Tag data container

__C declaration:__ @rpmtdReset@

__defined at:__ @rpm\/rpmtd.h:63:6@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_63aeb93054b99505" rpmtdReset ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO ()

{-|

  > rpmtd

  Free contained data. This is always safe to call as the container knows if data was malloc'ed or not. Container is reinitialized.

  [__@td@ /(input)/__]: Tag data container

__C declaration:__ @rpmtdFreeData@

__defined at:__ @rpm\/rpmtd.h:70:6@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_05e2ab7a04f60682" rpmtdFreeData ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO ()

{-|

  > rpmtd

  Retrieve array size of the container. For non-array types this is always 1.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Number of entries in contained data.

__C declaration:__ @rpmtdCount@

__defined at:__ @rpm\/rpmtd.h:77:13@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_a466a7e3273af063" rpmtdCount ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO Rpm_count_t

{-|

  > rpmtd

  Retrieve container data size (eg required for allocation). Note this currently only works for RPMTD_IMMUTABLE data.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Data size in bytes.

__C declaration:__ @rpmtdSize@

__defined at:__ @rpm\/rpmtd.h:85:13@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_75aa6cd3ebb7153d" rpmtdSize ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO Rpm_count_t

{-|

  > rpmtd

  Retrieve tag of the container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Rpm tag.

__C declaration:__ @rpmtdTag@

__defined at:__ @rpm\/rpmtd.h:92:11@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_c50b6a26409a57c6" rpmtdTag ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO RpmTagVal

{-|

  > rpmtd

  Retrieve type of the container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Rpm tag type.

__C declaration:__ @rpmtdType@

__defined at:__ @rpm\/rpmtd.h:99:12@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_983c815d3e66c8d2" rpmtdType ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO RPM.Tag.RpmTagType

{-|

  > rpmtd

  Retrieve class of the container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Rpm tag class

__C declaration:__ @rpmtdClass@

__defined at:__ @rpm\/rpmtd.h:106:13@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_28d7a766e6b4d987" rpmtdClass ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO RpmTagClass

{-|

  > rpmtd

  Retrieve flags of the container (allocation details etc)

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Container flags

__C declaration:__ @rpmtdGetFlags@

__defined at:__ @rpm\/rpmtd.h:113:12@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_b80791366b3b1e75" rpmtdGetFlags ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO RpmtdFlags

{-|

  > rpmtd

  Retrieve current iteration index of the container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Iteration index (or -1 if not iterating)

__C declaration:__ @rpmtdGetIndex@

__defined at:__ @rpm\/rpmtd.h:120:5@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_f5f59dff0d330c39" rpmtdGetIndex ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO FC.CInt

{-|

  > rpmtd

  Set iteration index of the container. If new index is out of bounds for the container, -1 is returned and iteration index is left untouched.

  [__@td@ /(input)/__]: Tag data container

  [__@index@ /(input)/__]: New index

  __returns:__ New index, or -1 if index out of bounds

__C declaration:__ @rpmtdSetIndex@

__defined at:__ @rpm\/rpmtd.h:130:5@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_3e5f26e20edfbf08" rpmtdSetIndex ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> FC.CInt
     {- ^

        [__@index@ /(input)/__]: New index

     __C declaration:__ @index@
     -}
  -> IO FC.CInt

{-|

  > rpmtd

  Initialize tag container for iteration

  [__@td@ /(input)/__]: Tag data container

  __returns:__ 0 on success

__C declaration:__ @rpmtdInit@

__defined at:__ @rpm\/rpmtd.h:137:5@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_c178542dc89eb008" rpmtdInit ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO FC.CInt

{-|

  > rpmtd

  Iterate over tag data container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Tag data container iterator index, -1 on termination

__C declaration:__ @rpmtdNext@

__defined at:__ @rpm\/rpmtd.h:144:5@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_1d43a0c8377400ed" rpmtdNext ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO FC.CInt

{-|

  > rpmtd

  Iterate over uint32_t type tag data container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to next value, NULL on termination or error

__C declaration:__ @rpmtdNextUint32@

__defined at:__ @rpm\/rpmtd.h:151:11@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_fb304ce886143e7e" rpmtdNextUint32 ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32)

{-|

  > rpmtd

  Iterate over uint64_t type tag data container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to next value, NULL on termination or error

__C declaration:__ @rpmtdNextUint64@

__defined at:__ @rpm\/rpmtd.h:158:11@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_6703d569d4712c3f" rpmtdNextUint64 ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word64)

{-|

  > rpmtd

  Iterate over string / string array type tag data container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to next value, NULL on termination or error

__C declaration:__ @rpmtdNextString@

__defined at:__ @rpm\/rpmtd.h:165:13@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_41ec3d23ae405659" rpmtdNextString ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmtd

  Return char data from tag container. For scalar return type, just return pointer to the integer. On array types, return pointer to current iteration index. If the tag container is not for char type, NULL is returned.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to uint16_t, NULL on error

__C declaration:__ @rpmtdGetChar@

__defined at:__ @rpm\/rpmtd.h:175:7@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_69c5f09d88b32101" rpmtdGetChar ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmtd

  Return uint16_t data from tag container. For scalar return type, just return pointer to the integer. On array types, return pointer to current iteration index. If the tag container is not for int16 type, NULL is returned.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to uint16_t, NULL on error

__C declaration:__ @rpmtdGetUint16@

__defined at:__ @rpm\/rpmtd.h:185:12@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_16bd6941bc90310e" rpmtdGetUint16 ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word16)

{-|

  > rpmtd

  Return uint32_t data from tag container. For scalar return type, just return pointer to the integer. On array types, return pointer to current iteration index. If the tag container is not for int32 type, NULL is returned.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to uint32_t, NULL on error

__C declaration:__ @rpmtdGetUint32@

__defined at:__ @rpm\/rpmtd.h:195:12@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_d6b112b8b5f10669" rpmtdGetUint32 ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32)

{-|

  > rpmtd

  Return uint64_t data from tag container. For scalar return type, just return pointer to the integer. On array types, return pointer to current iteration index. If the tag container is not for int64 type, NULL is returned.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to uint64_t, NULL on error

__C declaration:__ @rpmtdGetUint64@

__defined at:__ @rpm\/rpmtd.h:205:12@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_b1bb383b4e856343" rpmtdGetUint64 ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word64)

{-|

  > rpmtd

  Return string data from tag container. For string types, just return the string. On string array types, return the string from current iteration index. If the tag container is not for a string type, NULL is returned.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ String constant from container, NULL on error

__C declaration:__ @rpmtdGetString@

__defined at:__ @rpm\/rpmtd.h:215:14@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_32cc8a89051ed474" rpmtdGetString ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmtd

  Return numeric value from tag container. Returns the value of numeric container (RPM_NUMERIC_CLASS) from current iteration index as uint64_t regardless of its internal presentation (8/16/32/64-bit integer).

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Value of current iteration item as uint64_t, 0 for non-numeric types (error)

__C declaration:__ @rpmtdGetNumber@

__defined at:__ @rpm\/rpmtd.h:226:10@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_1acef78c5f705c46" rpmtdGetNumber ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> IO HsBindgen.Runtime.Prelude.Word64

{-|

  > rpmtd

  Format data from tag container to string presentation of given format. Return malloced string presentation of current data in container, converting from integers etc as necessary. On array types, data from current iteration index is used for formatting.

  [__@td@ /(input)/__]: Tag data container

  [__@fmt@ /(input)/__]: Format to apply

  [__@errmsg@ /(input)/__]: Error message from conversion (or NULL)

  __returns:__ String representation of current data (malloc'ed), NULL on error

__C declaration:__ @rpmtdFormat@

__defined at:__ @rpm\/rpmtd.h:271:7@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_35ec06edcc1f3bd7" rpmtdFormat ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> RpmtdFormats
     {- ^

        [__@fmt@ /(input)/__]: Format to apply

     __C declaration:__ @fmt@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@errmsg@ /(input)/__]: Error message from conversion (or NULL)

     __C declaration:__ @errmsg@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmtd

  Set container tag and type. For empty container, any valid tag can be set. If the container has data, changing is only permitted to tag of same type.

  [__@td@ /(input)/__]: Tag data container

  [__@tag@ /(input)/__]: New tag

  __returns:__ 1 on success, 0 on error

__C declaration:__ @rpmtdSetTag@

__defined at:__ @rpm\/rpmtd.h:281:5@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_009937c1a5be0386" rpmtdSetTag ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: New tag

     __C declaration:__ @tag@
     -}
  -> IO FC.CInt

{-|

  > rpmtd

  Construct tag container from uint8_t pointer. Tag type is checked to be of compatible type (CHAR, INT8 or BIN). For non-array types (BIN is a special case of INT8 array) count must be exactly 1.

  [__@td@ /(input)/__]: Tag data container

  [__@tag@ /(input)/__]: Rpm tag to construct

  [__@data@ /(input)/__]: Pointer to uint8_t (value or array)

  [__@count@ /(input)/__]: Number of entries

  __returns:__ 1 on success, 0 on error (eg wrong type)

__C declaration:__ @rpmtdFromUint8@

__defined at:__ @rpm\/rpmtd.h:294:5@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_29f8c8111842c209" rpmtdFromUint8 ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: Rpm tag to construct

     __C declaration:__ @tag@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word8
     {- ^ __C declaration:__ @data'@
     -}
  -> Rpm_count_t
     {- ^

        [__@count@ /(input)/__]: Number of entries

     __C declaration:__ @count@
     -}
  -> IO FC.CInt

{-|

  > rpmtd

  Construct tag container from uint16_t pointer. Tag type is checked to be of INT16 type. For non-array types count must be exactly 1.

  [__@td@ /(input)/__]: Tag data container

  [__@tag@ /(input)/__]: Rpm tag to construct

  [__@data@ /(input)/__]: Pointer to uint16_t (value or array)

  [__@count@ /(input)/__]: Number of entries

  __returns:__ 1 on success, 0 on error (eg wrong type)

__C declaration:__ @rpmtdFromUint16@

__defined at:__ @rpm\/rpmtd.h:306:5@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_34e1d0051198a154" rpmtdFromUint16 ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: Rpm tag to construct

     __C declaration:__ @tag@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word16
     {- ^ __C declaration:__ @data'@
     -}
  -> Rpm_count_t
     {- ^

        [__@count@ /(input)/__]: Number of entries

     __C declaration:__ @count@
     -}
  -> IO FC.CInt

{-|

  > rpmtd

  Construct tag container from uint32_t pointer. Tag type is checked to be of INT32 type. For non-array types count must be exactly 1.

  [__@td@ /(input)/__]: Tag data container

  [__@tag@ /(input)/__]: Rpm tag to construct

  [__@data@ /(input)/__]: Pointer to uint32_t (value or array)

  [__@count@ /(input)/__]: Number of entries

  __returns:__ 1 on success, 0 on error (eg wrong type)

__C declaration:__ @rpmtdFromUint32@

__defined at:__ @rpm\/rpmtd.h:318:5@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_f9da49cadd41811a" rpmtdFromUint32 ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: Rpm tag to construct

     __C declaration:__ @tag@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word32
     {- ^ __C declaration:__ @data'@
     -}
  -> Rpm_count_t
     {- ^

        [__@count@ /(input)/__]: Number of entries

     __C declaration:__ @count@
     -}
  -> IO FC.CInt

{-|

  > rpmtd

  Construct tag container from uint64_t pointer. Tag type is checked to be of INT64 type. For non-array types count must be exactly 1.

  [__@td@ /(input)/__]: Tag data container

  [__@tag@ /(input)/__]: Rpm tag to construct

  [__@data@ /(input)/__]: Pointer to uint64_t (value or array)

  [__@count@ /(input)/__]: Number of entries

  __returns:__ 1 on success, 0 on error (eg wrong type)

__C declaration:__ @rpmtdFromUint64@

__defined at:__ @rpm\/rpmtd.h:330:5@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_34ac9dcd98ab79be" rpmtdFromUint64 ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: Rpm tag to construct

     __C declaration:__ @tag@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word64
     {- ^ __C declaration:__ @data'@
     -}
  -> Rpm_count_t
     {- ^

        [__@count@ /(input)/__]: Number of entries

     __C declaration:__ @count@
     -}
  -> IO FC.CInt

{-|

  > rpmtd

  Construct tag container from a string. Tag type is checked to be of string type.

  [__@td@ /(input)/__]: Tag data container

  [__@tag@ /(input)/__]: Rpm tag to construct

  [__@data@ /(input)/__]: String to use

  __returns:__ 1 on success, 0 on error (eg wrong type)

__C declaration:__ @rpmtdFromString@

__defined at:__ @rpm\/rpmtd.h:340:5@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_8cd6df9217c4aff2" rpmtdFromString ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: Rpm tag to construct

     __C declaration:__ @tag@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @data'@
     -}
  -> IO FC.CInt

{-|

  > rpmtd

  Construct tag container from a string array. Tag type is checked to be of string or string array type. For non-array types count must be exactly 1.

  [__@td@ /(input)/__]: Tag data container

  [__@tag@ /(input)/__]: Rpm tag to construct

  [__@data@ /(input)/__]: Pointer to string array

  [__@count@ /(input)/__]: Number of entries

  __returns:__ 1 on success, 0 on error (eg wrong type)

__C declaration:__ @rpmtdFromStringArray@

__defined at:__ @rpm\/rpmtd.h:352:5@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_d71f22c4f18b6da4" rpmtdFromStringArray ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: Rpm tag to construct

     __C declaration:__ @tag@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^ __C declaration:__ @data'@
     -}
  -> Rpm_count_t
     {- ^

        [__@count@ /(input)/__]: Number of entries

     __C declaration:__ @count@
     -}
  -> IO FC.CInt

{-|

  > rpmtd

  Construct tag container from ARGV_t array. Tag type is checked to be of string array type and array is checked to be non-empty.

  [__@td@ /(input)/__]: Tag data container

  [__@tag@ /(input)/__]: Rpm tag to construct

  [__@argv@ /(input)/__]: ARGV array

  __returns:__ 1 on success, 0 on error (eg wrong type)

__C declaration:__ @rpmtdFromArgv@

__defined at:__ @rpm\/rpmtd.h:363:5@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_2a6d129e76d23672" rpmtdFromArgv ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: Rpm tag to construct

     __C declaration:__ @tag@
     -}
  -> RPM.Argv.ARGV_t
     {- ^

        [__@argv@ /(input)/__]: ARGV array

     __C declaration:__ @argv@
     -}
  -> IO FC.CInt

{-|

  > rpmtd

  Construct tag container from ARGI_t array. Tag type is checked to be of integer array type and array is checked to be non-empty.

  [__@td@ /(input)/__]: Tag data container

  [__@tag@ /(input)/__]: Rpm tag to construct

  [__@argi@ /(input)/__]: ARGI array

  __returns:__ 1 on success, 0 on error (eg wrong type)

__C declaration:__ @rpmtdFromArgi@

__defined at:__ @rpm\/rpmtd.h:374:5@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_18aebbb7ec53a7a5" rpmtdFromArgi ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: Rpm tag to construct

     __C declaration:__ @tag@
     -}
  -> RPM.Argv.ARGI_t
     {- ^

        [__@argi@ /(input)/__]: ARGI array

     __C declaration:__ @argi@
     -}
  -> IO FC.CInt

{-|

  > rpmtd

  Perform deep copy of container. Create a modifiable copy of tag data container (on string arrays each string is separately allocated)

  __TODO:__

  Only string arrays types are supported currently

  [__@td@ /(input)/__]: Container to copy

  __returns:__ New container or NULL on error

__C declaration:__ @rpmtdDup@

__defined at:__ @rpm\/rpmtd.h:384:7@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_4b45796617c933e2" rpmtdDup ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Container to copy

     __C declaration:__ @td@
     -}
  -> IO Rpmtd

{-|

  > rpmtd

  Push string array container contents to a string pool, return string ids.

  [__@td@ /(input)/__]: Tag data container

  [__@pool@ /(input)/__]: String pool

  __returns:__ Array of string id's (malloced)

__C declaration:__ @rpmtdToPool@

__defined at:__ @rpm\/rpmtd.h:392:10@

__exported by:__ @rpm\/rpmtd.h@
-}
foreign import ccall unsafe "hs_bindgen_0f8cfb6720ed0a06" rpmtdToPool ::
     Rpmtd
     {- ^

        [__@td@ /(input)/__]: Tag data container

     __C declaration:__ @td@
     -}
  -> RpmstrPool
     {- ^

        [__@pool@ /(input)/__]: String pool

     __C declaration:__ @pool@
     -}
  -> IO (Ptr.Ptr Rpmsid)
