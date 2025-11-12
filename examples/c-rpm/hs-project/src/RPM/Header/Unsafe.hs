{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Header.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Td
import qualified RPM.Types
import Data.Void (Void)
import Prelude (IO)
import RPM.Header

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/header.h>"
  , "Header hs_bindgen_ac47d7831888d8ce (void)"
  , "{"
  , "  return headerNew();"
  , "}"
  , "Header hs_bindgen_b075b057244f5b0a ("
  , "  Header arg1"
  , ")"
  , "{"
  , "  return headerFree(arg1);"
  , "}"
  , "Header hs_bindgen_8ba4789313626c8f ("
  , "  Header arg1"
  , ")"
  , "{"
  , "  return headerLink(arg1);"
  , "}"
  , "unsigned int hs_bindgen_0000981a7731aed5 ("
  , "  Header arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return headerSizeof(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_76890b170cd9142e ("
  , "  Header arg1,"
  , "  unsigned int *arg2"
  , ")"
  , "{"
  , "  return headerExport(arg1, arg2);"
  , "}"
  , "Header hs_bindgen_edc534f664930cbb ("
  , "  Header arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return headerReload(arg1, arg2);"
  , "}"
  , "Header hs_bindgen_2c08ab7a5b00c515 ("
  , "  Header arg1"
  , ")"
  , "{"
  , "  return headerCopy(arg1);"
  , "}"
  , "Header hs_bindgen_9685eb2649a99f07 ("
  , "  void *arg1,"
  , "  unsigned int arg2,"
  , "  headerImportFlags arg3"
  , ")"
  , "{"
  , "  return headerImport(arg1, arg2, arg3);"
  , "}"
  , "Header hs_bindgen_bfae3ffc9f93835b ("
  , "  FD_t arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return headerRead(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_590fd8c039572bfc ("
  , "  FD_t arg1,"
  , "  Header arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return headerWrite(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_84225f8d80a5caa9 ("
  , "  Header arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return headerIsEntry(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_00b0392e240f66c3 ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  rpmtd arg3,"
  , "  headerGetFlags arg4"
  , ")"
  , "{"
  , "  return headerGet(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_0dcb120a4215f492 ("
  , "  Header arg1,"
  , "  rpmtd arg2,"
  , "  headerPutFlags arg3"
  , ")"
  , "{"
  , "  return headerPut(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_f2707b37e88b70d2 ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  uint8_t const *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return headerPutBin(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_273889f07ead56c7 ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  char const *arg3"
  , ")"
  , "{"
  , "  return headerPutString(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_527321d2588bf5e6 ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  char const **arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return headerPutStringArray(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_d02f78fe32648f24 ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  char const *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return headerPutChar(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_4e58004e6f333e0a ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  uint8_t const *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return headerPutUint8(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_62f6e834f2b1368e ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  uint16_t const *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return headerPutUint16(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_ff364608c225a593 ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  uint32_t const *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return headerPutUint32(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_2f278f7640b0b035 ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  uint64_t const *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return headerPutUint64(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_070a4fa556b524e3 ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  char const *arg3,"
  , "  char const *arg4"
  , ")"
  , "{"
  , "  return headerAddI18NString(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_583089aa7337ac80 ("
  , "  Header arg1,"
  , "  rpmtd arg2"
  , ")"
  , "{"
  , "  return headerMod(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_f87d69f407e23f51 ("
  , "  Header arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return headerDel(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_495e98c8d78110f7 ("
  , "  Header arg1,"
  , "  char const *arg2,"
  , "  errmsg_t *arg3"
  , ")"
  , "{"
  , "  return headerFormat(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_1a9c70a5644665d1 ("
  , "  Header arg1,"
  , "  Header arg2,"
  , "  rpmTagVal const *arg3"
  , ")"
  , "{"
  , "  headerCopyTags(arg1, arg2, arg3);"
  , "}"
  , "HeaderIterator hs_bindgen_55b99dd229651d42 ("
  , "  HeaderIterator arg1"
  , ")"
  , "{"
  , "  return headerFreeIterator(arg1);"
  , "}"
  , "HeaderIterator hs_bindgen_9cf234e2adf776b2 ("
  , "  Header arg1"
  , ")"
  , "{"
  , "  return headerInitIterator(arg1);"
  , "}"
  , "signed int hs_bindgen_651ef34e8ffb83af ("
  , "  HeaderIterator arg1,"
  , "  rpmtd arg2"
  , ")"
  , "{"
  , "  return headerNext(arg1, arg2);"
  , "}"
  , "rpmTagVal hs_bindgen_aaf3950d7345cd7e ("
  , "  HeaderIterator arg1"
  , ")"
  , "{"
  , "  return headerNextTag(arg1);"
  , "}"
  , "char *hs_bindgen_be37c49afdb44f28 ("
  , "  Header arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return headerGetAsString(arg1, arg2);"
  , "}"
  , "char const *hs_bindgen_db4877d7c87fdf5c ("
  , "  Header arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return headerGetString(arg1, arg2);"
  , "}"
  , "uint64_t hs_bindgen_410c7143a3ace771 ("
  , "  Header arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return headerGetNumber(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_ab4b22b7bb814bbf ("
  , "  Header arg1"
  , ")"
  , "{"
  , "  return headerIsSource(arg1);"
  , "}"
  , "unsigned int hs_bindgen_3dc1e00d8089c094 ("
  , "  Header arg1"
  , ")"
  , "{"
  , "  return headerGetInstance(arg1);"
  , "}"
  , "signed int hs_bindgen_98a2ffde96a9618e ("
  , "  Header arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return headerConvert(arg1, arg2);"
  , "}"
  ]))

{-|

  > header

  Create new (empty) header instance.

  __returns:__ header

__C declaration:__ @headerNew@

__defined at:__ @rpm\/header.h:41:8@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_ac47d7831888d8ce" headerNew ::
     IO RPM.Types.Header

{-|

  > header

  Dereference a header instance.

  [__@h@ /(input)/__]: header

  __returns:__ NULL always

__C declaration:__ @headerFree@

__defined at:__ @rpm\/header.h:48:8@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_b075b057244f5b0a" headerFree ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> IO RPM.Types.Header

{-|

  > header

  Reference a header instance.

  [__@h@ /(input)/__]: header

  __returns:__ new header reference

__C declaration:__ @headerLink@

__defined at:__ @rpm\/header.h:55:8@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_8ba4789313626c8f" headerLink ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> IO RPM.Types.Header

{-|

  > header

  Return size of on-disk header representation in bytes.

  [__@h@ /(input)/__]: header

  [__@magicp@ /(input)/__]: include size of 8 bytes for (magic, 0)?

  __returns:__ size of on-disk header

__C declaration:__ @headerSizeof@

__defined at:__ @rpm\/header.h:63:14@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_0000981a7731aed5" headerSizeof ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> FC.CInt
     {- ^

        [__@magicp@ /(input)/__]: include size of 8 bytes for (magic, 0)?

     __C declaration:__ @magicp@
     -}
  -> IO FC.CUInt

{-|

  > header

  Export header to on-disk representation.

  [__@h@ /(input)/__]: header (with pointers)

  [__@bsize@ /(output)/__]: on-disk header blob size in bytes

  __returns:__ on-disk header blob (i.e. with offsets)

__C declaration:__ @headerExport@

__defined at:__ @rpm\/header.h:71:8@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_76890b170cd9142e" headerExport ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header (with pointers)

     __C declaration:__ @h@
     -}
  -> Ptr.Ptr FC.CUInt
     {- ^

        [__@bsize@ /(output)/__]: on-disk header blob size in bytes

     __C declaration:__ @bsize@
     -}
  -> IO (Ptr.Ptr Void)

{-|

  > header

  Convert header to on-disk representation, and then reload. This is used to insure that all header data is in one chunk.

  [__@h@ /(input)/__]: header (with pointers)

  [__@tag@ /(input)/__]: region tag

  __returns:__ on-disk header (with offsets)

__C declaration:__ @headerReload@

__defined at:__ @rpm\/header.h:80:8@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_edc534f664930cbb" headerReload ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header (with pointers)

     __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: region tag

     __C declaration:__ @tag@
     -}
  -> IO RPM.Types.Header

{-|

  > header

  Duplicate a header.

  [__@h@ /(input)/__]: header

  __returns:__ new header instance

__C declaration:__ @headerCopy@

__defined at:__ @rpm\/header.h:87:8@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_2c08ab7a5b00c515" headerCopy ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> IO RPM.Types.Header

{-|

  > header

  Import header to in-memory representation.

  [__@blob@ /(input)/__]: on-disk header blob (i.e. with offsets)

  [__@bsize@ /(input)/__]: on-disk header blob size in bytes (0 if unknown)

  [__@flags@ /(input)/__]: flags to control operation

  __returns:__ header

__C declaration:__ @headerImport@

__defined at:__ @rpm\/header.h:103:8@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_9685eb2649a99f07" headerImport ::
     Ptr.Ptr Void
     {- ^

        [__@blob@ /(input)/__]: on-disk header blob (i.e. with offsets)

     __C declaration:__ @blob@
     -}
  -> FC.CUInt
     {- ^

        [__@bsize@ /(input)/__]: on-disk header blob size in bytes (0 if unknown)

     __C declaration:__ @bsize@
     -}
  -> HeaderImportFlags
     {- ^

        [__@flags@ /(input)/__]: flags to control operation

     __C declaration:__ @flags@
     -}
  -> IO RPM.Types.Header

{-|

  > header

  Read (and load) header from file handle.

  [__@fd@ /(input)/__]: file handle

  [__@magicp@ /(input)/__]: read (and verify) 8 bytes of (magic, 0)?

  __returns:__ header (or NULL on error)

__C declaration:__ @headerRead@

__defined at:__ @rpm\/header.h:111:8@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_bfae3ffc9f93835b" headerRead ::
     RPM.Types.FD_t
     {- ^

        [__@fd@ /(input)/__]: file handle

     __C declaration:__ @fd@
     -}
  -> FC.CInt
     {- ^

        [__@magicp@ /(input)/__]: read (and verify) 8 bytes of (magic, 0)?

     __C declaration:__ @magicp@
     -}
  -> IO RPM.Types.Header

{-|

  > header

  Write (with unload) header to file handle.

  [__@fd@ /(input)/__]: file handle

  [__@h@ /(input)/__]: header

  [__@magicp@ /(input)/__]: prefix write with 8 bytes of (magic, 0)?

  __returns:__ 0 on success, 1 on error

__C declaration:__ @headerWrite@

__defined at:__ @rpm\/header.h:120:5@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_590fd8c039572bfc" headerWrite ::
     RPM.Types.FD_t
     {- ^

        [__@fd@ /(input)/__]: file handle

     __C declaration:__ @fd@
     -}
  -> RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> FC.CInt
     {- ^

        [__@magicp@ /(input)/__]: prefix write with 8 bytes of (magic, 0)?

     __C declaration:__ @magicp@
     -}
  -> IO FC.CInt

{-|

  > header

  Check if tag is in header.

  [__@h@ /(input)/__]: header

  [__@tag@ /(input)/__]: tag

  __returns:__ 1 on success, 0 on failure

__C declaration:__ @headerIsEntry@

__defined at:__ @rpm\/header.h:128:5@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_84225f8d80a5caa9" headerIsEntry ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: tag

     __C declaration:__ @tag@
     -}
  -> IO FC.CInt

{-|

  > header

  Retrieve tag value.

  [__@h@ /(input)/__]: header

  [__@tag@ /(input)/__]: tag

  [__@td@ /(output)/__]: tag data container

  [__@flags@ /(input)/__]: retrieval modifier flags

  __returns:__ 1 on success, 0 on failure

__C declaration:__ @headerGet@

__defined at:__ @rpm\/header.h:158:5@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_00b0392e240f66c3" headerGet ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: tag

     __C declaration:__ @tag@
     -}
  -> RPM.Td.Rpmtd
     {- ^

        [__@td@ /(output)/__]: tag data container

     __C declaration:__ @td@
     -}
  -> HeaderGetFlags
     {- ^

        [__@flags@ /(input)/__]: retrieval modifier flags

     __C declaration:__ @flags@
     -}
  -> IO FC.CInt

{-|

  > header

  Add or append tag to header.

  [__@h@ /(input)/__]: header

  [__@td@ /(input)/__]: tag data container

  [__@flags@ /(input)/__]: flags to control operation

  __returns:__ 1 on success, 0 on failure

__C declaration:__ @headerPut@

__defined at:__ @rpm\/header.h:176:5@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_0dcb120a4215f492" headerPut ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Td.Rpmtd
     {- ^

        [__@td@ /(input)/__]: tag data container

     __C declaration:__ @td@
     -}
  -> HeaderPutFlags
     {- ^

        [__@flags@ /(input)/__]: flags to control operation

     __C declaration:__ @flags@
     -}
  -> IO FC.CInt

{-|

  > header

  @ { Type-safe methods for inserting tag data to header. Tag data type is validated to match the function type, ie things like headerPutUint32(h, RPMTAG_NAME, ...) will return failure. For non-array types size must equal 1, and data is checked to be non-NULL. For array types, add-or-append mode is always used.

  headerPutString() can be used on both RPM_STRING_TYPE and RPM_STRING_ARRAY_TYPE (to add a single string into the array) tags, for others the type must match exactly.

  These are intended to "do the right thing" in the common case, if you need more fine grained control use headerPut() & friends instead.

  __TODO:__

  Make doxygen group these meaningfully.

  [__@h@ /(input)/__]: header

  [__@tag@ /(input)/__]: tag to insert

  [__@val@ /(input)/__]: pointer to value(s)

  [__@size@ /(input)/__]: number of items in array (1 or larger)

  __returns:__ 1 on success, 0 on failure

__C declaration:__ @headerPutBin@

__defined at:__ @rpm\/header.h:201:5@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_f2707b37e88b70d2" headerPutBin ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: tag to insert

     __C declaration:__ @tag@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word8
     {- ^

        [__@val@ /(input)/__]: pointer to value(s)

     __C declaration:__ @val@
     -}
  -> RPM.Td.Rpm_count_t
     {- ^

        [__@size@ /(input)/__]: number of items in array (1 or larger)

     __C declaration:__ @size@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @headerPutString@

    __defined at:__ @rpm\/header.h:202:5@

    __exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_273889f07ead56c7" headerPutString ::
     RPM.Types.Header
     {- ^ __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^ __C declaration:__ @tag@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @val@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @headerPutStringArray@

    __defined at:__ @rpm\/header.h:203:5@

    __exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_527321d2588bf5e6" headerPutStringArray ::
     RPM.Types.Header
     {- ^ __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^ __C declaration:__ @tag@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^ __C declaration:__ @val@
     -}
  -> RPM.Td.Rpm_count_t
     {- ^ __C declaration:__ @size@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @headerPutChar@

    __defined at:__ @rpm\/header.h:204:5@

    __exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_d02f78fe32648f24" headerPutChar ::
     RPM.Types.Header
     {- ^ __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^ __C declaration:__ @tag@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @val@
     -}
  -> RPM.Td.Rpm_count_t
     {- ^ __C declaration:__ @size@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @headerPutUint8@

    __defined at:__ @rpm\/header.h:205:5@

    __exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_4e58004e6f333e0a" headerPutUint8 ::
     RPM.Types.Header
     {- ^ __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^ __C declaration:__ @tag@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word8
     {- ^ __C declaration:__ @val@
     -}
  -> RPM.Td.Rpm_count_t
     {- ^ __C declaration:__ @size@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @headerPutUint16@

    __defined at:__ @rpm\/header.h:206:5@

    __exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_62f6e834f2b1368e" headerPutUint16 ::
     RPM.Types.Header
     {- ^ __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^ __C declaration:__ @tag@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word16
     {- ^ __C declaration:__ @val@
     -}
  -> RPM.Td.Rpm_count_t
     {- ^ __C declaration:__ @size@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @headerPutUint32@

    __defined at:__ @rpm\/header.h:207:5@

    __exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_ff364608c225a593" headerPutUint32 ::
     RPM.Types.Header
     {- ^ __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^ __C declaration:__ @tag@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word32
     {- ^ __C declaration:__ @val@
     -}
  -> RPM.Td.Rpm_count_t
     {- ^ __C declaration:__ @size@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @headerPutUint64@

    __defined at:__ @rpm\/header.h:208:5@

    __exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_2f278f7640b0b035" headerPutUint64 ::
     RPM.Types.Header
     {- ^ __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^ __C declaration:__ @tag@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word64
     {- ^ __C declaration:__ @val@
     -}
  -> RPM.Td.Rpm_count_t
     {- ^ __C declaration:__ @size@
     -}
  -> IO FC.CInt

{-|

  > header

  Add locale specific tag to header. A NULL lang is interpreted as the C locale. Here are the rules:

  @
  - If the tag isn't in the header, it's added with the passed string
  as new value.
  - If the tag occurs multiple times in entry, which tag is affected
  by the operation is undefined.
  - If the tag is in the header w/ this language, the entry is
  *replaced* (like headerMod()).
  @

  This function is intended to just "do the right thing". If you need more fine grained control use headerPut() and headerMod().

  [__@h@ /(input)/__]: header

  [__@tag@ /(input)/__]: tag

  [__@string@ /(input)/__]: tag value

  [__@lang@ /(input)/__]: locale

  __returns:__ 1 on success, 0 on failure

__C declaration:__ @headerAddI18NString@

__defined at:__ @rpm\/header.h:231:5@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_070a4fa556b524e3" headerAddI18NString ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: tag

     __C declaration:__ @tag@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@string@ /(input)/__]: tag value

     __C declaration:__ @string@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@lang@ /(input)/__]: locale

     __C declaration:__ @lang@
     -}
  -> IO FC.CInt

{-|

  > header

  Modify tag in header. If there are multiple entries with this tag, the first one gets replaced.

  [__@h@ /(input)/__]: header

  [__@td@ /(input)/__]: tag data container

  __returns:__ 1 on success, 0 on failure

__C declaration:__ @headerMod@

__defined at:__ @rpm\/header.h:241:5@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_583089aa7337ac80" headerMod ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Td.Rpmtd
     {- ^

        [__@td@ /(input)/__]: tag data container

     __C declaration:__ @td@
     -}
  -> IO FC.CInt

{-|

  > header

  Delete tag in header. Removes all entries of type tag from the header, returns 1 if none were found.

  [__@h@ /(input)/__]: header

  [__@tag@ /(input)/__]: tag

  __returns:__ 0 on success, 1 on failure (INCONSISTENT)

__C declaration:__ @headerDel@

__defined at:__ @rpm\/header.h:252:5@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_f87d69f407e23f51" headerDel ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: tag

     __C declaration:__ @tag@
     -}
  -> IO FC.CInt

{-|

  > header

  Return output formatted according to given rpm-queryformat(7) string. The returned string must be free()d.

  [__@h@ /(input)/__]: header

  [__@fmt@ /(input)/__]: format to use

  [__@errmsg@ /(output)/__]: error message (if any)

  __returns:__ formatted output string (malloc'ed)

__C declaration:__ @headerFormat@

__defined at:__ @rpm\/header.h:263:8@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_495e98c8d78110f7" headerFormat ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@fmt@ /(input)/__]: format to use

     __C declaration:__ @fmt@
     -}
  -> Ptr.Ptr RPM.Types.Errmsg_t
     {- ^

        [__@errmsg@ /(output)/__]: error message (if any)

     __C declaration:__ @errmsg@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > header

  Duplicate tag values from one header into another.

  [__@headerFrom@ /(input)/__]: source header

  [__@headerTo@ /(input)/__]: destination header

  [__@tagstocopy@ /(input)/__]: array of tags that are copied

__C declaration:__ @headerCopyTags@

__defined at:__ @rpm\/header.h:271:6@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_1a9c70a5644665d1" headerCopyTags ::
     RPM.Types.Header
     {- ^

        [__@headerFrom@ /(input)/__]: source header

     __C declaration:__ @headerFrom@
     -}
  -> RPM.Types.Header
     {- ^

        [__@headerTo@ /(input)/__]: destination header

     __C declaration:__ @headerTo@
     -}
  -> Ptr.Ptr RPM.Td.RpmTagVal
     {- ^

        [__@tagstocopy@ /(input)/__]: array of tags that are copied

     __C declaration:__ @tagstocopy@
     -}
  -> IO ()

{-|

  > header

  Destroy header tag iterator.

  [__@hi@ /(input)/__]: header tag iterator

  __returns:__ NULL always

__C declaration:__ @headerFreeIterator@

__defined at:__ @rpm\/header.h:279:16@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_55b99dd229651d42" headerFreeIterator ::
     RPM.Types.HeaderIterator
     {- ^

        [__@hi@ /(input)/__]: header tag iterator

     __C declaration:__ @hi@
     -}
  -> IO RPM.Types.HeaderIterator

{-|

  > header

  Create header tag iterator.

  [__@h@ /(input)/__]: header

  __returns:__ header tag iterator

__C declaration:__ @headerInitIterator@

__defined at:__ @rpm\/header.h:286:16@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_9cf234e2adf776b2" headerInitIterator ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> IO RPM.Types.HeaderIterator

{-|

  > header

  Return next tag contents from header.

  [__@hi@ /(input)/__]: header tag iterator

  [__@td@ /(output)/__]: tag data container

  __returns:__ 1 on success, 0 on failure

__C declaration:__ @headerNext@

__defined at:__ @rpm\/header.h:294:5@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_651ef34e8ffb83af" headerNext ::
     RPM.Types.HeaderIterator
     {- ^

        [__@hi@ /(input)/__]: header tag iterator

     __C declaration:__ @hi@
     -}
  -> RPM.Td.Rpmtd
     {- ^

        [__@td@ /(output)/__]: tag data container

     __C declaration:__ @td@
     -}
  -> IO FC.CInt

{-|

  > header

  Return next tag number from header.

  [__@hi@ /(input)/__]: header tag iterator

  __returns:__ next tag, RPMTAG_NOT_FOUND to stop iteration

__C declaration:__ @headerNextTag@

__defined at:__ @rpm\/header.h:301:11@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_aaf3950d7345cd7e" headerNextTag ::
     RPM.Types.HeaderIterator
     {- ^

        [__@hi@ /(input)/__]: header tag iterator

     __C declaration:__ @hi@
     -}
  -> IO RPM.Td.RpmTagVal

{-|

  > header

  Return any non-array tag from header, converted to string

  [__@h@ /(input)/__]: header

  [__@tag@ /(input)/__]: tag to retrieve

  __returns:__ string pointer (malloced) or NULL on failure

__C declaration:__ @headerGetAsString@

__defined at:__ @rpm\/header.h:309:8@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_be37c49afdb44f28" headerGetAsString ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: tag to retrieve

     __C declaration:__ @tag@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > header

  Return a simple string tag from header

  [__@h@ /(input)/__]: header

  [__@tag@ /(input)/__]: tag to retrieve

  __returns:__ string pointer (to header memory) or NULL on failure

__C declaration:__ @headerGetString@

__defined at:__ @rpm\/header.h:317:14@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_db4877d7c87fdf5c" headerGetString ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: tag to retrieve

     __C declaration:__ @tag@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > header

  Return a simple number tag (or extension) from header

  [__@h@ /(input)/__]: header

  [__@tag@ /(input)/__]: tag to retrieve

  __returns:__ numeric tag value or 0 on failure

__C declaration:__ @headerGetNumber@

__defined at:__ @rpm\/header.h:325:10@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_410c7143a3ace771" headerGetNumber ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> RPM.Td.RpmTagVal
     {- ^

        [__@tag@ /(input)/__]: tag to retrieve

     __C declaration:__ @tag@
     -}
  -> IO HsBindgen.Runtime.Prelude.Word64

{-|

  > header

  Check if header is a source or binary package header

  [__@h@ /(input)/__]: header

  __returns:__ 0 == binary, 1 == source

__C declaration:__ @headerIsSource@

__defined at:__ @rpm\/header.h:332:5@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_ab4b22b7bb814bbf" headerIsSource ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> IO FC.CInt

{-|

  > header

  Return header instance, ie is the header from rpmdb.

  [__@h@ /(input)/__]: header

  __returns:__ rpmdb record number or 0

__C declaration:__ @headerGetInstance@

__defined at:__ @rpm\/header.h:339:14@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_3dc1e00d8089c094" headerGetInstance ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> IO FC.CUInt

{-|

  > header

  Convert header to/from (legacy) data presentation

  [__@h@ /(input)/__]: header

  [__@op@ /(input)/__]: one of headerConvOps operations

  __returns:__ 1 on success, 0 on failure

__C declaration:__ @headerConvert@

__defined at:__ @rpm\/header.h:353:5@

__exported by:__ @rpm\/header.h@
-}
foreign import ccall unsafe "hs_bindgen_98a2ffde96a9618e" headerConvert ::
     RPM.Types.Header
     {- ^

        [__@h@ /(input)/__]: header

     __C declaration:__ @h@
     -}
  -> FC.CInt
     {- ^

        [__@op@ /(input)/__]: one of headerConvOps operations

     __C declaration:__ @op@
     -}
  -> IO FC.CInt
