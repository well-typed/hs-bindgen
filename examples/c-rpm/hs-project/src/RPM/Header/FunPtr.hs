{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Header.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Td
import qualified RPM.Types
import Data.Void (Void)
import Prelude (IO)
import RPM.Header

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/header.h>"
  , "/* get_headerNew_ptr */"
  , "__attribute__ ((const))"
  , "Header (*hs_bindgen_5436a8f000c602b6 (void)) (void)"
  , "{"
  , "  return &headerNew;"
  , "}"
  , "/* get_headerFree_ptr */"
  , "__attribute__ ((const))"
  , "Header (*hs_bindgen_78bdd7088706bd37 (void)) ("
  , "  Header arg1"
  , ")"
  , "{"
  , "  return &headerFree;"
  , "}"
  , "/* get_headerLink_ptr */"
  , "__attribute__ ((const))"
  , "Header (*hs_bindgen_97ba1289c6cd8628 (void)) ("
  , "  Header arg1"
  , ")"
  , "{"
  , "  return &headerLink;"
  , "}"
  , "/* get_headerSizeof_ptr */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_ad9d3d6e09678429 (void)) ("
  , "  Header arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &headerSizeof;"
  , "}"
  , "/* get_headerExport_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_d4570f9fd1cf9ddb (void)) ("
  , "  Header arg1,"
  , "  unsigned int *arg2"
  , ")"
  , "{"
  , "  return &headerExport;"
  , "}"
  , "/* get_headerReload_ptr */"
  , "__attribute__ ((const))"
  , "Header (*hs_bindgen_8e2941f44d12e542 (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return &headerReload;"
  , "}"
  , "/* get_headerCopy_ptr */"
  , "__attribute__ ((const))"
  , "Header (*hs_bindgen_38510e525a2e7b4e (void)) ("
  , "  Header arg1"
  , ")"
  , "{"
  , "  return &headerCopy;"
  , "}"
  , "/* get_headerImport_ptr */"
  , "__attribute__ ((const))"
  , "Header (*hs_bindgen_aa46fee0720b69e5 (void)) ("
  , "  void *arg1,"
  , "  unsigned int arg2,"
  , "  headerImportFlags arg3"
  , ")"
  , "{"
  , "  return &headerImport;"
  , "}"
  , "/* get_headerRead_ptr */"
  , "__attribute__ ((const))"
  , "Header (*hs_bindgen_344db26fb396fb75 (void)) ("
  , "  FD_t arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &headerRead;"
  , "}"
  , "/* get_headerWrite_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_7e4aa072286154d5 (void)) ("
  , "  FD_t arg1,"
  , "  Header arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &headerWrite;"
  , "}"
  , "/* get_headerIsEntry_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_df647687d91fbbda (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return &headerIsEntry;"
  , "}"
  , "/* get_headerGet_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_53a3515235d9b210 (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  rpmtd arg3,"
  , "  headerGetFlags arg4"
  , ")"
  , "{"
  , "  return &headerGet;"
  , "}"
  , "/* get_headerPut_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_020953ea84cf19e5 (void)) ("
  , "  Header arg1,"
  , "  rpmtd arg2,"
  , "  headerPutFlags arg3"
  , ")"
  , "{"
  , "  return &headerPut;"
  , "}"
  , "/* get_headerPutBin_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_7f168dbcec423bf8 (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  uint8_t const *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return &headerPutBin;"
  , "}"
  , "/* get_headerPutString_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c30949b9dbaca51d (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  char const *arg3"
  , ")"
  , "{"
  , "  return &headerPutString;"
  , "}"
  , "/* get_headerPutStringArray_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_f18e967934346c24 (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  char const **arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return &headerPutStringArray;"
  , "}"
  , "/* get_headerPutChar_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3548a16805b2bd9a (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  char const *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return &headerPutChar;"
  , "}"
  , "/* get_headerPutUint8_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_211ec34587673d80 (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  uint8_t const *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return &headerPutUint8;"
  , "}"
  , "/* get_headerPutUint16_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_25e067dfdbb05d1f (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  uint16_t const *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return &headerPutUint16;"
  , "}"
  , "/* get_headerPutUint32_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_28a53cbc3730fb2f (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  uint32_t const *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return &headerPutUint32;"
  , "}"
  , "/* get_headerPutUint64_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_7e6f6ebed7294dfe (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  uint64_t const *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return &headerPutUint64;"
  , "}"
  , "/* get_headerAddI18NString_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9e5f8f3b4d8bc182 (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2,"
  , "  char const *arg3,"
  , "  char const *arg4"
  , ")"
  , "{"
  , "  return &headerAddI18NString;"
  , "}"
  , "/* get_headerMod_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_ae4575c44e026380 (void)) ("
  , "  Header arg1,"
  , "  rpmtd arg2"
  , ")"
  , "{"
  , "  return &headerMod;"
  , "}"
  , "/* get_headerDel_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_47dead3d6bb425a0 (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return &headerDel;"
  , "}"
  , "/* get_headerFormat_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_a69f749d1112f8a6 (void)) ("
  , "  Header arg1,"
  , "  char const *arg2,"
  , "  errmsg_t *arg3"
  , ")"
  , "{"
  , "  return &headerFormat;"
  , "}"
  , "/* get_headerCopyTags_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8484cda7b1c27124 (void)) ("
  , "  Header arg1,"
  , "  Header arg2,"
  , "  rpmTagVal const *arg3"
  , ")"
  , "{"
  , "  return &headerCopyTags;"
  , "}"
  , "/* get_headerFreeIterator_ptr */"
  , "__attribute__ ((const))"
  , "HeaderIterator (*hs_bindgen_1a71e8cfe452ce60 (void)) ("
  , "  HeaderIterator arg1"
  , ")"
  , "{"
  , "  return &headerFreeIterator;"
  , "}"
  , "/* get_headerInitIterator_ptr */"
  , "__attribute__ ((const))"
  , "HeaderIterator (*hs_bindgen_536e1462b3e3a2e0 (void)) ("
  , "  Header arg1"
  , ")"
  , "{"
  , "  return &headerInitIterator;"
  , "}"
  , "/* get_headerNext_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_517e20a37053531c (void)) ("
  , "  HeaderIterator arg1,"
  , "  rpmtd arg2"
  , ")"
  , "{"
  , "  return &headerNext;"
  , "}"
  , "/* get_headerNextTag_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagVal (*hs_bindgen_91b1e6321559cd42 (void)) ("
  , "  HeaderIterator arg1"
  , ")"
  , "{"
  , "  return &headerNextTag;"
  , "}"
  , "/* get_headerGetAsString_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_372fb43b672da8a3 (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return &headerGetAsString;"
  , "}"
  , "/* get_headerGetString_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_8c612795ce460779 (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return &headerGetString;"
  , "}"
  , "/* get_headerGetNumber_ptr */"
  , "__attribute__ ((const))"
  , "uint64_t (*hs_bindgen_173950b6c136c994 (void)) ("
  , "  Header arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return &headerGetNumber;"
  , "}"
  , "/* get_headerIsSource_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_57293b4129a2b981 (void)) ("
  , "  Header arg1"
  , ")"
  , "{"
  , "  return &headerIsSource;"
  , "}"
  , "/* get_headerGetInstance_ptr */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_8c509fcfda428a29 (void)) ("
  , "  Header arg1"
  , ")"
  , "{"
  , "  return &headerGetInstance;"
  , "}"
  , "/* get_headerConvert_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_91ef666f919b4b33 (void)) ("
  , "  Header arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &headerConvert;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_5436a8f000c602b6" hs_bindgen_5436a8f000c602b6 ::
     IO (Ptr.FunPtr (IO RPM.Types.Header))

{-# NOINLINE headerNew_ptr #-}

{-|

  > header

  Create new (empty) header instance.

  __returns:__ header

__C declaration:__ @headerNew@

__defined at:__ @rpm\/header.h:41:8@

__exported by:__ @rpm\/header.h@
-}
headerNew_ptr :: Ptr.FunPtr (IO RPM.Types.Header)
headerNew_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5436a8f000c602b6

foreign import ccall unsafe "hs_bindgen_78bdd7088706bd37" hs_bindgen_78bdd7088706bd37 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> IO RPM.Types.Header))

{-# NOINLINE headerFree_ptr #-}

{-|

  > header

  Dereference a header instance.

  [__@h@ /(input)/__]: header

  __returns:__ NULL always

__C declaration:__ @headerFree@

__defined at:__ @rpm\/header.h:48:8@

__exported by:__ @rpm\/header.h@
-}
headerFree_ptr :: Ptr.FunPtr (RPM.Types.Header -> IO RPM.Types.Header)
headerFree_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_78bdd7088706bd37

foreign import ccall unsafe "hs_bindgen_97ba1289c6cd8628" hs_bindgen_97ba1289c6cd8628 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> IO RPM.Types.Header))

{-# NOINLINE headerLink_ptr #-}

{-|

  > header

  Reference a header instance.

  [__@h@ /(input)/__]: header

  __returns:__ new header reference

__C declaration:__ @headerLink@

__defined at:__ @rpm\/header.h:55:8@

__exported by:__ @rpm\/header.h@
-}
headerLink_ptr :: Ptr.FunPtr (RPM.Types.Header -> IO RPM.Types.Header)
headerLink_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_97ba1289c6cd8628

foreign import ccall unsafe "hs_bindgen_ad9d3d6e09678429" hs_bindgen_ad9d3d6e09678429 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> FC.CInt -> IO FC.CUInt))

{-# NOINLINE headerSizeof_ptr #-}

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
headerSizeof_ptr :: Ptr.FunPtr (RPM.Types.Header -> FC.CInt -> IO FC.CUInt)
headerSizeof_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ad9d3d6e09678429

foreign import ccall unsafe "hs_bindgen_d4570f9fd1cf9ddb" hs_bindgen_d4570f9fd1cf9ddb ::
     IO (Ptr.FunPtr (RPM.Types.Header -> (Ptr.Ptr FC.CUInt) -> IO (Ptr.Ptr Void)))

{-# NOINLINE headerExport_ptr #-}

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
headerExport_ptr :: Ptr.FunPtr (RPM.Types.Header -> (Ptr.Ptr FC.CUInt) -> IO (Ptr.Ptr Void))
headerExport_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d4570f9fd1cf9ddb

foreign import ccall unsafe "hs_bindgen_8e2941f44d12e542" hs_bindgen_8e2941f44d12e542 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> IO RPM.Types.Header))

{-# NOINLINE headerReload_ptr #-}

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
headerReload_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> IO RPM.Types.Header)
headerReload_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8e2941f44d12e542

foreign import ccall unsafe "hs_bindgen_38510e525a2e7b4e" hs_bindgen_38510e525a2e7b4e ::
     IO (Ptr.FunPtr (RPM.Types.Header -> IO RPM.Types.Header))

{-# NOINLINE headerCopy_ptr #-}

{-|

  > header

  Duplicate a header.

  [__@h@ /(input)/__]: header

  __returns:__ new header instance

__C declaration:__ @headerCopy@

__defined at:__ @rpm\/header.h:87:8@

__exported by:__ @rpm\/header.h@
-}
headerCopy_ptr :: Ptr.FunPtr (RPM.Types.Header -> IO RPM.Types.Header)
headerCopy_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_38510e525a2e7b4e

foreign import ccall unsafe "hs_bindgen_aa46fee0720b69e5" hs_bindgen_aa46fee0720b69e5 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> FC.CUInt -> HeaderImportFlags -> IO RPM.Types.Header))

{-# NOINLINE headerImport_ptr #-}

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
headerImport_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> FC.CUInt -> HeaderImportFlags -> IO RPM.Types.Header)
headerImport_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aa46fee0720b69e5

foreign import ccall unsafe "hs_bindgen_344db26fb396fb75" hs_bindgen_344db26fb396fb75 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> FC.CInt -> IO RPM.Types.Header))

{-# NOINLINE headerRead_ptr #-}

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
headerRead_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> FC.CInt -> IO RPM.Types.Header)
headerRead_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_344db26fb396fb75

foreign import ccall unsafe "hs_bindgen_7e4aa072286154d5" hs_bindgen_7e4aa072286154d5 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> RPM.Types.Header -> FC.CInt -> IO FC.CInt))

{-# NOINLINE headerWrite_ptr #-}

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
headerWrite_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> RPM.Types.Header -> FC.CInt -> IO FC.CInt)
headerWrite_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7e4aa072286154d5

foreign import ccall unsafe "hs_bindgen_df647687d91fbbda" hs_bindgen_df647687d91fbbda ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> IO FC.CInt))

{-# NOINLINE headerIsEntry_ptr #-}

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
headerIsEntry_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> IO FC.CInt)
headerIsEntry_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_df647687d91fbbda

foreign import ccall unsafe "hs_bindgen_53a3515235d9b210" hs_bindgen_53a3515235d9b210 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> RPM.Td.Rpmtd -> HeaderGetFlags -> IO FC.CInt))

{-# NOINLINE headerGet_ptr #-}

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
headerGet_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> RPM.Td.Rpmtd -> HeaderGetFlags -> IO FC.CInt)
headerGet_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_53a3515235d9b210

foreign import ccall unsafe "hs_bindgen_020953ea84cf19e5" hs_bindgen_020953ea84cf19e5 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.Rpmtd -> HeaderPutFlags -> IO FC.CInt))

{-# NOINLINE headerPut_ptr #-}

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
headerPut_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.Rpmtd -> HeaderPutFlags -> IO FC.CInt)
headerPut_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_020953ea84cf19e5

foreign import ccall unsafe "hs_bindgen_7f168dbcec423bf8" hs_bindgen_7f168dbcec423bf8 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> RPM.Td.Rpm_count_t -> IO FC.CInt))

{-# NOINLINE headerPutBin_ptr #-}

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
headerPutBin_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> RPM.Td.Rpm_count_t -> IO FC.CInt)
headerPutBin_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7f168dbcec423bf8

foreign import ccall unsafe "hs_bindgen_c30949b9dbaca51d" hs_bindgen_c30949b9dbaca51d ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE headerPutString_ptr #-}

{-| __C declaration:__ @headerPutString@

    __defined at:__ @rpm\/header.h:202:5@

    __exported by:__ @rpm\/header.h@
-}
headerPutString_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr FC.CChar) -> IO FC.CInt)
headerPutString_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c30949b9dbaca51d

foreign import ccall unsafe "hs_bindgen_f18e967934346c24" hs_bindgen_f18e967934346c24 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> RPM.Td.Rpm_count_t -> IO FC.CInt))

{-# NOINLINE headerPutStringArray_ptr #-}

{-| __C declaration:__ @headerPutStringArray@

    __defined at:__ @rpm\/header.h:203:5@

    __exported by:__ @rpm\/header.h@
-}
headerPutStringArray_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> RPM.Td.Rpm_count_t -> IO FC.CInt)
headerPutStringArray_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f18e967934346c24

foreign import ccall unsafe "hs_bindgen_3548a16805b2bd9a" hs_bindgen_3548a16805b2bd9a ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr FC.CChar) -> RPM.Td.Rpm_count_t -> IO FC.CInt))

{-# NOINLINE headerPutChar_ptr #-}

{-| __C declaration:__ @headerPutChar@

    __defined at:__ @rpm\/header.h:204:5@

    __exported by:__ @rpm\/header.h@
-}
headerPutChar_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr FC.CChar) -> RPM.Td.Rpm_count_t -> IO FC.CInt)
headerPutChar_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3548a16805b2bd9a

foreign import ccall unsafe "hs_bindgen_211ec34587673d80" hs_bindgen_211ec34587673d80 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> RPM.Td.Rpm_count_t -> IO FC.CInt))

{-# NOINLINE headerPutUint8_ptr #-}

{-| __C declaration:__ @headerPutUint8@

    __defined at:__ @rpm\/header.h:205:5@

    __exported by:__ @rpm\/header.h@
-}
headerPutUint8_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> RPM.Td.Rpm_count_t -> IO FC.CInt)
headerPutUint8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_211ec34587673d80

foreign import ccall unsafe "hs_bindgen_25e067dfdbb05d1f" hs_bindgen_25e067dfdbb05d1f ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word16) -> RPM.Td.Rpm_count_t -> IO FC.CInt))

{-# NOINLINE headerPutUint16_ptr #-}

{-| __C declaration:__ @headerPutUint16@

    __defined at:__ @rpm\/header.h:206:5@

    __exported by:__ @rpm\/header.h@
-}
headerPutUint16_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word16) -> RPM.Td.Rpm_count_t -> IO FC.CInt)
headerPutUint16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_25e067dfdbb05d1f

foreign import ccall unsafe "hs_bindgen_28a53cbc3730fb2f" hs_bindgen_28a53cbc3730fb2f ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32) -> RPM.Td.Rpm_count_t -> IO FC.CInt))

{-# NOINLINE headerPutUint32_ptr #-}

{-| __C declaration:__ @headerPutUint32@

    __defined at:__ @rpm\/header.h:207:5@

    __exported by:__ @rpm\/header.h@
-}
headerPutUint32_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32) -> RPM.Td.Rpm_count_t -> IO FC.CInt)
headerPutUint32_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_28a53cbc3730fb2f

foreign import ccall unsafe "hs_bindgen_7e6f6ebed7294dfe" hs_bindgen_7e6f6ebed7294dfe ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word64) -> RPM.Td.Rpm_count_t -> IO FC.CInt))

{-# NOINLINE headerPutUint64_ptr #-}

{-| __C declaration:__ @headerPutUint64@

    __defined at:__ @rpm\/header.h:208:5@

    __exported by:__ @rpm\/header.h@
-}
headerPutUint64_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word64) -> RPM.Td.Rpm_count_t -> IO FC.CInt)
headerPutUint64_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7e6f6ebed7294dfe

foreign import ccall unsafe "hs_bindgen_9e5f8f3b4d8bc182" hs_bindgen_9e5f8f3b4d8bc182 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE headerAddI18NString_ptr #-}

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
headerAddI18NString_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt)
headerAddI18NString_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9e5f8f3b4d8bc182

foreign import ccall unsafe "hs_bindgen_ae4575c44e026380" hs_bindgen_ae4575c44e026380 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.Rpmtd -> IO FC.CInt))

{-# NOINLINE headerMod_ptr #-}

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
headerMod_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.Rpmtd -> IO FC.CInt)
headerMod_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ae4575c44e026380

foreign import ccall unsafe "hs_bindgen_47dead3d6bb425a0" hs_bindgen_47dead3d6bb425a0 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> IO FC.CInt))

{-# NOINLINE headerDel_ptr #-}

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
headerDel_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> IO FC.CInt)
headerDel_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_47dead3d6bb425a0

foreign import ccall unsafe "hs_bindgen_a69f749d1112f8a6" hs_bindgen_a69f749d1112f8a6 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr RPM.Types.Errmsg_t) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE headerFormat_ptr #-}

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
headerFormat_ptr :: Ptr.FunPtr (RPM.Types.Header -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr RPM.Types.Errmsg_t) -> IO (Ptr.Ptr FC.CChar))
headerFormat_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a69f749d1112f8a6

foreign import ccall unsafe "hs_bindgen_8484cda7b1c27124" hs_bindgen_8484cda7b1c27124 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Types.Header -> (Ptr.Ptr RPM.Td.RpmTagVal) -> IO ()))

{-# NOINLINE headerCopyTags_ptr #-}

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
headerCopyTags_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Types.Header -> (Ptr.Ptr RPM.Td.RpmTagVal) -> IO ())
headerCopyTags_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8484cda7b1c27124

foreign import ccall unsafe "hs_bindgen_1a71e8cfe452ce60" hs_bindgen_1a71e8cfe452ce60 ::
     IO (Ptr.FunPtr (RPM.Types.HeaderIterator -> IO RPM.Types.HeaderIterator))

{-# NOINLINE headerFreeIterator_ptr #-}

{-|

  > header

  Destroy header tag iterator.

  [__@hi@ /(input)/__]: header tag iterator

  __returns:__ NULL always

__C declaration:__ @headerFreeIterator@

__defined at:__ @rpm\/header.h:279:16@

__exported by:__ @rpm\/header.h@
-}
headerFreeIterator_ptr :: Ptr.FunPtr (RPM.Types.HeaderIterator -> IO RPM.Types.HeaderIterator)
headerFreeIterator_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1a71e8cfe452ce60

foreign import ccall unsafe "hs_bindgen_536e1462b3e3a2e0" hs_bindgen_536e1462b3e3a2e0 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> IO RPM.Types.HeaderIterator))

{-# NOINLINE headerInitIterator_ptr #-}

{-|

  > header

  Create header tag iterator.

  [__@h@ /(input)/__]: header

  __returns:__ header tag iterator

__C declaration:__ @headerInitIterator@

__defined at:__ @rpm\/header.h:286:16@

__exported by:__ @rpm\/header.h@
-}
headerInitIterator_ptr :: Ptr.FunPtr (RPM.Types.Header -> IO RPM.Types.HeaderIterator)
headerInitIterator_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_536e1462b3e3a2e0

foreign import ccall unsafe "hs_bindgen_517e20a37053531c" hs_bindgen_517e20a37053531c ::
     IO (Ptr.FunPtr (RPM.Types.HeaderIterator -> RPM.Td.Rpmtd -> IO FC.CInt))

{-# NOINLINE headerNext_ptr #-}

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
headerNext_ptr :: Ptr.FunPtr (RPM.Types.HeaderIterator -> RPM.Td.Rpmtd -> IO FC.CInt)
headerNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_517e20a37053531c

foreign import ccall unsafe "hs_bindgen_91b1e6321559cd42" hs_bindgen_91b1e6321559cd42 ::
     IO (Ptr.FunPtr (RPM.Types.HeaderIterator -> IO RPM.Td.RpmTagVal))

{-# NOINLINE headerNextTag_ptr #-}

{-|

  > header

  Return next tag number from header.

  [__@hi@ /(input)/__]: header tag iterator

  __returns:__ next tag, RPMTAG_NOT_FOUND to stop iteration

__C declaration:__ @headerNextTag@

__defined at:__ @rpm\/header.h:301:11@

__exported by:__ @rpm\/header.h@
-}
headerNextTag_ptr :: Ptr.FunPtr (RPM.Types.HeaderIterator -> IO RPM.Td.RpmTagVal)
headerNextTag_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_91b1e6321559cd42

foreign import ccall unsafe "hs_bindgen_372fb43b672da8a3" hs_bindgen_372fb43b672da8a3 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE headerGetAsString_ptr #-}

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
headerGetAsString_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> IO (Ptr.Ptr FC.CChar))
headerGetAsString_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_372fb43b672da8a3

foreign import ccall unsafe "hs_bindgen_8c612795ce460779" hs_bindgen_8c612795ce460779 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE headerGetString_ptr #-}

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
headerGetString_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> IO (Ptr.Ptr FC.CChar))
headerGetString_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8c612795ce460779

foreign import ccall unsafe "hs_bindgen_173950b6c136c994" hs_bindgen_173950b6c136c994 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> IO HsBindgen.Runtime.Prelude.Word64))

{-# NOINLINE headerGetNumber_ptr #-}

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
headerGetNumber_ptr :: Ptr.FunPtr (RPM.Types.Header -> RPM.Td.RpmTagVal -> IO HsBindgen.Runtime.Prelude.Word64)
headerGetNumber_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_173950b6c136c994

foreign import ccall unsafe "hs_bindgen_57293b4129a2b981" hs_bindgen_57293b4129a2b981 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> IO FC.CInt))

{-# NOINLINE headerIsSource_ptr #-}

{-|

  > header

  Check if header is a source or binary package header

  [__@h@ /(input)/__]: header

  __returns:__ 0 == binary, 1 == source

__C declaration:__ @headerIsSource@

__defined at:__ @rpm\/header.h:332:5@

__exported by:__ @rpm\/header.h@
-}
headerIsSource_ptr :: Ptr.FunPtr (RPM.Types.Header -> IO FC.CInt)
headerIsSource_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_57293b4129a2b981

foreign import ccall unsafe "hs_bindgen_8c509fcfda428a29" hs_bindgen_8c509fcfda428a29 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> IO FC.CUInt))

{-# NOINLINE headerGetInstance_ptr #-}

{-|

  > header

  Return header instance, ie is the header from rpmdb.

  [__@h@ /(input)/__]: header

  __returns:__ rpmdb record number or 0

__C declaration:__ @headerGetInstance@

__defined at:__ @rpm\/header.h:339:14@

__exported by:__ @rpm\/header.h@
-}
headerGetInstance_ptr :: Ptr.FunPtr (RPM.Types.Header -> IO FC.CUInt)
headerGetInstance_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8c509fcfda428a29

foreign import ccall unsafe "hs_bindgen_91ef666f919b4b33" hs_bindgen_91ef666f919b4b33 ::
     IO (Ptr.FunPtr (RPM.Types.Header -> FC.CInt -> IO FC.CInt))

{-# NOINLINE headerConvert_ptr #-}

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
headerConvert_ptr :: Ptr.FunPtr (RPM.Types.Header -> FC.CInt -> IO FC.CInt)
headerConvert_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_91ef666f919b4b33
