{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Td.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Argv
import qualified RPM.Tag
import Prelude (IO)
import RPM.Td

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmtd.h>"
  , "/* get_rpmtdNew_ptr */"
  , "__attribute__ ((const))"
  , "rpmtd (*hs_bindgen_fd26e820ad435f07 (void)) (void)"
  , "{"
  , "  return &rpmtdNew;"
  , "}"
  , "/* get_rpmtdFree_ptr */"
  , "__attribute__ ((const))"
  , "rpmtd (*hs_bindgen_cc69ce210713fdf7 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdFree;"
  , "}"
  , "/* get_rpmtdReset_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e4bf29dc413af3b5 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdReset;"
  , "}"
  , "/* get_rpmtdFreeData_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e71200670187f95f (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdFreeData;"
  , "}"
  , "/* get_rpmtdCount_ptr */"
  , "__attribute__ ((const))"
  , "rpm_count_t (*hs_bindgen_8b12d981e290deca (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdCount;"
  , "}"
  , "/* get_rpmtdSize_ptr */"
  , "__attribute__ ((const))"
  , "rpm_count_t (*hs_bindgen_0321e2d1fcda8c43 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdSize;"
  , "}"
  , "/* get_rpmtdTag_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagVal (*hs_bindgen_e622a8e380213cfb (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdTag;"
  , "}"
  , "/* get_rpmtdType_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagType (*hs_bindgen_016fd8860e55202a (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdType;"
  , "}"
  , "/* get_rpmtdClass_ptr */"
  , "__attribute__ ((const))"
  , "rpmTagClass (*hs_bindgen_439709293c5be147 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdClass;"
  , "}"
  , "/* get_rpmtdGetFlags_ptr */"
  , "__attribute__ ((const))"
  , "rpmtdFlags (*hs_bindgen_b6e884ab4376f831 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdGetFlags;"
  , "}"
  , "/* get_rpmtdGetIndex_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4da6f2cb272b50aa (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdGetIndex;"
  , "}"
  , "/* get_rpmtdSetIndex_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4bfa2e66ed12e632 (void)) ("
  , "  rpmtd arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &rpmtdSetIndex;"
  , "}"
  , "/* get_rpmtdInit_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a31622c3cd2b5486 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdInit;"
  , "}"
  , "/* get_rpmtdNext_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_72914aa88f3804f4 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdNext;"
  , "}"
  , "/* get_rpmtdNextUint32_ptr */"
  , "__attribute__ ((const))"
  , "uint32_t *(*hs_bindgen_3ecd9dfaffbcc59b (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdNextUint32;"
  , "}"
  , "/* get_rpmtdNextUint64_ptr */"
  , "__attribute__ ((const))"
  , "uint64_t *(*hs_bindgen_4978b7376011fdda (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdNextUint64;"
  , "}"
  , "/* get_rpmtdNextString_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_c6e673aaa2a1b502 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdNextString;"
  , "}"
  , "/* get_rpmtdGetChar_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_457ccdd495565dc6 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdGetChar;"
  , "}"
  , "/* get_rpmtdGetUint16_ptr */"
  , "__attribute__ ((const))"
  , "uint16_t *(*hs_bindgen_b9736ae432f108cd (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdGetUint16;"
  , "}"
  , "/* get_rpmtdGetUint32_ptr */"
  , "__attribute__ ((const))"
  , "uint32_t *(*hs_bindgen_91e8dddbb71c9795 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdGetUint32;"
  , "}"
  , "/* get_rpmtdGetUint64_ptr */"
  , "__attribute__ ((const))"
  , "uint64_t *(*hs_bindgen_6eb653d94a355e53 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdGetUint64;"
  , "}"
  , "/* get_rpmtdGetString_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_a1289a328f066273 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdGetString;"
  , "}"
  , "/* get_rpmtdGetNumber_ptr */"
  , "__attribute__ ((const))"
  , "uint64_t (*hs_bindgen_e56db7061a090d8a (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdGetNumber;"
  , "}"
  , "/* get_rpmtdFormat_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_c5c7d01f6579d083 (void)) ("
  , "  rpmtd arg1,"
  , "  rpmtdFormats arg2,"
  , "  char const *arg3"
  , ")"
  , "{"
  , "  return &rpmtdFormat;"
  , "}"
  , "/* get_rpmtdSetTag_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_86705672ab187f9d (void)) ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2"
  , ")"
  , "{"
  , "  return &rpmtdSetTag;"
  , "}"
  , "/* get_rpmtdFromUint8_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_56ac141d938dfbe1 (void)) ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  uint8_t *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return &rpmtdFromUint8;"
  , "}"
  , "/* get_rpmtdFromUint16_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c0040ed856e88f8a (void)) ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  uint16_t *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return &rpmtdFromUint16;"
  , "}"
  , "/* get_rpmtdFromUint32_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_214f22109b2aed0f (void)) ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  uint32_t *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return &rpmtdFromUint32;"
  , "}"
  , "/* get_rpmtdFromUint64_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_55e54858240b3e77 (void)) ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  uint64_t *arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return &rpmtdFromUint64;"
  , "}"
  , "/* get_rpmtdFromString_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_e443b74a6eee532b (void)) ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  char const *arg3"
  , ")"
  , "{"
  , "  return &rpmtdFromString;"
  , "}"
  , "/* get_rpmtdFromStringArray_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_d5700b8cc5522c94 (void)) ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  char const **arg3,"
  , "  rpm_count_t arg4"
  , ")"
  , "{"
  , "  return &rpmtdFromStringArray;"
  , "}"
  , "/* get_rpmtdFromArgv_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_1d5f9fa0c11b6e41 (void)) ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  ARGV_t arg3"
  , ")"
  , "{"
  , "  return &rpmtdFromArgv;"
  , "}"
  , "/* get_rpmtdFromArgi_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_5113eadcaa4d498c (void)) ("
  , "  rpmtd arg1,"
  , "  rpmTagVal arg2,"
  , "  ARGI_t arg3"
  , ")"
  , "{"
  , "  return &rpmtdFromArgi;"
  , "}"
  , "/* get_rpmtdDup_ptr */"
  , "__attribute__ ((const))"
  , "rpmtd (*hs_bindgen_298d8c46ed5cef64 (void)) ("
  , "  rpmtd arg1"
  , ")"
  , "{"
  , "  return &rpmtdDup;"
  , "}"
  , "/* get_rpmtdToPool_ptr */"
  , "__attribute__ ((const))"
  , "rpmsid *(*hs_bindgen_788553b5e3b445dd (void)) ("
  , "  rpmtd arg1,"
  , "  rpmstrPool arg2"
  , ")"
  , "{"
  , "  return &rpmtdToPool;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_fd26e820ad435f07" hs_bindgen_fd26e820ad435f07 ::
     IO (Ptr.FunPtr (IO Rpmtd))

{-# NOINLINE rpmtdNew_ptr #-}

{-|

  > rpmtd

  Create new tag data container

  __returns:__ New, initialized tag data container.

__C declaration:__ @rpmtdNew@

__defined at:__ @rpm\/rpmtd.h:49:7@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdNew_ptr :: Ptr.FunPtr (IO Rpmtd)
rpmtdNew_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fd26e820ad435f07

foreign import ccall unsafe "hs_bindgen_cc69ce210713fdf7" hs_bindgen_cc69ce210713fdf7 ::
     IO (Ptr.FunPtr (Rpmtd -> IO Rpmtd))

{-# NOINLINE rpmtdFree_ptr #-}

{-|

  > rpmtd

  Destroy tag data container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ NULL always

__C declaration:__ @rpmtdFree@

__defined at:__ @rpm\/rpmtd.h:56:7@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdFree_ptr :: Ptr.FunPtr (Rpmtd -> IO Rpmtd)
rpmtdFree_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cc69ce210713fdf7

foreign import ccall unsafe "hs_bindgen_e4bf29dc413af3b5" hs_bindgen_e4bf29dc413af3b5 ::
     IO (Ptr.FunPtr (Rpmtd -> IO ()))

{-# NOINLINE rpmtdReset_ptr #-}

{-|

  > rpmtd

  (Re-)initialize tag data container. Contents will be zeroed out and iteration index reset.

  [__@td@ /(input)/__]: Tag data container

__C declaration:__ @rpmtdReset@

__defined at:__ @rpm\/rpmtd.h:63:6@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdReset_ptr :: Ptr.FunPtr (Rpmtd -> IO ())
rpmtdReset_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e4bf29dc413af3b5

foreign import ccall unsafe "hs_bindgen_e71200670187f95f" hs_bindgen_e71200670187f95f ::
     IO (Ptr.FunPtr (Rpmtd -> IO ()))

{-# NOINLINE rpmtdFreeData_ptr #-}

{-|

  > rpmtd

  Free contained data. This is always safe to call as the container knows if data was malloc'ed or not. Container is reinitialized.

  [__@td@ /(input)/__]: Tag data container

__C declaration:__ @rpmtdFreeData@

__defined at:__ @rpm\/rpmtd.h:70:6@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdFreeData_ptr :: Ptr.FunPtr (Rpmtd -> IO ())
rpmtdFreeData_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e71200670187f95f

foreign import ccall unsafe "hs_bindgen_8b12d981e290deca" hs_bindgen_8b12d981e290deca ::
     IO (Ptr.FunPtr (Rpmtd -> IO Rpm_count_t))

{-# NOINLINE rpmtdCount_ptr #-}

{-|

  > rpmtd

  Retrieve array size of the container. For non-array types this is always 1.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Number of entries in contained data.

__C declaration:__ @rpmtdCount@

__defined at:__ @rpm\/rpmtd.h:77:13@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdCount_ptr :: Ptr.FunPtr (Rpmtd -> IO Rpm_count_t)
rpmtdCount_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8b12d981e290deca

foreign import ccall unsafe "hs_bindgen_0321e2d1fcda8c43" hs_bindgen_0321e2d1fcda8c43 ::
     IO (Ptr.FunPtr (Rpmtd -> IO Rpm_count_t))

{-# NOINLINE rpmtdSize_ptr #-}

{-|

  > rpmtd

  Retrieve container data size (eg required for allocation). Note this currently only works for RPMTD_IMMUTABLE data.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Data size in bytes.

__C declaration:__ @rpmtdSize@

__defined at:__ @rpm\/rpmtd.h:85:13@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdSize_ptr :: Ptr.FunPtr (Rpmtd -> IO Rpm_count_t)
rpmtdSize_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0321e2d1fcda8c43

foreign import ccall unsafe "hs_bindgen_e622a8e380213cfb" hs_bindgen_e622a8e380213cfb ::
     IO (Ptr.FunPtr (Rpmtd -> IO RpmTagVal))

{-# NOINLINE rpmtdTag_ptr #-}

{-|

  > rpmtd

  Retrieve tag of the container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Rpm tag.

__C declaration:__ @rpmtdTag@

__defined at:__ @rpm\/rpmtd.h:92:11@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdTag_ptr :: Ptr.FunPtr (Rpmtd -> IO RpmTagVal)
rpmtdTag_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e622a8e380213cfb

foreign import ccall unsafe "hs_bindgen_016fd8860e55202a" hs_bindgen_016fd8860e55202a ::
     IO (Ptr.FunPtr (Rpmtd -> IO RPM.Tag.RpmTagType))

{-# NOINLINE rpmtdType_ptr #-}

{-|

  > rpmtd

  Retrieve type of the container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Rpm tag type.

__C declaration:__ @rpmtdType@

__defined at:__ @rpm\/rpmtd.h:99:12@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdType_ptr :: Ptr.FunPtr (Rpmtd -> IO RPM.Tag.RpmTagType)
rpmtdType_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_016fd8860e55202a

foreign import ccall unsafe "hs_bindgen_439709293c5be147" hs_bindgen_439709293c5be147 ::
     IO (Ptr.FunPtr (Rpmtd -> IO RpmTagClass))

{-# NOINLINE rpmtdClass_ptr #-}

{-|

  > rpmtd

  Retrieve class of the container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Rpm tag class

__C declaration:__ @rpmtdClass@

__defined at:__ @rpm\/rpmtd.h:106:13@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdClass_ptr :: Ptr.FunPtr (Rpmtd -> IO RpmTagClass)
rpmtdClass_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_439709293c5be147

foreign import ccall unsafe "hs_bindgen_b6e884ab4376f831" hs_bindgen_b6e884ab4376f831 ::
     IO (Ptr.FunPtr (Rpmtd -> IO RpmtdFlags))

{-# NOINLINE rpmtdGetFlags_ptr #-}

{-|

  > rpmtd

  Retrieve flags of the container (allocation details etc)

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Container flags

__C declaration:__ @rpmtdGetFlags@

__defined at:__ @rpm\/rpmtd.h:113:12@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdGetFlags_ptr :: Ptr.FunPtr (Rpmtd -> IO RpmtdFlags)
rpmtdGetFlags_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b6e884ab4376f831

foreign import ccall unsafe "hs_bindgen_4da6f2cb272b50aa" hs_bindgen_4da6f2cb272b50aa ::
     IO (Ptr.FunPtr (Rpmtd -> IO FC.CInt))

{-# NOINLINE rpmtdGetIndex_ptr #-}

{-|

  > rpmtd

  Retrieve current iteration index of the container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Iteration index (or -1 if not iterating)

__C declaration:__ @rpmtdGetIndex@

__defined at:__ @rpm\/rpmtd.h:120:5@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdGetIndex_ptr :: Ptr.FunPtr (Rpmtd -> IO FC.CInt)
rpmtdGetIndex_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4da6f2cb272b50aa

foreign import ccall unsafe "hs_bindgen_4bfa2e66ed12e632" hs_bindgen_4bfa2e66ed12e632 ::
     IO (Ptr.FunPtr (Rpmtd -> FC.CInt -> IO FC.CInt))

{-# NOINLINE rpmtdSetIndex_ptr #-}

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
rpmtdSetIndex_ptr :: Ptr.FunPtr (Rpmtd -> FC.CInt -> IO FC.CInt)
rpmtdSetIndex_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4bfa2e66ed12e632

foreign import ccall unsafe "hs_bindgen_a31622c3cd2b5486" hs_bindgen_a31622c3cd2b5486 ::
     IO (Ptr.FunPtr (Rpmtd -> IO FC.CInt))

{-# NOINLINE rpmtdInit_ptr #-}

{-|

  > rpmtd

  Initialize tag container for iteration

  [__@td@ /(input)/__]: Tag data container

  __returns:__ 0 on success

__C declaration:__ @rpmtdInit@

__defined at:__ @rpm\/rpmtd.h:137:5@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdInit_ptr :: Ptr.FunPtr (Rpmtd -> IO FC.CInt)
rpmtdInit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a31622c3cd2b5486

foreign import ccall unsafe "hs_bindgen_72914aa88f3804f4" hs_bindgen_72914aa88f3804f4 ::
     IO (Ptr.FunPtr (Rpmtd -> IO FC.CInt))

{-# NOINLINE rpmtdNext_ptr #-}

{-|

  > rpmtd

  Iterate over tag data container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Tag data container iterator index, -1 on termination

__C declaration:__ @rpmtdNext@

__defined at:__ @rpm\/rpmtd.h:144:5@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdNext_ptr :: Ptr.FunPtr (Rpmtd -> IO FC.CInt)
rpmtdNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_72914aa88f3804f4

foreign import ccall unsafe "hs_bindgen_3ecd9dfaffbcc59b" hs_bindgen_3ecd9dfaffbcc59b ::
     IO (Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32)))

{-# NOINLINE rpmtdNextUint32_ptr #-}

{-|

  > rpmtd

  Iterate over uint32_t type tag data container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to next value, NULL on termination or error

__C declaration:__ @rpmtdNextUint32@

__defined at:__ @rpm\/rpmtd.h:151:11@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdNextUint32_ptr :: Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32))
rpmtdNextUint32_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3ecd9dfaffbcc59b

foreign import ccall unsafe "hs_bindgen_4978b7376011fdda" hs_bindgen_4978b7376011fdda ::
     IO (Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word64)))

{-# NOINLINE rpmtdNextUint64_ptr #-}

{-|

  > rpmtd

  Iterate over uint64_t type tag data container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to next value, NULL on termination or error

__C declaration:__ @rpmtdNextUint64@

__defined at:__ @rpm\/rpmtd.h:158:11@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdNextUint64_ptr :: Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word64))
rpmtdNextUint64_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4978b7376011fdda

foreign import ccall unsafe "hs_bindgen_c6e673aaa2a1b502" hs_bindgen_c6e673aaa2a1b502 ::
     IO (Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmtdNextString_ptr #-}

{-|

  > rpmtd

  Iterate over string / string array type tag data container.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to next value, NULL on termination or error

__C declaration:__ @rpmtdNextString@

__defined at:__ @rpm\/rpmtd.h:165:13@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdNextString_ptr :: Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr FC.CChar))
rpmtdNextString_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c6e673aaa2a1b502

foreign import ccall unsafe "hs_bindgen_457ccdd495565dc6" hs_bindgen_457ccdd495565dc6 ::
     IO (Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmtdGetChar_ptr #-}

{-|

  > rpmtd

  Return char data from tag container. For scalar return type, just return pointer to the integer. On array types, return pointer to current iteration index. If the tag container is not for char type, NULL is returned.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to uint16_t, NULL on error

__C declaration:__ @rpmtdGetChar@

__defined at:__ @rpm\/rpmtd.h:175:7@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdGetChar_ptr :: Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr FC.CChar))
rpmtdGetChar_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_457ccdd495565dc6

foreign import ccall unsafe "hs_bindgen_b9736ae432f108cd" hs_bindgen_b9736ae432f108cd ::
     IO (Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word16)))

{-# NOINLINE rpmtdGetUint16_ptr #-}

{-|

  > rpmtd

  Return uint16_t data from tag container. For scalar return type, just return pointer to the integer. On array types, return pointer to current iteration index. If the tag container is not for int16 type, NULL is returned.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to uint16_t, NULL on error

__C declaration:__ @rpmtdGetUint16@

__defined at:__ @rpm\/rpmtd.h:185:12@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdGetUint16_ptr :: Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word16))
rpmtdGetUint16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b9736ae432f108cd

foreign import ccall unsafe "hs_bindgen_91e8dddbb71c9795" hs_bindgen_91e8dddbb71c9795 ::
     IO (Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32)))

{-# NOINLINE rpmtdGetUint32_ptr #-}

{-|

  > rpmtd

  Return uint32_t data from tag container. For scalar return type, just return pointer to the integer. On array types, return pointer to current iteration index. If the tag container is not for int32 type, NULL is returned.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to uint32_t, NULL on error

__C declaration:__ @rpmtdGetUint32@

__defined at:__ @rpm\/rpmtd.h:195:12@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdGetUint32_ptr :: Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32))
rpmtdGetUint32_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_91e8dddbb71c9795

foreign import ccall unsafe "hs_bindgen_6eb653d94a355e53" hs_bindgen_6eb653d94a355e53 ::
     IO (Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word64)))

{-# NOINLINE rpmtdGetUint64_ptr #-}

{-|

  > rpmtd

  Return uint64_t data from tag container. For scalar return type, just return pointer to the integer. On array types, return pointer to current iteration index. If the tag container is not for int64 type, NULL is returned.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Pointer to uint64_t, NULL on error

__C declaration:__ @rpmtdGetUint64@

__defined at:__ @rpm\/rpmtd.h:205:12@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdGetUint64_ptr :: Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word64))
rpmtdGetUint64_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6eb653d94a355e53

foreign import ccall unsafe "hs_bindgen_a1289a328f066273" hs_bindgen_a1289a328f066273 ::
     IO (Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmtdGetString_ptr #-}

{-|

  > rpmtd

  Return string data from tag container. For string types, just return the string. On string array types, return the string from current iteration index. If the tag container is not for a string type, NULL is returned.

  [__@td@ /(input)/__]: Tag data container

  __returns:__ String constant from container, NULL on error

__C declaration:__ @rpmtdGetString@

__defined at:__ @rpm\/rpmtd.h:215:14@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdGetString_ptr :: Ptr.FunPtr (Rpmtd -> IO (Ptr.Ptr FC.CChar))
rpmtdGetString_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a1289a328f066273

foreign import ccall unsafe "hs_bindgen_e56db7061a090d8a" hs_bindgen_e56db7061a090d8a ::
     IO (Ptr.FunPtr (Rpmtd -> IO HsBindgen.Runtime.Prelude.Word64))

{-# NOINLINE rpmtdGetNumber_ptr #-}

{-|

  > rpmtd

  Return numeric value from tag container. Returns the value of numeric container (RPM_NUMERIC_CLASS) from current iteration index as uint64_t regardless of its internal presentation (8/16/32/64-bit integer).

  [__@td@ /(input)/__]: Tag data container

  __returns:__ Value of current iteration item as uint64_t, 0 for non-numeric types (error)

__C declaration:__ @rpmtdGetNumber@

__defined at:__ @rpm\/rpmtd.h:226:10@

__exported by:__ @rpm\/rpmtd.h@
-}
rpmtdGetNumber_ptr :: Ptr.FunPtr (Rpmtd -> IO HsBindgen.Runtime.Prelude.Word64)
rpmtdGetNumber_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e56db7061a090d8a

foreign import ccall unsafe "hs_bindgen_c5c7d01f6579d083" hs_bindgen_c5c7d01f6579d083 ::
     IO (Ptr.FunPtr (Rpmtd -> RpmtdFormats -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rpmtdFormat_ptr #-}

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
rpmtdFormat_ptr :: Ptr.FunPtr (Rpmtd -> RpmtdFormats -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FC.CChar))
rpmtdFormat_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c5c7d01f6579d083

foreign import ccall unsafe "hs_bindgen_86705672ab187f9d" hs_bindgen_86705672ab187f9d ::
     IO (Ptr.FunPtr (Rpmtd -> RpmTagVal -> IO FC.CInt))

{-# NOINLINE rpmtdSetTag_ptr #-}

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
rpmtdSetTag_ptr :: Ptr.FunPtr (Rpmtd -> RpmTagVal -> IO FC.CInt)
rpmtdSetTag_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_86705672ab187f9d

foreign import ccall unsafe "hs_bindgen_56ac141d938dfbe1" hs_bindgen_56ac141d938dfbe1 ::
     IO (Ptr.FunPtr (Rpmtd -> RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> Rpm_count_t -> IO FC.CInt))

{-# NOINLINE rpmtdFromUint8_ptr #-}

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
rpmtdFromUint8_ptr :: Ptr.FunPtr (Rpmtd -> RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> Rpm_count_t -> IO FC.CInt)
rpmtdFromUint8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_56ac141d938dfbe1

foreign import ccall unsafe "hs_bindgen_c0040ed856e88f8a" hs_bindgen_c0040ed856e88f8a ::
     IO (Ptr.FunPtr (Rpmtd -> RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word16) -> Rpm_count_t -> IO FC.CInt))

{-# NOINLINE rpmtdFromUint16_ptr #-}

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
rpmtdFromUint16_ptr :: Ptr.FunPtr (Rpmtd -> RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word16) -> Rpm_count_t -> IO FC.CInt)
rpmtdFromUint16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c0040ed856e88f8a

foreign import ccall unsafe "hs_bindgen_214f22109b2aed0f" hs_bindgen_214f22109b2aed0f ::
     IO (Ptr.FunPtr (Rpmtd -> RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32) -> Rpm_count_t -> IO FC.CInt))

{-# NOINLINE rpmtdFromUint32_ptr #-}

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
rpmtdFromUint32_ptr :: Ptr.FunPtr (Rpmtd -> RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32) -> Rpm_count_t -> IO FC.CInt)
rpmtdFromUint32_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_214f22109b2aed0f

foreign import ccall unsafe "hs_bindgen_55e54858240b3e77" hs_bindgen_55e54858240b3e77 ::
     IO (Ptr.FunPtr (Rpmtd -> RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word64) -> Rpm_count_t -> IO FC.CInt))

{-# NOINLINE rpmtdFromUint64_ptr #-}

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
rpmtdFromUint64_ptr :: Ptr.FunPtr (Rpmtd -> RpmTagVal -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word64) -> Rpm_count_t -> IO FC.CInt)
rpmtdFromUint64_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_55e54858240b3e77

foreign import ccall unsafe "hs_bindgen_e443b74a6eee532b" hs_bindgen_e443b74a6eee532b ::
     IO (Ptr.FunPtr (Rpmtd -> RpmTagVal -> (Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE rpmtdFromString_ptr #-}

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
rpmtdFromString_ptr :: Ptr.FunPtr (Rpmtd -> RpmTagVal -> (Ptr.Ptr FC.CChar) -> IO FC.CInt)
rpmtdFromString_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e443b74a6eee532b

foreign import ccall unsafe "hs_bindgen_d5700b8cc5522c94" hs_bindgen_d5700b8cc5522c94 ::
     IO (Ptr.FunPtr (Rpmtd -> RpmTagVal -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> Rpm_count_t -> IO FC.CInt))

{-# NOINLINE rpmtdFromStringArray_ptr #-}

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
rpmtdFromStringArray_ptr :: Ptr.FunPtr (Rpmtd -> RpmTagVal -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> Rpm_count_t -> IO FC.CInt)
rpmtdFromStringArray_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d5700b8cc5522c94

foreign import ccall unsafe "hs_bindgen_1d5f9fa0c11b6e41" hs_bindgen_1d5f9fa0c11b6e41 ::
     IO (Ptr.FunPtr (Rpmtd -> RpmTagVal -> RPM.Argv.ARGV_t -> IO FC.CInt))

{-# NOINLINE rpmtdFromArgv_ptr #-}

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
rpmtdFromArgv_ptr :: Ptr.FunPtr (Rpmtd -> RpmTagVal -> RPM.Argv.ARGV_t -> IO FC.CInt)
rpmtdFromArgv_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1d5f9fa0c11b6e41

foreign import ccall unsafe "hs_bindgen_5113eadcaa4d498c" hs_bindgen_5113eadcaa4d498c ::
     IO (Ptr.FunPtr (Rpmtd -> RpmTagVal -> RPM.Argv.ARGI_t -> IO FC.CInt))

{-# NOINLINE rpmtdFromArgi_ptr #-}

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
rpmtdFromArgi_ptr :: Ptr.FunPtr (Rpmtd -> RpmTagVal -> RPM.Argv.ARGI_t -> IO FC.CInt)
rpmtdFromArgi_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5113eadcaa4d498c

foreign import ccall unsafe "hs_bindgen_298d8c46ed5cef64" hs_bindgen_298d8c46ed5cef64 ::
     IO (Ptr.FunPtr (Rpmtd -> IO Rpmtd))

{-# NOINLINE rpmtdDup_ptr #-}

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
rpmtdDup_ptr :: Ptr.FunPtr (Rpmtd -> IO Rpmtd)
rpmtdDup_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_298d8c46ed5cef64

foreign import ccall unsafe "hs_bindgen_788553b5e3b445dd" hs_bindgen_788553b5e3b445dd ::
     IO (Ptr.FunPtr (Rpmtd -> RpmstrPool -> IO (Ptr.Ptr Rpmsid)))

{-# NOINLINE rpmtdToPool_ptr #-}

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
rpmtdToPool_ptr :: Ptr.FunPtr (Rpmtd -> RpmstrPool -> IO (Ptr.Ptr Rpmsid))
rpmtdToPool_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_788553b5e3b445dd
