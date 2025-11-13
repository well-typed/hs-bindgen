{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Lib.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Ds
import Data.Void (Void)
import Prelude (IO)
import RPM.Lib

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmlib.h>"
  , "/* get_rpmReadConfigFiles_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_492a8b26712a1544 (void)) ("
  , "  char const *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &rpmReadConfigFiles;"
  , "}"
  , "/* get_rpmGetArchInfo_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c789ca212112c91b (void)) ("
  , "  char const **arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &rpmGetArchInfo;"
  , "}"
  , "/* get_rpmGetArchColor_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_7b162e2809cb0db6 (void)) ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return &rpmGetArchColor;"
  , "}"
  , "/* get_rpmGetOsInfo_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_aad049d9481b29ae (void)) ("
  , "  char const **arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &rpmGetOsInfo;"
  , "}"
  , "/* get_rpmMachineScore_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9c73153518a9dbb9 (void)) ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &rpmMachineScore;"
  , "}"
  , "/* get_rpmShowRC_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c79feb41ab871b83 (void)) ("
  , "  FILE *arg1"
  , ")"
  , "{"
  , "  return &rpmShowRC;"
  , "}"
  , "/* get_rpmFreeRpmrc_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_31ba0ff1cbe2b7e6 (void)) (void)"
  , "{"
  , "  return &rpmFreeRpmrc;"
  , "}"
  , "/* get_rpmVersionCompare_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_34e163acc40746af (void)) ("
  , "  Header arg1,"
  , "  Header arg2"
  , ")"
  , "{"
  , "  return &rpmVersionCompare;"
  , "}"
  , "/* get_headerCheck_ptr */"
  , "__attribute__ ((const))"
  , "rpmRC (*hs_bindgen_7849d01e88d84132 (void)) ("
  , "  rpmts arg1,"
  , "  void const *arg2,"
  , "  size_t arg3,"
  , "  char **arg4"
  , ")"
  , "{"
  , "  return &headerCheck;"
  , "}"
  , "/* get_rpmReadHeader_ptr */"
  , "__attribute__ ((const))"
  , "rpmRC (*hs_bindgen_33d6c9977eb5aa1c (void)) ("
  , "  rpmts arg1,"
  , "  FD_t arg2,"
  , "  Header *arg3,"
  , "  char **arg4"
  , ")"
  , "{"
  , "  return &rpmReadHeader;"
  , "}"
  , "/* get_rpmReadPackageFile_ptr */"
  , "__attribute__ ((const))"
  , "rpmRC (*hs_bindgen_9985a3d158278386 (void)) ("
  , "  rpmts arg1,"
  , "  FD_t arg2,"
  , "  char const *arg3,"
  , "  Header *arg4"
  , ")"
  , "{"
  , "  return &rpmReadPackageFile;"
  , "}"
  , "/* get_rpmInstallSourcePackage_ptr */"
  , "__attribute__ ((const))"
  , "rpmRC (*hs_bindgen_4c184f320c0c9a76 (void)) ("
  , "  rpmts arg1,"
  , "  FD_t arg2,"
  , "  char **arg3,"
  , "  char **arg4"
  , ")"
  , "{"
  , "  return &rpmInstallSourcePackage;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_492a8b26712a1544" hs_bindgen_492a8b26712a1544 ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE rpmReadConfigFiles_ptr #-}

{-|

  > rpmrc

  Read macro configuration file(s) for a target.

  [__@file@ /(input)/__]: colon separated files to read (NULL uses default)

  [__@target@ /(input)/__]: target platform (NULL uses default)

  __returns:__ 0 on success, -1 on error

__C declaration:__ @rpmReadConfigFiles@

__defined at:__ @rpm\/rpmlib.h:58:5@

__exported by:__ @rpm\/rpmlib.h@
-}
rpmReadConfigFiles_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO FC.CInt)
rpmReadConfigFiles_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_492a8b26712a1544

foreign import ccall unsafe "hs_bindgen_c789ca212112c91b" hs_bindgen_c789ca212112c91b ::
     IO (Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE rpmGetArchInfo_ptr #-}

{-|

  > rpmrc

  Return current arch name and/or number.

  __TODO:__

  Generalize to extract arch component from target_platform macro.

  [__@name@ /(output)/__]: address of arch name (or NULL)

  [__@num@ /(output)/__]: address of arch number (or NULL)

__C declaration:__ @rpmGetArchInfo@

__defined at:__ @rpm\/rpmlib.h:67:6@

__exported by:__ @rpm\/rpmlib.h@
-}
rpmGetArchInfo_ptr :: Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> (Ptr.Ptr FC.CInt) -> IO ())
rpmGetArchInfo_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c789ca212112c91b

foreign import ccall unsafe "hs_bindgen_7b162e2809cb0db6" hs_bindgen_7b162e2809cb0db6 ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE rpmGetArchColor_ptr #-}

{-|

  > rpmrc

  Return color for an arch

  [__@arch@ /(input)/__]: name of an architecture

  __returns:__ color        color of arch, -1 if the arch couldn't be determined

__C declaration:__ @rpmGetArchColor@

__defined at:__ @rpm\/rpmlib.h:75:5@

__exported by:__ @rpm\/rpmlib.h@
-}
rpmGetArchColor_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt)
rpmGetArchColor_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7b162e2809cb0db6

foreign import ccall unsafe "hs_bindgen_aad049d9481b29ae" hs_bindgen_aad049d9481b29ae ::
     IO (Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> (Ptr.Ptr FC.CInt) -> IO ()))

{-# NOINLINE rpmGetOsInfo_ptr #-}

{-|

  > rpmrc

  Return current os name and/or number.

  __TODO:__

  Generalize to extract os component from target_platform macro.

  [__@name@ /(output)/__]: address of os name (or NULL)

  [__@num@ /(output)/__]: address of os number (or NULL)

__C declaration:__ @rpmGetOsInfo@

__defined at:__ @rpm\/rpmlib.h:83:6@

__exported by:__ @rpm\/rpmlib.h@
-}
rpmGetOsInfo_ptr :: Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> (Ptr.Ptr FC.CInt) -> IO ())
rpmGetOsInfo_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aad049d9481b29ae

foreign import ccall unsafe "hs_bindgen_9c73153518a9dbb9" hs_bindgen_9c73153518a9dbb9 ::
     IO (Ptr.FunPtr (FC.CInt -> (Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE rpmMachineScore_ptr #-}

{-|

  > rpmrc

  Return arch/os score of a name. An arch/os score measures the "nearness" of a name to the currently running (or defined) platform arch/os. For example, the score of arch "i586" on an i686 platform is (usually) 2. The arch/os score is used to select one of several otherwise identical packages using the arch/os tags from the header as hints of the intended platform for the package.

  __TODO:__

  Rewrite to use RE's against config.guess target platform output.

  [__@type@ /(input)/__]: any of the RPM_MACHTABLE_* constants

  [__@name@ /(input)/__]: name

  __returns:__ arch score (0 is no match, lower is preferred)

__C declaration:__ @rpmMachineScore@

__defined at:__ @rpm\/rpmlib.h:99:5@

__exported by:__ @rpm\/rpmlib.h@
-}
rpmMachineScore_ptr :: Ptr.FunPtr (FC.CInt -> (Ptr.Ptr FC.CChar) -> IO FC.CInt)
rpmMachineScore_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9c73153518a9dbb9

foreign import ccall unsafe "hs_bindgen_c79feb41ab871b83" hs_bindgen_c79feb41ab871b83 ::
     IO (Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> IO FC.CInt))

{-# NOINLINE rpmShowRC_ptr #-}

{-|

  > rpmrc

  Display current rpmrc (and macro) configuration.

  [__@fp@ /(input)/__]: output file handle

  __returns:__ 0 always

__C declaration:__ @rpmShowRC@

__defined at:__ @rpm\/rpmlib.h:106:5@

__exported by:__ @rpm\/rpmlib.h@
-}
rpmShowRC_ptr :: Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> IO FC.CInt)
rpmShowRC_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c79feb41ab871b83

foreign import ccall unsafe "hs_bindgen_31ba0ff1cbe2b7e6" hs_bindgen_31ba0ff1cbe2b7e6 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE rpmFreeRpmrc_ptr #-}

{-|

  > rpmrc

  Destroy rpmrc arch/os compatibility tables.

  __TODO:__

  Eliminate from API.

__C declaration:__ @rpmFreeRpmrc@

__defined at:__ @rpm\/rpmlib.h:112:6@

__exported by:__ @rpm\/rpmlib.h@
-}
rpmFreeRpmrc_ptr :: Ptr.FunPtr (IO ())
rpmFreeRpmrc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_31ba0ff1cbe2b7e6

foreign import ccall unsafe "hs_bindgen_34e163acc40746af" hs_bindgen_34e163acc40746af ::
     IO (Ptr.FunPtr (Header -> Header -> IO FC.CInt))

{-# NOINLINE rpmVersionCompare_ptr #-}

{-| Compare headers to determine which header is "newer".

  [__@first@ /(input)/__]: 1st header

  [__@second@ /(input)/__]: 2nd header

  __returns:__ result of comparison

__C declaration:__ @rpmVersionCompare@

__defined at:__ @rpm\/rpmlib.h:120:5@

__exported by:__ @rpm\/rpmlib.h@
-}
rpmVersionCompare_ptr :: Ptr.FunPtr (Header -> Header -> IO FC.CInt)
rpmVersionCompare_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_34e163acc40746af

foreign import ccall unsafe "hs_bindgen_7849d01e88d84132" hs_bindgen_7849d01e88d84132 ::
     IO (Ptr.FunPtr (Rpmts -> (Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RPM.Ds.RpmRC))

{-# NOINLINE headerCheck_ptr #-}

{-|

  > header

  Check header consistency, performing headerGetEntry() the hard way.

  Sanity checks on the header are performed while looking for a header-only digest or signature to verify the blob. If found, the digest or signature is verified.

  [__@ts@ /(input)/__]: transaction set

  [__@uh@ /(input)/__]: unloaded header blob

  [__@uc@ /(input)/__]: no. of bytes in blob (or 0 to disable)

  [__@*msg@ /(output)/__]: verification error message (or NULL)

  __returns:__ RPMRC_OK on success

__C declaration:__ @headerCheck@

__defined at:__ @rpm\/rpmlib.h:135:7@

__exported by:__ @rpm\/rpmlib.h@
-}
headerCheck_ptr :: Ptr.FunPtr (Rpmts -> (Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RPM.Ds.RpmRC)
headerCheck_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7849d01e88d84132

foreign import ccall unsafe "hs_bindgen_33d6c9977eb5aa1c" hs_bindgen_33d6c9977eb5aa1c ::
     IO (Ptr.FunPtr (Rpmts -> FD_t -> (Ptr.Ptr Header) -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RPM.Ds.RpmRC))

{-# NOINLINE rpmReadHeader_ptr #-}

{-|

  > header

  Return checked and loaded header.

  [__@ts@ /(input)/__]: unused

  [__@fd@ /(input)/__]: file handle

  [__@hdrp@ /(output)/__]: address of header (or NULL)

  [__@*msg@ /(output)/__]: verification error message (or NULL)

  __returns:__ RPMRC_OK on success

__C declaration:__ @rpmReadHeader@

__defined at:__ @rpm\/rpmlib.h:145:7@

__exported by:__ @rpm\/rpmlib.h@
-}
rpmReadHeader_ptr :: Ptr.FunPtr (Rpmts -> FD_t -> (Ptr.Ptr Header) -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RPM.Ds.RpmRC)
rpmReadHeader_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_33d6c9977eb5aa1c

foreign import ccall unsafe "hs_bindgen_9985a3d158278386" hs_bindgen_9985a3d158278386 ::
     IO (Ptr.FunPtr (Rpmts -> FD_t -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr Header) -> IO RPM.Ds.RpmRC))

{-# NOINLINE rpmReadPackageFile_ptr #-}

{-|

  > header

  Return package header from file handle, verifying digests/signatures.

  [__@ts@ /(input)/__]: transaction set

  [__@fd@ /(input)/__]: file handle

  [__@fn@ /(input)/__]: file name

  [__@hdrp@ /(output)/__]: address of header (or NULL)

  __returns:__ RPMRC_OK on success

__C declaration:__ @rpmReadPackageFile@

__defined at:__ @rpm\/rpmlib.h:155:7@

__exported by:__ @rpm\/rpmlib.h@
-}
rpmReadPackageFile_ptr :: Ptr.FunPtr (Rpmts -> FD_t -> (Ptr.Ptr FC.CChar) -> (Ptr.Ptr Header) -> IO RPM.Ds.RpmRC)
rpmReadPackageFile_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9985a3d158278386

foreign import ccall unsafe "hs_bindgen_4c184f320c0c9a76" hs_bindgen_4c184f320c0c9a76 ::
     IO (Ptr.FunPtr (Rpmts -> FD_t -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RPM.Ds.RpmRC))

{-# NOINLINE rpmInstallSourcePackage_ptr #-}

{-|

  > rpmts

  Install source package.

  [__@ts@ /(input)/__]: transaction set

  [__@fd@ /(input)/__]: file handle

  [__@specFilePtr@ /(output)/__]: address of spec file name (or NULL)

  [__@cookie@ /(output)/__]: address of cookie pointer (or NULL)

  __returns:__ rpmRC return code

__C declaration:__ @rpmInstallSourcePackage@

__defined at:__ @rpm\/rpmlib.h:166:7@

__exported by:__ @rpm\/rpmlib.h@
-}
rpmInstallSourcePackage_ptr :: Ptr.FunPtr (Rpmts -> FD_t -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> (Ptr.Ptr (Ptr.Ptr FC.CChar)) -> IO RPM.Ds.RpmRC)
rpmInstallSourcePackage_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4c184f320c0c9a76
