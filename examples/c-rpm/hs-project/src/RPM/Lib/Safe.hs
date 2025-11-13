{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Lib.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Ds
import Data.Void (Void)
import Prelude (IO)
import RPM.Lib

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmlib.h>"
  , "signed int hs_bindgen_995d176e39b13904 ("
  , "  char const *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return rpmReadConfigFiles(arg1, arg2);"
  , "}"
  , "void hs_bindgen_6d27f95b7ff3f420 ("
  , "  char const **arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  rpmGetArchInfo(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_9acb8e99d7982570 ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return rpmGetArchColor(arg1);"
  , "}"
  , "void hs_bindgen_23092549656b36bd ("
  , "  char const **arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  rpmGetOsInfo(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_ae711f20cd942b9f ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return rpmMachineScore(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_e11929d4a378c400 ("
  , "  FILE *arg1"
  , ")"
  , "{"
  , "  return rpmShowRC(arg1);"
  , "}"
  , "void hs_bindgen_f98d86dc3770615d (void)"
  , "{"
  , "  rpmFreeRpmrc();"
  , "}"
  , "signed int hs_bindgen_5bfdeea2c17e11c7 ("
  , "  Header arg1,"
  , "  Header arg2"
  , ")"
  , "{"
  , "  return rpmVersionCompare(arg1, arg2);"
  , "}"
  , "rpmRC hs_bindgen_0423ed0479df6f57 ("
  , "  rpmts arg1,"
  , "  void const *arg2,"
  , "  size_t arg3,"
  , "  char **arg4"
  , ")"
  , "{"
  , "  return headerCheck(arg1, arg2, arg3, arg4);"
  , "}"
  , "rpmRC hs_bindgen_ebb5b20047672efa ("
  , "  rpmts arg1,"
  , "  FD_t arg2,"
  , "  Header *arg3,"
  , "  char **arg4"
  , ")"
  , "{"
  , "  return rpmReadHeader(arg1, arg2, arg3, arg4);"
  , "}"
  , "rpmRC hs_bindgen_154e23e78d317a55 ("
  , "  rpmts arg1,"
  , "  FD_t arg2,"
  , "  char const *arg3,"
  , "  Header *arg4"
  , ")"
  , "{"
  , "  return rpmReadPackageFile(arg1, arg2, arg3, arg4);"
  , "}"
  , "rpmRC hs_bindgen_5e91ead0713aaa5a ("
  , "  rpmts arg1,"
  , "  FD_t arg2,"
  , "  char **arg3,"
  , "  char **arg4"
  , ")"
  , "{"
  , "  return rpmInstallSourcePackage(arg1, arg2, arg3, arg4);"
  , "}"
  ]))

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
foreign import ccall safe "hs_bindgen_995d176e39b13904" rpmReadConfigFiles ::
     Ptr.Ptr FC.CChar
     {- ^

        [__@file@ /(input)/__]: colon separated files to read (NULL uses default)

     __C declaration:__ @file@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@target@ /(input)/__]: target platform (NULL uses default)

     __C declaration:__ @target@
     -}
  -> IO FC.CInt

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
foreign import ccall safe "hs_bindgen_6d27f95b7ff3f420" rpmGetArchInfo ::
     Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^

        [__@name@ /(output)/__]: address of arch name (or NULL)

     __C declaration:__ @name@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^

        [__@num@ /(output)/__]: address of arch number (or NULL)

     __C declaration:__ @num@
     -}
  -> IO ()

{-|

  > rpmrc

  Return color for an arch

  [__@arch@ /(input)/__]: name of an architecture

  __returns:__ color        color of arch, -1 if the arch couldn't be determined

__C declaration:__ @rpmGetArchColor@

__defined at:__ @rpm\/rpmlib.h:75:5@

__exported by:__ @rpm\/rpmlib.h@
-}
foreign import ccall safe "hs_bindgen_9acb8e99d7982570" rpmGetArchColor ::
     Ptr.Ptr FC.CChar
     {- ^

        [__@arch@ /(input)/__]: name of an architecture

     __C declaration:__ @arch@
     -}
  -> IO FC.CInt

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
foreign import ccall safe "hs_bindgen_23092549656b36bd" rpmGetOsInfo ::
     Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^

        [__@name@ /(output)/__]: address of os name (or NULL)

     __C declaration:__ @name@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^

        [__@num@ /(output)/__]: address of os number (or NULL)

     __C declaration:__ @num@
     -}
  -> IO ()

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
foreign import ccall safe "hs_bindgen_ae711f20cd942b9f" rpmMachineScore ::
     FC.CInt
     {- ^ __C declaration:__ @type'@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@name@ /(input)/__]: name

     __C declaration:__ @name@
     -}
  -> IO FC.CInt

{-|

  > rpmrc

  Display current rpmrc (and macro) configuration.

  [__@fp@ /(input)/__]: output file handle

  __returns:__ 0 always

__C declaration:__ @rpmShowRC@

__defined at:__ @rpm\/rpmlib.h:106:5@

__exported by:__ @rpm\/rpmlib.h@
-}
foreign import ccall safe "hs_bindgen_e11929d4a378c400" rpmShowRC ::
     Ptr.Ptr HsBindgen.Runtime.Prelude.CFile
     {- ^

        [__@fp@ /(input)/__]: output file handle

     __C declaration:__ @fp@
     -}
  -> IO FC.CInt

{-|

  > rpmrc

  Destroy rpmrc arch/os compatibility tables.

  __TODO:__

  Eliminate from API.

__C declaration:__ @rpmFreeRpmrc@

__defined at:__ @rpm\/rpmlib.h:112:6@

__exported by:__ @rpm\/rpmlib.h@
-}
foreign import ccall safe "hs_bindgen_f98d86dc3770615d" rpmFreeRpmrc ::
     IO ()

{-| Compare headers to determine which header is "newer".

  [__@first@ /(input)/__]: 1st header

  [__@second@ /(input)/__]: 2nd header

  __returns:__ result of comparison

__C declaration:__ @rpmVersionCompare@

__defined at:__ @rpm\/rpmlib.h:120:5@

__exported by:__ @rpm\/rpmlib.h@
-}
foreign import ccall safe "hs_bindgen_5bfdeea2c17e11c7" rpmVersionCompare ::
     Header
     {- ^

        [__@first@ /(input)/__]: 1st header

     __C declaration:__ @first@
     -}
  -> Header
     {- ^

        [__@second@ /(input)/__]: 2nd header

     __C declaration:__ @second@
     -}
  -> IO FC.CInt

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
foreign import ccall safe "hs_bindgen_0423ed0479df6f57" headerCheck ::
     Rpmts
     {- ^

        [__@ts@ /(input)/__]: transaction set

     __C declaration:__ @ts@
     -}
  -> Ptr.Ptr Void
     {- ^

        [__@uh@ /(input)/__]: unloaded header blob

     __C declaration:__ @uh@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^

        [__@uc@ /(input)/__]: no. of bytes in blob (or 0 to disable)

     __C declaration:__ @uc@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^ __C declaration:__ @msg@
     -}
  -> IO RPM.Ds.RpmRC

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
foreign import ccall safe "hs_bindgen_ebb5b20047672efa" rpmReadHeader ::
     Rpmts
     {- ^

        [__@ts@ /(input)/__]: unused

     __C declaration:__ @ts@
     -}
  -> FD_t
     {- ^

        [__@fd@ /(input)/__]: file handle

     __C declaration:__ @fd@
     -}
  -> Ptr.Ptr Header
     {- ^

        [__@hdrp@ /(output)/__]: address of header (or NULL)

     __C declaration:__ @hdrp@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^ __C declaration:__ @msg@
     -}
  -> IO RPM.Ds.RpmRC

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
foreign import ccall safe "hs_bindgen_154e23e78d317a55" rpmReadPackageFile ::
     Rpmts
     {- ^

        [__@ts@ /(input)/__]: transaction set

     __C declaration:__ @ts@
     -}
  -> FD_t
     {- ^

        [__@fd@ /(input)/__]: file handle

     __C declaration:__ @fd@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^

        [__@fn@ /(input)/__]: file name

     __C declaration:__ @fn@
     -}
  -> Ptr.Ptr Header
     {- ^

        [__@hdrp@ /(output)/__]: address of header (or NULL)

     __C declaration:__ @hdrp@
     -}
  -> IO RPM.Ds.RpmRC

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
foreign import ccall safe "hs_bindgen_5e91ead0713aaa5a" rpmInstallSourcePackage ::
     Rpmts
     {- ^

        [__@ts@ /(input)/__]: transaction set

     __C declaration:__ @ts@
     -}
  -> FD_t
     {- ^

        [__@fd@ /(input)/__]: file handle

     __C declaration:__ @fd@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^

        [__@specFilePtr@ /(output)/__]: address of spec file name (or NULL)

     __C declaration:__ @specFilePtr@
     -}
  -> Ptr.Ptr (Ptr.Ptr FC.CChar)
     {- ^

        [__@cookie@ /(output)/__]: address of cookie pointer (or NULL)

     __C declaration:__ @cookie@
     -}
  -> IO RPM.Ds.RpmRC
