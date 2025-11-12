{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.IO.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Sw
import qualified RPM.Types
import Data.Void (Void)
import Prelude (IO)
import RPM.IO

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmio.h>"
  , "/* get_Fstrerror_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_d5a266b84a79dce6 (void)) ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return &Fstrerror;"
  , "}"
  , "/* get_Fread_ptr */"
  , "__attribute__ ((const))"
  , "ssize_t (*hs_bindgen_86d945b810e7d8d0 (void)) ("
  , "  void *arg1,"
  , "  size_t arg2,"
  , "  size_t arg3,"
  , "  FD_t arg4"
  , ")"
  , "{"
  , "  return &Fread;"
  , "}"
  , "/* get_Fwrite_ptr */"
  , "__attribute__ ((const))"
  , "ssize_t (*hs_bindgen_4b604d0b8d2f38d3 (void)) ("
  , "  void const *arg1,"
  , "  size_t arg2,"
  , "  size_t arg3,"
  , "  FD_t arg4"
  , ")"
  , "{"
  , "  return &Fwrite;"
  , "}"
  , "/* get_Fseek_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_990e3acf610efd62 (void)) ("
  , "  FD_t arg1,"
  , "  off_t arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &Fseek;"
  , "}"
  , "/* get_Ftell_ptr */"
  , "__attribute__ ((const))"
  , "off_t (*hs_bindgen_3cd682b1666e3f39 (void)) ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return &Ftell;"
  , "}"
  , "/* get_Fclose_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3a8b4d016ce765ec (void)) ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return &Fclose;"
  , "}"
  , "/* get_Fdopen_ptr */"
  , "__attribute__ ((const))"
  , "FD_t (*hs_bindgen_28aba02a8e660902 (void)) ("
  , "  FD_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &Fdopen;"
  , "}"
  , "/* get_Fopen_ptr */"
  , "__attribute__ ((const))"
  , "FD_t (*hs_bindgen_ea22dee4eb8d7877 (void)) ("
  , "  char const *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &Fopen;"
  , "}"
  , "/* get_Fflush_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_59f16f2c9968d876 (void)) ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return &Fflush;"
  , "}"
  , "/* get_Ferror_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_faa81cb20f2e2c90 (void)) ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return &Ferror;"
  , "}"
  , "/* get_Fileno_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_851025c50836b507 (void)) ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return &Fileno;"
  , "}"
  , "/* get_Fcntl_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_38416ab66d4abccd (void)) ("
  , "  FD_t arg1,"
  , "  signed int arg2,"
  , "  void *arg3"
  , ")"
  , "{"
  , "  return &Fcntl;"
  , "}"
  , "/* get_Fdescr_ptr */"
  , "__attribute__ ((const))"
  , "char const *(*hs_bindgen_7e02840ef8a5b409 (void)) ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return &Fdescr;"
  , "}"
  , "/* get_fdSize_ptr */"
  , "__attribute__ ((const))"
  , "off_t (*hs_bindgen_aabdee04f2505a08 (void)) ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return &fdSize;"
  , "}"
  , "/* get_fdDup_ptr */"
  , "__attribute__ ((const))"
  , "FD_t (*hs_bindgen_b8519bf2143252bd (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &fdDup;"
  , "}"
  , "/* get_fdLink_ptr */"
  , "__attribute__ ((const))"
  , "FD_t (*hs_bindgen_9d4b09a9b47473e5 (void)) ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return &fdLink;"
  , "}"
  , "/* get_fdFree_ptr */"
  , "__attribute__ ((const))"
  , "FD_t (*hs_bindgen_394abc86ffec9714 (void)) ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return &fdFree;"
  , "}"
  , "/* get_ufdCopy_ptr */"
  , "__attribute__ ((const))"
  , "off_t (*hs_bindgen_e0fb1fb7cc6d6222 (void)) ("
  , "  FD_t arg1,"
  , "  FD_t arg2"
  , ")"
  , "{"
  , "  return &ufdCopy;"
  , "}"
  , "/* get_fdOp_ptr */"
  , "__attribute__ ((const))"
  , "rpmop (*hs_bindgen_a76438c28f5bc5d7 (void)) ("
  , "  FD_t arg1,"
  , "  fdOpX arg2"
  , ")"
  , "{"
  , "  return &fdOp;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_d5a266b84a79dce6" hs_bindgen_d5a266b84a79dce6 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE fstrerror_ptr #-}

{-|

  > rpmio

  strerror(3) clone.

__C declaration:__ @Fstrerror@

__defined at:__ @rpm\/rpmio.h:30:14@

__exported by:__ @rpm\/rpmio.h@
-}
fstrerror_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> IO (Ptr.Ptr FC.CChar))
fstrerror_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d5a266b84a79dce6

foreign import ccall unsafe "hs_bindgen_86d945b810e7d8d0" hs_bindgen_86d945b810e7d8d0 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> RPM.Types.FD_t -> IO Ssize_t))

{-# NOINLINE fread_ptr #-}

{-|

  > rpmio

  Like fread(3) but with read(3)-style return values.

__C declaration:__ @Fread@

__defined at:__ @rpm\/rpmio.h:35:9@

__exported by:__ @rpm\/rpmio.h@
-}
fread_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> RPM.Types.FD_t -> IO Ssize_t)
fread_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_86d945b810e7d8d0

foreign import ccall unsafe "hs_bindgen_4b604d0b8d2f38d3" hs_bindgen_4b604d0b8d2f38d3 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> RPM.Types.FD_t -> IO Ssize_t))

{-# NOINLINE fwrite_ptr #-}

{-|

  > rpmio

  Like fwrite(3) but with write(3)-style return values.

__C declaration:__ @Fwrite@

__defined at:__ @rpm\/rpmio.h:40:9@

__exported by:__ @rpm\/rpmio.h@
-}
fwrite_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> RPM.Types.FD_t -> IO Ssize_t)
fwrite_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4b604d0b8d2f38d3

foreign import ccall unsafe "hs_bindgen_990e3acf610efd62" hs_bindgen_990e3acf610efd62 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> Off_t -> FC.CInt -> IO FC.CInt))

{-# NOINLINE fseek_ptr #-}

{-|

  > rpmio

  fseek(3) clone.

__C declaration:__ @Fseek@

__defined at:__ @rpm\/rpmio.h:45:5@

__exported by:__ @rpm\/rpmio.h@
-}
fseek_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> Off_t -> FC.CInt -> IO FC.CInt)
fseek_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_990e3acf610efd62

foreign import ccall unsafe "hs_bindgen_3cd682b1666e3f39" hs_bindgen_3cd682b1666e3f39 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> IO Off_t))

{-# NOINLINE ftell_ptr #-}

{-|

  > rpmio

  ftell(3) clone.

__C declaration:__ @Ftell@

__defined at:__ @rpm\/rpmio.h:50:7@

__exported by:__ @rpm\/rpmio.h@
-}
ftell_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> IO Off_t)
ftell_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3cd682b1666e3f39

foreign import ccall unsafe "hs_bindgen_3a8b4d016ce765ec" hs_bindgen_3a8b4d016ce765ec ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> IO FC.CInt))

{-# NOINLINE fclose_ptr #-}

{-|

  > rpmio

  fclose(3) clone.

__C declaration:__ @Fclose@

__defined at:__ @rpm\/rpmio.h:55:5@

__exported by:__ @rpm\/rpmio.h@
-}
fclose_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> IO FC.CInt)
fclose_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3a8b4d016ce765ec

foreign import ccall unsafe "hs_bindgen_28aba02a8e660902" hs_bindgen_28aba02a8e660902 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> (Ptr.Ptr FC.CChar) -> IO RPM.Types.FD_t))

{-# NOINLINE fdopen_ptr #-}

{-|

  > rpmio

  fdopen(3) clone.

  See Fopen() for details.

__C declaration:__ @Fdopen@

__defined at:__ @rpm\/rpmio.h:62:6@

__exported by:__ @rpm\/rpmio.h@
-}
fdopen_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> (Ptr.Ptr FC.CChar) -> IO RPM.Types.FD_t)
fdopen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_28aba02a8e660902

foreign import ccall unsafe "hs_bindgen_ea22dee4eb8d7877" hs_bindgen_ea22dee4eb8d7877 ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO RPM.Types.FD_t))

{-# NOINLINE fopen_ptr #-}

{-|

  > rpmio

  fopen(3) clone with compression support.

  The `fmode` parameter is based on that of `fopen(3)`, but may also include a compression method (`type` and `flags`) to use when opening the stream. See `rpm-payloadflags`(7) manual for details.

__C declaration:__ @Fopen@

__defined at:__ @rpm\/rpmio.h:71:6@

__exported by:__ @rpm\/rpmio.h@
-}
fopen_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO RPM.Types.FD_t)
fopen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ea22dee4eb8d7877

foreign import ccall unsafe "hs_bindgen_59f16f2c9968d876" hs_bindgen_59f16f2c9968d876 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> IO FC.CInt))

{-# NOINLINE fflush_ptr #-}

{-|

  > rpmio

  fflush(3) clone.

__C declaration:__ @Fflush@

__defined at:__ @rpm\/rpmio.h:78:5@

__exported by:__ @rpm\/rpmio.h@
-}
fflush_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> IO FC.CInt)
fflush_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_59f16f2c9968d876

foreign import ccall unsafe "hs_bindgen_faa81cb20f2e2c90" hs_bindgen_faa81cb20f2e2c90 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> IO FC.CInt))

{-# NOINLINE ferror_ptr #-}

{-|

  > rpmio

  ferror(3) clone.

__C declaration:__ @Ferror@

__defined at:__ @rpm\/rpmio.h:83:5@

__exported by:__ @rpm\/rpmio.h@
-}
ferror_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> IO FC.CInt)
ferror_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_faa81cb20f2e2c90

foreign import ccall unsafe "hs_bindgen_851025c50836b507" hs_bindgen_851025c50836b507 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> IO FC.CInt))

{-# NOINLINE fileno_ptr #-}

{-|

  > rpmio

  fileno(3) clone.

__C declaration:__ @Fileno@

__defined at:__ @rpm\/rpmio.h:88:5@

__exported by:__ @rpm\/rpmio.h@
-}
fileno_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> IO FC.CInt)
fileno_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_851025c50836b507

foreign import ccall unsafe "hs_bindgen_38416ab66d4abccd" hs_bindgen_38416ab66d4abccd ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> FC.CInt -> (Ptr.Ptr Void) -> IO FC.CInt))

{-# NOINLINE fcntl_ptr #-}

{-|

  > rpmio

  fcntl(2) clone.

__C declaration:__ @Fcntl@

__defined at:__ @rpm\/rpmio.h:93:5@

__exported by:__ @rpm\/rpmio.h@
-}
fcntl_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> FC.CInt -> (Ptr.Ptr Void) -> IO FC.CInt)
fcntl_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_38416ab66d4abccd

foreign import ccall unsafe "hs_bindgen_7e02840ef8a5b409" hs_bindgen_7e02840ef8a5b409 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE fdescr_ptr #-}

{-|

  > rpmio

  Get informative description (eg file name) from fd for diagnostic output.

__C declaration:__ @Fdescr@

__defined at:__ @rpm\/rpmio.h:98:14@

__exported by:__ @rpm\/rpmio.h@
-}
fdescr_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> IO (Ptr.Ptr FC.CChar))
fdescr_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7e02840ef8a5b409

foreign import ccall unsafe "hs_bindgen_aabdee04f2505a08" hs_bindgen_aabdee04f2505a08 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> IO Off_t))

{-# NOINLINE fdSize_ptr #-}

{-|

  > rpmio

  Return the size of the backing file of the descriptor.

__C declaration:__ @fdSize@

__defined at:__ @rpm\/rpmio.h:107:7@

__exported by:__ @rpm\/rpmio.h@
-}
fdSize_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> IO Off_t)
fdSize_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aabdee04f2505a08

foreign import ccall unsafe "hs_bindgen_b8519bf2143252bd" hs_bindgen_b8519bf2143252bd ::
     IO (Ptr.FunPtr (FC.CInt -> IO RPM.Types.FD_t))

{-# NOINLINE fdDup_ptr #-}

{-|

  > rpmio

  dup(2) clone.

__C declaration:__ @fdDup@

__defined at:__ @rpm\/rpmio.h:112:6@

__exported by:__ @rpm\/rpmio.h@
-}
fdDup_ptr :: Ptr.FunPtr (FC.CInt -> IO RPM.Types.FD_t)
fdDup_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b8519bf2143252bd

foreign import ccall unsafe "hs_bindgen_9d4b09a9b47473e5" hs_bindgen_9d4b09a9b47473e5 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> IO RPM.Types.FD_t))

{-# NOINLINE fdLink_ptr #-}

{-|

  > rpmio

  Reference a file descriptor.

__C declaration:__ @fdLink@

__defined at:__ @rpm\/rpmio.h:117:6@

__exported by:__ @rpm\/rpmio.h@
-}
fdLink_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> IO RPM.Types.FD_t)
fdLink_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9d4b09a9b47473e5

foreign import ccall unsafe "hs_bindgen_394abc86ffec9714" hs_bindgen_394abc86ffec9714 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> IO RPM.Types.FD_t))

{-# NOINLINE fdFree_ptr #-}

{-|

  > rpmio

  Dereference a file descriptor. This does NOT close the file.

__C declaration:__ @fdFree@

__defined at:__ @rpm\/rpmio.h:122:6@

__exported by:__ @rpm\/rpmio.h@
-}
fdFree_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> IO RPM.Types.FD_t)
fdFree_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_394abc86ffec9714

foreign import ccall unsafe "hs_bindgen_e0fb1fb7cc6d6222" hs_bindgen_e0fb1fb7cc6d6222 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> RPM.Types.FD_t -> IO Off_t))

{-# NOINLINE ufdCopy_ptr #-}

{-| Copy file descriptor into another.

__C declaration:__ @ufdCopy@

__defined at:__ @rpm\/rpmio.h:127:7@

__exported by:__ @rpm\/rpmio.h@
-}
ufdCopy_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> RPM.Types.FD_t -> IO Off_t)
ufdCopy_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e0fb1fb7cc6d6222

foreign import ccall unsafe "hs_bindgen_a76438c28f5bc5d7" hs_bindgen_a76438c28f5bc5d7 ::
     IO (Ptr.FunPtr (RPM.Types.FD_t -> FdOpX -> IO RPM.Sw.Rpmop))

{-# NOINLINE fdOp_ptr #-}

{-|

  > rpmio

  File operation statistics.

__C declaration:__ @fdOp@

__defined at:__ @rpm\/rpmio.h:144:7@

__exported by:__ @rpm\/rpmio.h@
-}
fdOp_ptr :: Ptr.FunPtr (RPM.Types.FD_t -> FdOpX -> IO RPM.Sw.Rpmop)
fdOp_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a76438c28f5bc5d7
