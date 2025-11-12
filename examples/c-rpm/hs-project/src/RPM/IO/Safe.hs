{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.IO.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Sw
import qualified RPM.Types
import Data.Void (Void)
import Prelude (IO)
import RPM.IO

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmio.h>"
  , "char const *hs_bindgen_36aa376679e1f69a ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return Fstrerror(arg1);"
  , "}"
  , "ssize_t hs_bindgen_ff829dcb21a59d09 ("
  , "  void *arg1,"
  , "  size_t arg2,"
  , "  size_t arg3,"
  , "  FD_t arg4"
  , ")"
  , "{"
  , "  return Fread(arg1, arg2, arg3, arg4);"
  , "}"
  , "ssize_t hs_bindgen_f12584806ceae41d ("
  , "  void const *arg1,"
  , "  size_t arg2,"
  , "  size_t arg3,"
  , "  FD_t arg4"
  , ")"
  , "{"
  , "  return Fwrite(arg1, arg2, arg3, arg4);"
  , "}"
  , "signed int hs_bindgen_28f8d4ce253a97ca ("
  , "  FD_t arg1,"
  , "  off_t arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return Fseek(arg1, arg2, arg3);"
  , "}"
  , "off_t hs_bindgen_b65a0fbac880817a ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return Ftell(arg1);"
  , "}"
  , "signed int hs_bindgen_f1e120d1d1e372e0 ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return Fclose(arg1);"
  , "}"
  , "FD_t hs_bindgen_3af9e512dd713256 ("
  , "  FD_t arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return Fdopen(arg1, arg2);"
  , "}"
  , "FD_t hs_bindgen_e842a8cca9be3177 ("
  , "  char const *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return Fopen(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_9cbe6badc7a9bbae ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return Fflush(arg1);"
  , "}"
  , "signed int hs_bindgen_9c4cb7e7fb1606aa ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return Ferror(arg1);"
  , "}"
  , "signed int hs_bindgen_22c1e9924d179352 ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return Fileno(arg1);"
  , "}"
  , "signed int hs_bindgen_057a8602427be933 ("
  , "  FD_t arg1,"
  , "  signed int arg2,"
  , "  void *arg3"
  , ")"
  , "{"
  , "  return Fcntl(arg1, arg2, arg3);"
  , "}"
  , "char const *hs_bindgen_4b7dd813312b6e1f ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return Fdescr(arg1);"
  , "}"
  , "off_t hs_bindgen_5f862e72417c2331 ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return fdSize(arg1);"
  , "}"
  , "FD_t hs_bindgen_51365a0c5c529002 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return fdDup(arg1);"
  , "}"
  , "FD_t hs_bindgen_3976f85a90a24813 ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return fdLink(arg1);"
  , "}"
  , "FD_t hs_bindgen_5bebbced5afdff77 ("
  , "  FD_t arg1"
  , ")"
  , "{"
  , "  return fdFree(arg1);"
  , "}"
  , "off_t hs_bindgen_23c85072b420f148 ("
  , "  FD_t arg1,"
  , "  FD_t arg2"
  , ")"
  , "{"
  , "  return ufdCopy(arg1, arg2);"
  , "}"
  , "rpmop hs_bindgen_6ad7e3e6f20ff044 ("
  , "  FD_t arg1,"
  , "  fdOpX arg2"
  , ")"
  , "{"
  , "  return fdOp(arg1, arg2);"
  , "}"
  ]))

{-|

  > rpmio

  strerror(3) clone.

__C declaration:__ @Fstrerror@

__defined at:__ @rpm\/rpmio.h:30:14@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_36aa376679e1f69a" fstrerror ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmio

  Like fread(3) but with read(3)-style return values.

__C declaration:__ @Fread@

__defined at:__ @rpm\/rpmio.h:35:9@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_ff829dcb21a59d09" fread ::
     Ptr.Ptr Void
     {- ^ __C declaration:__ @buf@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @size@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @nmemb@
     -}
  -> RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> IO Ssize_t

{-|

  > rpmio

  Like fwrite(3) but with write(3)-style return values.

__C declaration:__ @Fwrite@

__defined at:__ @rpm\/rpmio.h:40:9@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_f12584806ceae41d" fwrite ::
     Ptr.Ptr Void
     {- ^ __C declaration:__ @buf@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @size@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @nmemb@
     -}
  -> RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> IO Ssize_t

{-|

  > rpmio

  fseek(3) clone.

__C declaration:__ @Fseek@

__defined at:__ @rpm\/rpmio.h:45:5@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_28f8d4ce253a97ca" fseek ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> Off_t
     {- ^ __C declaration:__ @offset@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @whence@
     -}
  -> IO FC.CInt

{-|

  > rpmio

  ftell(3) clone.

__C declaration:__ @Ftell@

__defined at:__ @rpm\/rpmio.h:50:7@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_b65a0fbac880817a" ftell ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> IO Off_t

{-|

  > rpmio

  fclose(3) clone.

__C declaration:__ @Fclose@

__defined at:__ @rpm\/rpmio.h:55:5@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_f1e120d1d1e372e0" fclose ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> IO FC.CInt

{-|

  > rpmio

  fdopen(3) clone.

  See Fopen() for details.

__C declaration:__ @Fdopen@

__defined at:__ @rpm\/rpmio.h:62:6@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_3af9e512dd713256" fdopen ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @ofd@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @fmode@
     -}
  -> IO RPM.Types.FD_t

{-|

  > rpmio

  fopen(3) clone with compression support.

  The `fmode` parameter is based on that of `fopen(3)`, but may also include a compression method (`type` and `flags`) to use when opening the stream. See `rpm-payloadflags`(7) manual for details.

__C declaration:__ @Fopen@

__defined at:__ @rpm\/rpmio.h:71:6@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_e842a8cca9be3177" fopen ::
     Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @path@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @fmode@
     -}
  -> IO RPM.Types.FD_t

{-|

  > rpmio

  fflush(3) clone.

__C declaration:__ @Fflush@

__defined at:__ @rpm\/rpmio.h:78:5@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_9cbe6badc7a9bbae" fflush ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> IO FC.CInt

{-|

  > rpmio

  ferror(3) clone.

__C declaration:__ @Ferror@

__defined at:__ @rpm\/rpmio.h:83:5@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_9c4cb7e7fb1606aa" ferror ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> IO FC.CInt

{-|

  > rpmio

  fileno(3) clone.

__C declaration:__ @Fileno@

__defined at:__ @rpm\/rpmio.h:88:5@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_22c1e9924d179352" fileno ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> IO FC.CInt

{-|

  > rpmio

  fcntl(2) clone.

__C declaration:__ @Fcntl@

__defined at:__ @rpm\/rpmio.h:93:5@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_057a8602427be933" fcntl ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @op@
     -}
  -> Ptr.Ptr Void
     {- ^ __C declaration:__ @lip@
     -}
  -> IO FC.CInt

{-|

  > rpmio

  Get informative description (eg file name) from fd for diagnostic output.

__C declaration:__ @Fdescr@

__defined at:__ @rpm\/rpmio.h:98:14@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_4b7dd813312b6e1f" fdescr ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-|

  > rpmio

  Return the size of the backing file of the descriptor.

__C declaration:__ @fdSize@

__defined at:__ @rpm\/rpmio.h:107:7@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_5f862e72417c2331" fdSize ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> IO Off_t

{-|

  > rpmio

  dup(2) clone.

__C declaration:__ @fdDup@

__defined at:__ @rpm\/rpmio.h:112:6@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_51365a0c5c529002" fdDup ::
     FC.CInt
     {- ^ __C declaration:__ @fdno@
     -}
  -> IO RPM.Types.FD_t

{-|

  > rpmio

  Reference a file descriptor.

__C declaration:__ @fdLink@

__defined at:__ @rpm\/rpmio.h:117:6@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_3976f85a90a24813" fdLink ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> IO RPM.Types.FD_t

{-|

  > rpmio

  Dereference a file descriptor. This does NOT close the file.

__C declaration:__ @fdFree@

__defined at:__ @rpm\/rpmio.h:122:6@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_5bebbced5afdff77" fdFree ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> IO RPM.Types.FD_t

{-| Copy file descriptor into another.

__C declaration:__ @ufdCopy@

__defined at:__ @rpm\/rpmio.h:127:7@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_23c85072b420f148" ufdCopy ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @sfd@
     -}
  -> RPM.Types.FD_t
     {- ^ __C declaration:__ @tfd@
     -}
  -> IO Off_t

{-|

  > rpmio

  File operation statistics.

__C declaration:__ @fdOp@

__defined at:__ @rpm\/rpmio.h:144:7@

__exported by:__ @rpm\/rpmio.h@
-}
foreign import ccall safe "hs_bindgen_6ad7e3e6f20ff044" fdOp ::
     RPM.Types.FD_t
     {- ^ __C declaration:__ @fd@
     -}
  -> FdOpX
     {- ^ __C declaration:__ @opx@
     -}
  -> IO RPM.Sw.Rpmop
