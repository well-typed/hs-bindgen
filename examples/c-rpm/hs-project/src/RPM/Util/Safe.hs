{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Util.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)
import RPM.Util

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmutil.h>"
  , "void *hs_bindgen_46a80783720b5712 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return rmalloc(arg1);"
  , "}"
  , "void *hs_bindgen_2dc4fa5a7d8b8c6f ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return rcalloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_dd73c01d22a656bc ("
  , "  void *arg1,"
  , "  size_t arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return rreallocn(arg1, arg2, arg3);"
  , "}"
  , "void *hs_bindgen_db9473f402aa2f1c ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return rrealloc(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_cf9d8d8d552e6c4a ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return rstrdup(arg1);"
  , "}"
  , "char *hs_bindgen_1c0b4a12bfac4db6 ("
  , "  char const *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return rstrndup(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_7efc11846dc0f835 ("
  , "  void *arg1"
  , ")"
  , "{"
  , "  return rfree(arg1);"
  , "}"
  , "rpmMemFailFunc hs_bindgen_a1426ba832382013 ("
  , "  rpmMemFailFunc arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return rpmSetMemFail(arg1, arg2);"
  , "}"
  ]))

{-| __C declaration:__ @rmalloc@

    __defined at:__ @rpm\/rpmutil.h:122:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
foreign import ccall safe "hs_bindgen_46a80783720b5712" rmalloc ::
     HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @size@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @rcalloc@

    __defined at:__ @rpm\/rpmutil.h:125:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
foreign import ccall safe "hs_bindgen_2dc4fa5a7d8b8c6f" rcalloc ::
     HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @nmemb@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @size@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @rreallocn@

    __defined at:__ @rpm\/rpmutil.h:129:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
foreign import ccall safe "hs_bindgen_dd73c01d22a656bc" rreallocn ::
     Ptr.Ptr Void
     {- ^ __C declaration:__ @ptr@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @nmemb@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @size@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @rrealloc@

    __defined at:__ @rpm\/rpmutil.h:132:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
foreign import ccall safe "hs_bindgen_db9473f402aa2f1c" rrealloc ::
     Ptr.Ptr Void
     {- ^ __C declaration:__ @ptr@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @size@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @rstrdup@

    __defined at:__ @rpm\/rpmutil.h:134:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
foreign import ccall safe "hs_bindgen_cf9d8d8d552e6c4a" rstrdup ::
     Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @str@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @rstrndup@

    __defined at:__ @rpm\/rpmutil.h:136:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
foreign import ccall safe "hs_bindgen_1c0b4a12bfac4db6" rstrndup ::
     Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @str@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @n@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @rfree@

    __defined at:__ @rpm\/rpmutil.h:139:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
foreign import ccall safe "hs_bindgen_7efc11846dc0f835" rfree ::
     Ptr.Ptr Void
     {- ^ __C declaration:__ @ptr@
     -}
  -> IO (Ptr.Ptr Void)

{-|

  > rpmutil

  Set memory allocation failure callback.

  [__@func@ /(input)/__]: Allocation failure callback function

  [__@data@ /(input)/__]: User data (or NULL)

  __returns:__ Previous callback function

__C declaration:__ @rpmSetMemFail@

__defined at:__ @rpm\/rpmutil.h:160:16@

__exported by:__ @rpm\/rpmutil.h@
-}
foreign import ccall safe "hs_bindgen_a1426ba832382013" rpmSetMemFail ::
     RpmMemFailFunc
     {- ^

        [__@func@ /(input)/__]: Allocation failure callback function

     __C declaration:__ @func@
     -}
  -> Ptr.Ptr Void
     {- ^ __C declaration:__ @data'@
     -}
  -> IO RpmMemFailFunc
