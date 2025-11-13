{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Util.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)
import RPM.Util

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmutil.h>"
  , "void *hs_bindgen_0fc0353d02c19b09 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return rmalloc(arg1);"
  , "}"
  , "void *hs_bindgen_993645c5c555b6dd ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return rcalloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_996bac89f4fa4aaf ("
  , "  void *arg1,"
  , "  size_t arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return rreallocn(arg1, arg2, arg3);"
  , "}"
  , "void *hs_bindgen_cb393b3bf1a1993b ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return rrealloc(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_e10c8419ebe9d4e5 ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return rstrdup(arg1);"
  , "}"
  , "char *hs_bindgen_49afd9963f26bcb3 ("
  , "  char const *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return rstrndup(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_a4e1377394235b0a ("
  , "  void *arg1"
  , ")"
  , "{"
  , "  return rfree(arg1);"
  , "}"
  , "rpmMemFailFunc hs_bindgen_7dab4ec9c085f3a3 ("
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
foreign import ccall unsafe "hs_bindgen_0fc0353d02c19b09" rmalloc ::
     HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @size@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @rcalloc@

    __defined at:__ @rpm\/rpmutil.h:125:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
foreign import ccall unsafe "hs_bindgen_993645c5c555b6dd" rcalloc ::
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
foreign import ccall unsafe "hs_bindgen_996bac89f4fa4aaf" rreallocn ::
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
foreign import ccall unsafe "hs_bindgen_cb393b3bf1a1993b" rrealloc ::
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
foreign import ccall unsafe "hs_bindgen_e10c8419ebe9d4e5" rstrdup ::
     Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @str@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @rstrndup@

    __defined at:__ @rpm\/rpmutil.h:136:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
foreign import ccall unsafe "hs_bindgen_49afd9963f26bcb3" rstrndup ::
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
foreign import ccall unsafe "hs_bindgen_a4e1377394235b0a" rfree ::
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
foreign import ccall unsafe "hs_bindgen_7dab4ec9c085f3a3" rpmSetMemFail ::
     RpmMemFailFunc
     {- ^

        [__@func@ /(input)/__]: Allocation failure callback function

     __C declaration:__ @func@
     -}
  -> Ptr.Ptr Void
     {- ^ __C declaration:__ @data'@
     -}
  -> IO RpmMemFailFunc
