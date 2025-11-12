{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Util.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)
import RPM.Util

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmutil.h>"
  , "/* get_rmalloc_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_b162fe509477bcf5 (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &rmalloc;"
  , "}"
  , "/* get_rcalloc_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_06876c751a0fda77 (void)) ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &rcalloc;"
  , "}"
  , "/* get_rreallocn_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_ebf91bc0179bc77f (void)) ("
  , "  void *arg1,"
  , "  size_t arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &rreallocn;"
  , "}"
  , "/* get_rrealloc_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_d740b21bc83e6369 (void)) ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &rrealloc;"
  , "}"
  , "/* get_rstrdup_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_57c56dfcdef455db (void)) ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return &rstrdup;"
  , "}"
  , "/* get_rstrndup_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_1a063939c5d25451 (void)) ("
  , "  char const *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &rstrndup;"
  , "}"
  , "/* get_rfree_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_c0a7ac6403e9236d (void)) ("
  , "  void *arg1"
  , ")"
  , "{"
  , "  return &rfree;"
  , "}"
  , "/* get_rpmSetMemFail_ptr */"
  , "__attribute__ ((const))"
  , "rpmMemFailFunc (*hs_bindgen_80a9571d86787b92 (void)) ("
  , "  rpmMemFailFunc arg1,"
  , "  void *arg2"
  , ")"
  , "{"
  , "  return &rpmSetMemFail;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_b162fe509477bcf5" hs_bindgen_b162fe509477bcf5 ::
     IO (Ptr.FunPtr (HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr Void)))

{-# NOINLINE rmalloc_ptr #-}

{-| __C declaration:__ @rmalloc@

    __defined at:__ @rpm\/rpmutil.h:122:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
rmalloc_ptr :: Ptr.FunPtr (HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr Void))
rmalloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b162fe509477bcf5

foreign import ccall unsafe "hs_bindgen_06876c751a0fda77" hs_bindgen_06876c751a0fda77 ::
     IO (Ptr.FunPtr (HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr Void)))

{-# NOINLINE rcalloc_ptr #-}

{-| __C declaration:__ @rcalloc@

    __defined at:__ @rpm\/rpmutil.h:125:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
rcalloc_ptr :: Ptr.FunPtr (HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr Void))
rcalloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_06876c751a0fda77

foreign import ccall unsafe "hs_bindgen_ebf91bc0179bc77f" hs_bindgen_ebf91bc0179bc77f ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr Void)))

{-# NOINLINE rreallocn_ptr #-}

{-| __C declaration:__ @rreallocn@

    __defined at:__ @rpm\/rpmutil.h:129:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
rreallocn_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr Void))
rreallocn_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ebf91bc0179bc77f

foreign import ccall unsafe "hs_bindgen_d740b21bc83e6369" hs_bindgen_d740b21bc83e6369 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr Void)))

{-# NOINLINE rrealloc_ptr #-}

{-| __C declaration:__ @rrealloc@

    __defined at:__ @rpm\/rpmutil.h:132:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
rrealloc_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr Void))
rrealloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d740b21bc83e6369

foreign import ccall unsafe "hs_bindgen_57c56dfcdef455db" hs_bindgen_57c56dfcdef455db ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rstrdup_ptr #-}

{-| __C declaration:__ @rstrdup@

    __defined at:__ @rpm\/rpmutil.h:134:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
rstrdup_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FC.CChar))
rstrdup_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_57c56dfcdef455db

foreign import ccall unsafe "hs_bindgen_1a063939c5d25451" hs_bindgen_1a063939c5d25451 ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE rstrndup_ptr #-}

{-| __C declaration:__ @rstrndup@

    __defined at:__ @rpm\/rpmutil.h:136:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
rstrndup_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr FC.CChar))
rstrndup_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1a063939c5d25451

foreign import ccall unsafe "hs_bindgen_c0a7ac6403e9236d" hs_bindgen_c0a7ac6403e9236d ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> IO (Ptr.Ptr Void)))

{-# NOINLINE rfree_ptr #-}

{-| __C declaration:__ @rfree@

    __defined at:__ @rpm\/rpmutil.h:139:8@

    __exported by:__ @rpm\/rpmutil.h@
-}
rfree_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> IO (Ptr.Ptr Void))
rfree_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c0a7ac6403e9236d

foreign import ccall unsafe "hs_bindgen_80a9571d86787b92" hs_bindgen_80a9571d86787b92 ::
     IO (Ptr.FunPtr (RpmMemFailFunc -> (Ptr.Ptr Void) -> IO RpmMemFailFunc))

{-# NOINLINE rpmSetMemFail_ptr #-}

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
rpmSetMemFail_ptr :: Ptr.FunPtr (RpmMemFailFunc -> (Ptr.Ptr Void) -> IO RpmMemFailFunc)
rpmSetMemFail_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_80a9571d86787b92
