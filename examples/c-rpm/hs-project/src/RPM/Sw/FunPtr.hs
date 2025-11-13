{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Sw.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)
import RPM.Sw

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmsw.h>"
  , "/* get_rpmswNow_ptr */"
  , "__attribute__ ((const))"
  , "rpmsw (*hs_bindgen_5eb6c8b44abffbf8 (void)) ("
  , "  rpmsw arg1"
  , ")"
  , "{"
  , "  return &rpmswNow;"
  , "}"
  , "/* get_rpmswDiff_ptr */"
  , "__attribute__ ((const))"
  , "rpmtime_t (*hs_bindgen_57b67f9b73111ebf (void)) ("
  , "  rpmsw arg1,"
  , "  rpmsw arg2"
  , ")"
  , "{"
  , "  return &rpmswDiff;"
  , "}"
  , "/* get_rpmswInit_ptr */"
  , "__attribute__ ((const))"
  , "rpmtime_t (*hs_bindgen_f8336d2fd4534924 (void)) (void)"
  , "{"
  , "  return &rpmswInit;"
  , "}"
  , "/* get_rpmswEnter_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_1d3ae3371e473cf7 (void)) ("
  , "  rpmop arg1,"
  , "  ssize_t arg2"
  , ")"
  , "{"
  , "  return &rpmswEnter;"
  , "}"
  , "/* get_rpmswExit_ptr */"
  , "__attribute__ ((const))"
  , "rpmtime_t (*hs_bindgen_8a356cce3843b7fb (void)) ("
  , "  rpmop arg1,"
  , "  ssize_t arg2"
  , ")"
  , "{"
  , "  return &rpmswExit;"
  , "}"
  , "/* get_rpmswAdd_ptr */"
  , "__attribute__ ((const))"
  , "rpmtime_t (*hs_bindgen_c88b6675bac6cda7 (void)) ("
  , "  rpmop arg1,"
  , "  rpmop arg2"
  , ")"
  , "{"
  , "  return &rpmswAdd;"
  , "}"
  , "/* get_rpmswSub_ptr */"
  , "__attribute__ ((const))"
  , "rpmtime_t (*hs_bindgen_47277bdf25481370 (void)) ("
  , "  rpmop arg1,"
  , "  rpmop arg2"
  , ")"
  , "{"
  , "  return &rpmswSub;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_5eb6c8b44abffbf8" hs_bindgen_5eb6c8b44abffbf8 ::
     IO (Ptr.FunPtr (Rpmsw -> IO Rpmsw))

{-# NOINLINE rpmswNow_ptr #-}

{-|

  > rpmsw

  Return benchmark time stamp.

  [__@*sw@ /(input)/__]: time stamp

  __returns:__ 0 on success

__C declaration:__ @rpmswNow@

__defined at:__ @rpm\/rpmsw.h:54:7@

__exported by:__ @rpm\/rpmsw.h@
-}
rpmswNow_ptr :: Ptr.FunPtr (Rpmsw -> IO Rpmsw)
rpmswNow_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5eb6c8b44abffbf8

foreign import ccall unsafe "hs_bindgen_57b67f9b73111ebf" hs_bindgen_57b67f9b73111ebf ::
     IO (Ptr.FunPtr (Rpmsw -> Rpmsw -> IO Rpmtime_t))

{-# NOINLINE rpmswDiff_ptr #-}

{-|

  > rpmsw

  Return benchmark time stamp difference.

  [__@*end@ /(input)/__]: end time stamp

  [__@*begin@ /(input)/__]: begin time stamp

  __returns:__ difference in micro-seconds

__C declaration:__ @rpmswDiff@

__defined at:__ @rpm\/rpmsw.h:62:11@

__exported by:__ @rpm\/rpmsw.h@
-}
rpmswDiff_ptr :: Ptr.FunPtr (Rpmsw -> Rpmsw -> IO Rpmtime_t)
rpmswDiff_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_57b67f9b73111ebf

foreign import ccall unsafe "hs_bindgen_f8336d2fd4534924" hs_bindgen_f8336d2fd4534924 ::
     IO (Ptr.FunPtr (IO Rpmtime_t))

{-# NOINLINE rpmswInit_ptr #-}

{-|

  > rpmsw

  Return benchmark time stamp overhead.

  __returns:__ overhead in micro-seconds

__C declaration:__ @rpmswInit@

__defined at:__ @rpm\/rpmsw.h:68:11@

__exported by:__ @rpm\/rpmsw.h@
-}
rpmswInit_ptr :: Ptr.FunPtr (IO Rpmtime_t)
rpmswInit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f8336d2fd4534924

foreign import ccall unsafe "hs_bindgen_1d3ae3371e473cf7" hs_bindgen_1d3ae3371e473cf7 ::
     IO (Ptr.FunPtr (Rpmop -> Ssize_t -> IO FC.CInt))

{-# NOINLINE rpmswEnter_ptr #-}

{-|

  > rpmsw

  Enter timed operation.

  [__@op@ /(input)/__]: operation statistics

  [__@rc@ /(input)/__]: -1 clears usec counter

  __returns:__ 0 always

__C declaration:__ @rpmswEnter@

__defined at:__ @rpm\/rpmsw.h:76:5@

__exported by:__ @rpm\/rpmsw.h@
-}
rpmswEnter_ptr :: Ptr.FunPtr (Rpmop -> Ssize_t -> IO FC.CInt)
rpmswEnter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1d3ae3371e473cf7

foreign import ccall unsafe "hs_bindgen_8a356cce3843b7fb" hs_bindgen_8a356cce3843b7fb ::
     IO (Ptr.FunPtr (Rpmop -> Ssize_t -> IO Rpmtime_t))

{-# NOINLINE rpmswExit_ptr #-}

{-|

  > rpmsw

  Exit timed operation.

  [__@op@ /(input)/__]: operation statistics

  [__@rc@ /(input)/__]: per-operation data (e.g. bytes transferred)

  __returns:__ cumulative usecs for operation

__C declaration:__ @rpmswExit@

__defined at:__ @rpm\/rpmsw.h:84:11@

__exported by:__ @rpm\/rpmsw.h@
-}
rpmswExit_ptr :: Ptr.FunPtr (Rpmop -> Ssize_t -> IO Rpmtime_t)
rpmswExit_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8a356cce3843b7fb

foreign import ccall unsafe "hs_bindgen_c88b6675bac6cda7" hs_bindgen_c88b6675bac6cda7 ::
     IO (Ptr.FunPtr (Rpmop -> Rpmop -> IO Rpmtime_t))

{-# NOINLINE rpmswAdd_ptr #-}

{-|

  > rpmsw

  Sum statistic counters.

  [__@to@ /(input)/__]: result statistics

  [__@from@ /(input)/__]: operation statistics

  __returns:__ cumulative usecs for operation

__C declaration:__ @rpmswAdd@

__defined at:__ @rpm\/rpmsw.h:92:11@

__exported by:__ @rpm\/rpmsw.h@
-}
rpmswAdd_ptr :: Ptr.FunPtr (Rpmop -> Rpmop -> IO Rpmtime_t)
rpmswAdd_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c88b6675bac6cda7

foreign import ccall unsafe "hs_bindgen_47277bdf25481370" hs_bindgen_47277bdf25481370 ::
     IO (Ptr.FunPtr (Rpmop -> Rpmop -> IO Rpmtime_t))

{-# NOINLINE rpmswSub_ptr #-}

{-|

  > rpmsw

  Subtract statistic counters.

  [__@to@ /(input)/__]: result statistics

  [__@from@ /(input)/__]: operation statistics

  __returns:__ cumulative usecs for operation

__C declaration:__ @rpmswSub@

__defined at:__ @rpm\/rpmsw.h:100:11@

__exported by:__ @rpm\/rpmsw.h@
-}
rpmswSub_ptr :: Ptr.FunPtr (Rpmop -> Rpmop -> IO Rpmtime_t)
rpmswSub_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_47277bdf25481370
