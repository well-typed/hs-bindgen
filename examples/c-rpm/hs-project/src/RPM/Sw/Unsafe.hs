{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Sw.Unsafe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)
import RPM.Sw

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmsw.h>"
  , "rpmsw hs_bindgen_b16f602d562b7329 ("
  , "  rpmsw arg1"
  , ")"
  , "{"
  , "  return rpmswNow(arg1);"
  , "}"
  , "rpmtime_t hs_bindgen_55ff8321cbff294f ("
  , "  rpmsw arg1,"
  , "  rpmsw arg2"
  , ")"
  , "{"
  , "  return rpmswDiff(arg1, arg2);"
  , "}"
  , "rpmtime_t hs_bindgen_dbcf7e809bec060b (void)"
  , "{"
  , "  return rpmswInit();"
  , "}"
  , "signed int hs_bindgen_525b52ee538dc46a ("
  , "  rpmop arg1,"
  , "  ssize_t arg2"
  , ")"
  , "{"
  , "  return rpmswEnter(arg1, arg2);"
  , "}"
  , "rpmtime_t hs_bindgen_746f4d2fc6f53703 ("
  , "  rpmop arg1,"
  , "  ssize_t arg2"
  , ")"
  , "{"
  , "  return rpmswExit(arg1, arg2);"
  , "}"
  , "rpmtime_t hs_bindgen_d7bf81d7a30d781e ("
  , "  rpmop arg1,"
  , "  rpmop arg2"
  , ")"
  , "{"
  , "  return rpmswAdd(arg1, arg2);"
  , "}"
  , "rpmtime_t hs_bindgen_8a22cf7ed5bee296 ("
  , "  rpmop arg1,"
  , "  rpmop arg2"
  , ")"
  , "{"
  , "  return rpmswSub(arg1, arg2);"
  , "}"
  ]))

{-|

  > rpmsw

  Return benchmark time stamp.

  [__@*sw@ /(input)/__]: time stamp

  __returns:__ 0 on success

__C declaration:__ @rpmswNow@

__defined at:__ @rpm\/rpmsw.h:54:7@

__exported by:__ @rpm\/rpmsw.h@
-}
foreign import ccall unsafe "hs_bindgen_b16f602d562b7329" rpmswNow ::
     Rpmsw
     {- ^ __C declaration:__ @sw@
     -}
  -> IO Rpmsw

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
foreign import ccall unsafe "hs_bindgen_55ff8321cbff294f" rpmswDiff ::
     Rpmsw
     {- ^ __C declaration:__ @end@
     -}
  -> Rpmsw
     {- ^ __C declaration:__ @begin@
     -}
  -> IO Rpmtime_t

{-|

  > rpmsw

  Return benchmark time stamp overhead.

  __returns:__ overhead in micro-seconds

__C declaration:__ @rpmswInit@

__defined at:__ @rpm\/rpmsw.h:68:11@

__exported by:__ @rpm\/rpmsw.h@
-}
foreign import ccall unsafe "hs_bindgen_dbcf7e809bec060b" rpmswInit ::
     IO Rpmtime_t

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
foreign import ccall unsafe "hs_bindgen_525b52ee538dc46a" rpmswEnter ::
     Rpmop
     {- ^

        [__@op@ /(input)/__]: operation statistics

     __C declaration:__ @op@
     -}
  -> Ssize_t
     {- ^

        [__@rc@ /(input)/__]: -1 clears usec counter

     __C declaration:__ @rc@
     -}
  -> IO FC.CInt

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
foreign import ccall unsafe "hs_bindgen_746f4d2fc6f53703" rpmswExit ::
     Rpmop
     {- ^

        [__@op@ /(input)/__]: operation statistics

     __C declaration:__ @op@
     -}
  -> Ssize_t
     {- ^

        [__@rc@ /(input)/__]: per-operation data (e.g. bytes transferred)

     __C declaration:__ @rc@
     -}
  -> IO Rpmtime_t

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
foreign import ccall unsafe "hs_bindgen_d7bf81d7a30d781e" rpmswAdd ::
     Rpmop
     {- ^

        [__@to@ /(input)/__]: result statistics

     __C declaration:__ @to@
     -}
  -> Rpmop
     {- ^

        [__@from@ /(input)/__]: operation statistics

     __C declaration:__ @from@
     -}
  -> IO Rpmtime_t

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
foreign import ccall unsafe "hs_bindgen_8a22cf7ed5bee296" rpmswSub ::
     Rpmop
     {- ^

        [__@to@ /(input)/__]: result statistics

     __C declaration:__ @to@
     -}
  -> Rpmop
     {- ^

        [__@from@ /(input)/__]: operation statistics

     __C declaration:__ @from@
     -}
  -> IO Rpmtime_t
