{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Ps.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Prob
import qualified RPM.Types
import Prelude (IO)
import RPM.Ps

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmps.h>"
  , "rpmps hs_bindgen_d0e4bb53175a16d5 ("
  , "  rpmps arg1"
  , ")"
  , "{"
  , "  return rpmpsLink(arg1);"
  , "}"
  , "signed int hs_bindgen_e3d78529a3cdf49a ("
  , "  rpmps arg1"
  , ")"
  , "{"
  , "  return rpmpsNumProblems(arg1);"
  , "}"
  , "rpmpsi hs_bindgen_51c3c723c9e061f8 ("
  , "  rpmps arg1"
  , ")"
  , "{"
  , "  return rpmpsInitIterator(arg1);"
  , "}"
  , "rpmpsi hs_bindgen_da18a828201e4438 ("
  , "  rpmpsi arg1"
  , ")"
  , "{"
  , "  return rpmpsFreeIterator(arg1);"
  , "}"
  , "rpmProblem hs_bindgen_09091ead849cd662 ("
  , "  rpmpsi arg1"
  , ")"
  , "{"
  , "  return rpmpsiNext(arg1);"
  , "}"
  , "signed int hs_bindgen_c9b1ee2ea9738c83 ("
  , "  rpmpsi arg1"
  , ")"
  , "{"
  , "  return rpmpsNextIterator(arg1);"
  , "}"
  , "rpmProblem hs_bindgen_2b427e3487432896 ("
  , "  rpmpsi arg1"
  , ")"
  , "{"
  , "  return rpmpsGetProblem(arg1);"
  , "}"
  , "rpmps hs_bindgen_f7e725c147660e52 (void)"
  , "{"
  , "  return rpmpsCreate();"
  , "}"
  , "rpmps hs_bindgen_7e991da65bcabdac ("
  , "  rpmps arg1"
  , ")"
  , "{"
  , "  return rpmpsFree(arg1);"
  , "}"
  , "void hs_bindgen_edc9859d081a136f ("
  , "  FILE *arg1,"
  , "  rpmps arg2"
  , ")"
  , "{"
  , "  rpmpsPrint(arg1, arg2);"
  , "}"
  , "void hs_bindgen_cda788334bb4d83a ("
  , "  rpmps arg1,"
  , "  rpmProblem arg2"
  , ")"
  , "{"
  , "  rpmpsAppendProblem(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_c4baa83d88e3cbf6 ("
  , "  rpmps arg1,"
  , "  rpmps arg2"
  , ")"
  , "{"
  , "  return rpmpsMerge(arg1, arg2);"
  , "}"
  ]))

{-|

  > rpmps

  Reference a problem set instance.

  [__@ps@ /(input)/__]: transaction set

  __returns:__ new transaction set reference

__C declaration:__ @rpmpsLink@

__defined at:__ @rpm\/rpmps.h:27:7@

__exported by:__ @rpm\/rpmps.h@
-}
foreign import ccall safe "hs_bindgen_d0e4bb53175a16d5" rpmpsLink ::
     RPM.Types.Rpmps
     {- ^

        [__@ps@ /(input)/__]: transaction set

     __C declaration:__ @ps@
     -}
  -> IO RPM.Types.Rpmps

{-|

  > rpmps

  Return number of problems in set.

  [__@ps@ /(input)/__]: problem set

  __returns:__ number of problems

__C declaration:__ @rpmpsNumProblems@

__defined at:__ @rpm\/rpmps.h:34:5@

__exported by:__ @rpm\/rpmps.h@
-}
foreign import ccall safe "hs_bindgen_e3d78529a3cdf49a" rpmpsNumProblems ::
     RPM.Types.Rpmps
     {- ^

        [__@ps@ /(input)/__]: problem set

     __C declaration:__ @ps@
     -}
  -> IO FC.CInt

{-|

  > rpmps

  Initialize problem set iterator.

  [__@ps@ /(input)/__]: problem set

  __returns:__ problem set iterator

__C declaration:__ @rpmpsInitIterator@

__defined at:__ @rpm\/rpmps.h:41:8@

__exported by:__ @rpm\/rpmps.h@
-}
foreign import ccall safe "hs_bindgen_51c3c723c9e061f8" rpmpsInitIterator ::
     RPM.Types.Rpmps
     {- ^

        [__@ps@ /(input)/__]: problem set

     __C declaration:__ @ps@
     -}
  -> IO Rpmpsi

{-|

  > rpmps

  Destroy problem set iterator.

  [__@psi@ /(input)/__]: problem set iterator

  __returns:__ problem set iterator (NULL)

__C declaration:__ @rpmpsFreeIterator@

__defined at:__ @rpm\/rpmps.h:48:8@

__exported by:__ @rpm\/rpmps.h@
-}
foreign import ccall safe "hs_bindgen_da18a828201e4438" rpmpsFreeIterator ::
     Rpmpsi
     {- ^

        [__@psi@ /(input)/__]: problem set iterator

     __C declaration:__ @psi@
     -}
  -> IO Rpmpsi

{-|

  > rpmps

  Return next problem from iterator

  [__@psi@ /(input)/__]: problem set iterator

  __returns:__ next problem (weak ref), NULL on termination

__C declaration:__ @rpmpsiNext@

__defined at:__ @rpm\/rpmps.h:55:12@

__exported by:__ @rpm\/rpmps.h@
-}
foreign import ccall safe "hs_bindgen_09091ead849cd662" rpmpsiNext ::
     Rpmpsi
     {- ^

        [__@psi@ /(input)/__]: problem set iterator

     __C declaration:__ @psi@
     -}
  -> IO RPM.Prob.RpmProblem

{-|

  > rpmps

  Return next problem set iterator index

  [__@psi@ /(input)/__]: problem set iterator

  __returns:__ iterator index, -1 on termination

__C declaration:__ @rpmpsNextIterator@

__defined at:__ @rpm\/rpmps.h:62:5@

__exported by:__ @rpm\/rpmps.h@
-}
foreign import ccall safe "hs_bindgen_c9b1ee2ea9738c83" rpmpsNextIterator ::
     Rpmpsi
     {- ^

        [__@psi@ /(input)/__]: problem set iterator

     __C declaration:__ @psi@
     -}
  -> IO FC.CInt

{-|

  > rpmps

  Return current problem from problem set

  [__@psi@ /(input)/__]: problem set iterator

  __returns:__ current rpmProblem

__C declaration:__ @rpmpsGetProblem@

__defined at:__ @rpm\/rpmps.h:69:12@

__exported by:__ @rpm\/rpmps.h@
-}
foreign import ccall safe "hs_bindgen_2b427e3487432896" rpmpsGetProblem ::
     Rpmpsi
     {- ^

        [__@psi@ /(input)/__]: problem set iterator

     __C declaration:__ @psi@
     -}
  -> IO RPM.Prob.RpmProblem

{-|

  > rpmps

  Create a problem set.

  __returns:__ new problem set

__C declaration:__ @rpmpsCreate@

__defined at:__ @rpm\/rpmps.h:75:7@

__exported by:__ @rpm\/rpmps.h@
-}
foreign import ccall safe "hs_bindgen_f7e725c147660e52" rpmpsCreate ::
     IO RPM.Types.Rpmps

{-|

  > rpmps

  Destroy a problem set.

  [__@ps@ /(input)/__]: problem set

  __returns:__ NULL always

__C declaration:__ @rpmpsFree@

__defined at:__ @rpm\/rpmps.h:82:7@

__exported by:__ @rpm\/rpmps.h@
-}
foreign import ccall safe "hs_bindgen_7e991da65bcabdac" rpmpsFree ::
     RPM.Types.Rpmps
     {- ^

        [__@ps@ /(input)/__]: problem set

     __C declaration:__ @ps@
     -}
  -> IO RPM.Types.Rpmps

{-|

  > rpmps

  Print problems to file handle.

  [__@fp@ /(input)/__]: file handle (NULL uses stderr)

  [__@ps@ /(input)/__]: problem set

__C declaration:__ @rpmpsPrint@

__defined at:__ @rpm\/rpmps.h:89:6@

__exported by:__ @rpm\/rpmps.h@
-}
foreign import ccall safe "hs_bindgen_edc9859d081a136f" rpmpsPrint ::
     Ptr.Ptr HsBindgen.Runtime.Prelude.CFile
     {- ^

        [__@fp@ /(input)/__]: file handle (NULL uses stderr)

     __C declaration:__ @fp@
     -}
  -> RPM.Types.Rpmps
     {- ^

        [__@ps@ /(input)/__]: problem set

     __C declaration:__ @ps@
     -}
  -> IO ()

{-|

  > rpmps

  Append a problem to current set of problems.

  [__@ps@ /(input)/__]: problem set

  [__@prob@ /(input)/__]: rpmProblem

__C declaration:__ @rpmpsAppendProblem@

__defined at:__ @rpm\/rpmps.h:96:6@

__exported by:__ @rpm\/rpmps.h@
-}
foreign import ccall safe "hs_bindgen_cda788334bb4d83a" rpmpsAppendProblem ::
     RPM.Types.Rpmps
     {- ^

        [__@ps@ /(input)/__]: problem set

     __C declaration:__ @ps@
     -}
  -> RPM.Prob.RpmProblem
     {- ^

        [__@prob@ /(input)/__]: rpmProblem

     __C declaration:__ @prob@
     -}
  -> IO ()

{-|

  > rpmps

  Merge problem set into another.

  [__@dest@ /(input)/__]: destination problem set

  [__@src@ /(input)/__]: source problem set

  __returns:__ number of problems merged

__C declaration:__ @rpmpsMerge@

__defined at:__ @rpm\/rpmps.h:104:5@

__exported by:__ @rpm\/rpmps.h@
-}
foreign import ccall safe "hs_bindgen_c4baa83d88e3cbf6" rpmpsMerge ::
     RPM.Types.Rpmps
     {- ^

        [__@dest@ /(input)/__]: destination problem set

     __C declaration:__ @dest@
     -}
  -> RPM.Types.Rpmps
     {- ^

        [__@src@ /(input)/__]: source problem set

     __C declaration:__ @src@
     -}
  -> IO FC.CInt
