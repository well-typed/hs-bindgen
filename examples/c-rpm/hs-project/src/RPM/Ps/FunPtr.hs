{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module RPM.Ps.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified RPM.Prob
import qualified RPM.Types
import Prelude (IO)
import RPM.Ps

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <rpm/rpmps.h>"
  , "/* get_rpmpsLink_ptr */"
  , "__attribute__ ((const))"
  , "rpmps (*hs_bindgen_38b91bdbf4b9def1 (void)) ("
  , "  rpmps arg1"
  , ")"
  , "{"
  , "  return &rpmpsLink;"
  , "}"
  , "/* get_rpmpsNumProblems_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_19480baf5363711a (void)) ("
  , "  rpmps arg1"
  , ")"
  , "{"
  , "  return &rpmpsNumProblems;"
  , "}"
  , "/* get_rpmpsInitIterator_ptr */"
  , "__attribute__ ((const))"
  , "rpmpsi (*hs_bindgen_da72d497ae79b9b2 (void)) ("
  , "  rpmps arg1"
  , ")"
  , "{"
  , "  return &rpmpsInitIterator;"
  , "}"
  , "/* get_rpmpsFreeIterator_ptr */"
  , "__attribute__ ((const))"
  , "rpmpsi (*hs_bindgen_f7c314d0fbec44ad (void)) ("
  , "  rpmpsi arg1"
  , ")"
  , "{"
  , "  return &rpmpsFreeIterator;"
  , "}"
  , "/* get_rpmpsiNext_ptr */"
  , "__attribute__ ((const))"
  , "rpmProblem (*hs_bindgen_688f225ba40e6c17 (void)) ("
  , "  rpmpsi arg1"
  , ")"
  , "{"
  , "  return &rpmpsiNext;"
  , "}"
  , "/* get_rpmpsNextIterator_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a0e86a6b6055cbb3 (void)) ("
  , "  rpmpsi arg1"
  , ")"
  , "{"
  , "  return &rpmpsNextIterator;"
  , "}"
  , "/* get_rpmpsGetProblem_ptr */"
  , "__attribute__ ((const))"
  , "rpmProblem (*hs_bindgen_5e89df94546d00ef (void)) ("
  , "  rpmpsi arg1"
  , ")"
  , "{"
  , "  return &rpmpsGetProblem;"
  , "}"
  , "/* get_rpmpsCreate_ptr */"
  , "__attribute__ ((const))"
  , "rpmps (*hs_bindgen_e9b21becc9587fd0 (void)) (void)"
  , "{"
  , "  return &rpmpsCreate;"
  , "}"
  , "/* get_rpmpsFree_ptr */"
  , "__attribute__ ((const))"
  , "rpmps (*hs_bindgen_3765d22bca1e3686 (void)) ("
  , "  rpmps arg1"
  , ")"
  , "{"
  , "  return &rpmpsFree;"
  , "}"
  , "/* get_rpmpsPrint_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6c16567724913531 (void)) ("
  , "  FILE *arg1,"
  , "  rpmps arg2"
  , ")"
  , "{"
  , "  return &rpmpsPrint;"
  , "}"
  , "/* get_rpmpsAppendProblem_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f8e4cff7d3bab99b (void)) ("
  , "  rpmps arg1,"
  , "  rpmProblem arg2"
  , ")"
  , "{"
  , "  return &rpmpsAppendProblem;"
  , "}"
  , "/* get_rpmpsMerge_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9718ffe3da6fdf38 (void)) ("
  , "  rpmps arg1,"
  , "  rpmps arg2"
  , ")"
  , "{"
  , "  return &rpmpsMerge;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_38b91bdbf4b9def1" hs_bindgen_38b91bdbf4b9def1 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmps -> IO RPM.Types.Rpmps))

{-# NOINLINE rpmpsLink_ptr #-}

{-|

  > rpmps

  Reference a problem set instance.

  [__@ps@ /(input)/__]: transaction set

  __returns:__ new transaction set reference

__C declaration:__ @rpmpsLink@

__defined at:__ @rpm\/rpmps.h:27:7@

__exported by:__ @rpm\/rpmps.h@
-}
rpmpsLink_ptr :: Ptr.FunPtr (RPM.Types.Rpmps -> IO RPM.Types.Rpmps)
rpmpsLink_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_38b91bdbf4b9def1

foreign import ccall unsafe "hs_bindgen_19480baf5363711a" hs_bindgen_19480baf5363711a ::
     IO (Ptr.FunPtr (RPM.Types.Rpmps -> IO FC.CInt))

{-# NOINLINE rpmpsNumProblems_ptr #-}

{-|

  > rpmps

  Return number of problems in set.

  [__@ps@ /(input)/__]: problem set

  __returns:__ number of problems

__C declaration:__ @rpmpsNumProblems@

__defined at:__ @rpm\/rpmps.h:34:5@

__exported by:__ @rpm\/rpmps.h@
-}
rpmpsNumProblems_ptr :: Ptr.FunPtr (RPM.Types.Rpmps -> IO FC.CInt)
rpmpsNumProblems_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_19480baf5363711a

foreign import ccall unsafe "hs_bindgen_da72d497ae79b9b2" hs_bindgen_da72d497ae79b9b2 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmps -> IO Rpmpsi))

{-# NOINLINE rpmpsInitIterator_ptr #-}

{-|

  > rpmps

  Initialize problem set iterator.

  [__@ps@ /(input)/__]: problem set

  __returns:__ problem set iterator

__C declaration:__ @rpmpsInitIterator@

__defined at:__ @rpm\/rpmps.h:41:8@

__exported by:__ @rpm\/rpmps.h@
-}
rpmpsInitIterator_ptr :: Ptr.FunPtr (RPM.Types.Rpmps -> IO Rpmpsi)
rpmpsInitIterator_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_da72d497ae79b9b2

foreign import ccall unsafe "hs_bindgen_f7c314d0fbec44ad" hs_bindgen_f7c314d0fbec44ad ::
     IO (Ptr.FunPtr (Rpmpsi -> IO Rpmpsi))

{-# NOINLINE rpmpsFreeIterator_ptr #-}

{-|

  > rpmps

  Destroy problem set iterator.

  [__@psi@ /(input)/__]: problem set iterator

  __returns:__ problem set iterator (NULL)

__C declaration:__ @rpmpsFreeIterator@

__defined at:__ @rpm\/rpmps.h:48:8@

__exported by:__ @rpm\/rpmps.h@
-}
rpmpsFreeIterator_ptr :: Ptr.FunPtr (Rpmpsi -> IO Rpmpsi)
rpmpsFreeIterator_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f7c314d0fbec44ad

foreign import ccall unsafe "hs_bindgen_688f225ba40e6c17" hs_bindgen_688f225ba40e6c17 ::
     IO (Ptr.FunPtr (Rpmpsi -> IO RPM.Prob.RpmProblem))

{-# NOINLINE rpmpsiNext_ptr #-}

{-|

  > rpmps

  Return next problem from iterator

  [__@psi@ /(input)/__]: problem set iterator

  __returns:__ next problem (weak ref), NULL on termination

__C declaration:__ @rpmpsiNext@

__defined at:__ @rpm\/rpmps.h:55:12@

__exported by:__ @rpm\/rpmps.h@
-}
rpmpsiNext_ptr :: Ptr.FunPtr (Rpmpsi -> IO RPM.Prob.RpmProblem)
rpmpsiNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_688f225ba40e6c17

foreign import ccall unsafe "hs_bindgen_a0e86a6b6055cbb3" hs_bindgen_a0e86a6b6055cbb3 ::
     IO (Ptr.FunPtr (Rpmpsi -> IO FC.CInt))

{-# NOINLINE rpmpsNextIterator_ptr #-}

{-|

  > rpmps

  Return next problem set iterator index

  [__@psi@ /(input)/__]: problem set iterator

  __returns:__ iterator index, -1 on termination

__C declaration:__ @rpmpsNextIterator@

__defined at:__ @rpm\/rpmps.h:62:5@

__exported by:__ @rpm\/rpmps.h@
-}
rpmpsNextIterator_ptr :: Ptr.FunPtr (Rpmpsi -> IO FC.CInt)
rpmpsNextIterator_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a0e86a6b6055cbb3

foreign import ccall unsafe "hs_bindgen_5e89df94546d00ef" hs_bindgen_5e89df94546d00ef ::
     IO (Ptr.FunPtr (Rpmpsi -> IO RPM.Prob.RpmProblem))

{-# NOINLINE rpmpsGetProblem_ptr #-}

{-|

  > rpmps

  Return current problem from problem set

  [__@psi@ /(input)/__]: problem set iterator

  __returns:__ current rpmProblem

__C declaration:__ @rpmpsGetProblem@

__defined at:__ @rpm\/rpmps.h:69:12@

__exported by:__ @rpm\/rpmps.h@
-}
rpmpsGetProblem_ptr :: Ptr.FunPtr (Rpmpsi -> IO RPM.Prob.RpmProblem)
rpmpsGetProblem_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5e89df94546d00ef

foreign import ccall unsafe "hs_bindgen_e9b21becc9587fd0" hs_bindgen_e9b21becc9587fd0 ::
     IO (Ptr.FunPtr (IO RPM.Types.Rpmps))

{-# NOINLINE rpmpsCreate_ptr #-}

{-|

  > rpmps

  Create a problem set.

  __returns:__ new problem set

__C declaration:__ @rpmpsCreate@

__defined at:__ @rpm\/rpmps.h:75:7@

__exported by:__ @rpm\/rpmps.h@
-}
rpmpsCreate_ptr :: Ptr.FunPtr (IO RPM.Types.Rpmps)
rpmpsCreate_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e9b21becc9587fd0

foreign import ccall unsafe "hs_bindgen_3765d22bca1e3686" hs_bindgen_3765d22bca1e3686 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmps -> IO RPM.Types.Rpmps))

{-# NOINLINE rpmpsFree_ptr #-}

{-|

  > rpmps

  Destroy a problem set.

  [__@ps@ /(input)/__]: problem set

  __returns:__ NULL always

__C declaration:__ @rpmpsFree@

__defined at:__ @rpm\/rpmps.h:82:7@

__exported by:__ @rpm\/rpmps.h@
-}
rpmpsFree_ptr :: Ptr.FunPtr (RPM.Types.Rpmps -> IO RPM.Types.Rpmps)
rpmpsFree_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3765d22bca1e3686

foreign import ccall unsafe "hs_bindgen_6c16567724913531" hs_bindgen_6c16567724913531 ::
     IO (Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> RPM.Types.Rpmps -> IO ()))

{-# NOINLINE rpmpsPrint_ptr #-}

{-|

  > rpmps

  Print problems to file handle.

  [__@fp@ /(input)/__]: file handle (NULL uses stderr)

  [__@ps@ /(input)/__]: problem set

__C declaration:__ @rpmpsPrint@

__defined at:__ @rpm\/rpmps.h:89:6@

__exported by:__ @rpm\/rpmps.h@
-}
rpmpsPrint_ptr :: Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> RPM.Types.Rpmps -> IO ())
rpmpsPrint_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6c16567724913531

foreign import ccall unsafe "hs_bindgen_f8e4cff7d3bab99b" hs_bindgen_f8e4cff7d3bab99b ::
     IO (Ptr.FunPtr (RPM.Types.Rpmps -> RPM.Prob.RpmProblem -> IO ()))

{-# NOINLINE rpmpsAppendProblem_ptr #-}

{-|

  > rpmps

  Append a problem to current set of problems.

  [__@ps@ /(input)/__]: problem set

  [__@prob@ /(input)/__]: rpmProblem

__C declaration:__ @rpmpsAppendProblem@

__defined at:__ @rpm\/rpmps.h:96:6@

__exported by:__ @rpm\/rpmps.h@
-}
rpmpsAppendProblem_ptr :: Ptr.FunPtr (RPM.Types.Rpmps -> RPM.Prob.RpmProblem -> IO ())
rpmpsAppendProblem_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f8e4cff7d3bab99b

foreign import ccall unsafe "hs_bindgen_9718ffe3da6fdf38" hs_bindgen_9718ffe3da6fdf38 ::
     IO (Ptr.FunPtr (RPM.Types.Rpmps -> RPM.Types.Rpmps -> IO FC.CInt))

{-# NOINLINE rpmpsMerge_ptr #-}

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
rpmpsMerge_ptr :: Ptr.FunPtr (RPM.Types.Rpmps -> RPM.Types.Rpmps -> IO FC.CInt)
rpmpsMerge_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9718ffe3da6fdf38
