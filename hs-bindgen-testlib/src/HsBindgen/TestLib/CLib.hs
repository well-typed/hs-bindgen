{-# LANGUAGE CApiFFI #-}

module HsBindgen.TestLib.CLib (
    -- * GenSeq
    genSeqCChar
  , genSeqCSChar
  , genSeqCUChar
  , genSeqCShort
  , genSeqCUShort
  , genSeqCInt
  , genSeqCUInt
  , genSeqCLong
  , genSeqCULong
  , genSeqCPtrdiff
  , genSeqCSize
  , genSeqCWchar
  , genSeqCSigAtomic
  , genSeqCLLong
  , genSeqCULLong
  , genSeqCBool
  , genSeqCIntPtr
  , genSeqCUIntPtr
  , genSeqCIntMax
  , genSeqCUIntMax
  , genSeqCClock
  , genSeqCTime
  , genSeqCFloat
  , genSeqCDouble
    -- * Storable
    -- ** sizeof
  , sizeofCChar
  , sizeofCSChar
  , sizeofCUChar
  , sizeofCShort
  , sizeofCUShort
  , sizeofCInt
  , sizeofCUInt
  , sizeofCLong
  , sizeofCULong
  , sizeofCPtrdiff
  , sizeofCSize
  , sizeofCWchar
  , sizeofCSigAtomic
  , sizeofCLLong
  , sizeofCULLong
  , sizeofCBool
  , sizeofCIntPtr
  , sizeofCUIntPtr
  , sizeofCIntMax
  , sizeofCUIntMax
  , sizeofCClock
  , sizeofCTime
  , sizeofCFloat
  , sizeofCDouble
    -- ** alignof
  , alignofCChar
  , alignofCSChar
  , alignofCUChar
  , alignofCShort
  , alignofCUShort
  , alignofCInt
  , alignofCUInt
  , alignofCLong
  , alignofCULong
  , alignofCPtrdiff
  , alignofCSize
  , alignofCWchar
  , alignofCSigAtomic
  , alignofCLLong
  , alignofCULLong
  , alignofCBool
  , alignofCIntPtr
  , alignofCUIntPtr
  , alignofCIntMax
  , alignofCUIntMax
  , alignofCClock
  , alignofCTime
  , alignofCFloat
  , alignofCDouble
    -- * Preturb
  , preturbCChar
  , preturbCSChar
  , preturbCUChar
  , preturbCShort
  , preturbCUShort
  , preturbCInt
  , preturbCUInt
  , preturbCLong
  , preturbCULong
  , preturbCPtrdiff
  , preturbCSize
  , preturbCWchar
  , preturbCSigAtomic
  , preturbCLLong
  , preturbCULLong
  , preturbCBool
  , preturbCIntPtr
  , preturbCUIntPtr
  , preturbCIntMax
  , preturbCUIntMax
  , preturbCClock
  , preturbCTime
  , preturbCFloat
  , preturbCDouble
  ) where

import Foreign.C qualified as FC

{-------------------------------------------------------------------------------
  GenSeq
-------------------------------------------------------------------------------}

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CChar"
  genSeqCChar :: FC.CULong -> IO FC.CChar

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CSChar"
  genSeqCSChar :: FC.CULong -> IO FC.CSChar

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CUChar"
  genSeqCUChar :: FC.CULong -> IO FC.CUChar

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CShort"
  genSeqCShort :: FC.CULong -> IO FC.CShort

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CUShort"
  genSeqCUShort :: FC.CULong -> IO FC.CUShort

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CInt"
  genSeqCInt :: FC.CULong -> IO FC.CInt

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CUInt"
  genSeqCUInt :: FC.CULong -> IO FC.CUInt

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CLong"
  genSeqCLong :: FC.CULong -> IO FC.CLong

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CULong"
  genSeqCULong :: FC.CULong -> IO FC.CULong

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CPtrdiff"
  genSeqCPtrdiff :: FC.CULong -> IO FC.CPtrdiff

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CSize"
  genSeqCSize :: FC.CULong -> IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CWchar"
  genSeqCWchar :: FC.CULong -> IO FC.CWchar

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CSigAtomic"
  genSeqCSigAtomic :: FC.CULong -> IO FC.CSigAtomic

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CLLong"
  genSeqCLLong :: FC.CULong -> IO FC.CLLong

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CULLong"
  genSeqCULLong :: FC.CULong -> IO FC.CULLong

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CBool"
  genSeqCBool :: FC.CULong -> IO FC.CBool

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CIntPtr"
  genSeqCIntPtr :: FC.CULong -> IO FC.CIntPtr

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CUIntPtr"
  genSeqCUIntPtr :: FC.CULong -> IO FC.CUIntPtr

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CIntMax"
  genSeqCIntMax :: FC.CULong -> IO FC.CIntMax

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CUIntMax"
  genSeqCUIntMax :: FC.CULong -> IO FC.CUIntMax

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CClock"
  genSeqCClock :: FC.CULong -> IO FC.CClock

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CTime"
  genSeqCTime :: FC.CULong -> IO FC.CTime

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CFloat"
  genSeqCFloat :: FC.CULong -> IO FC.CFloat

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_genseq_CDouble"
  genSeqCDouble :: FC.CULong -> IO FC.CDouble

{-------------------------------------------------------------------------------
  Storable: sizeof
-------------------------------------------------------------------------------}

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CChar"
  sizeofCChar :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CSChar"
  sizeofCSChar :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CUChar"
  sizeofCUChar :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CShort"
  sizeofCShort :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CUShort"
  sizeofCUShort :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CInt"
  sizeofCInt :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CUInt"
  sizeofCUInt :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CLong"
  sizeofCLong :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CULong"
  sizeofCULong :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CPtrdiff"
  sizeofCPtrdiff :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CSize"
  sizeofCSize :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CWchar"
  sizeofCWchar :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CSigAtomic"
  sizeofCSigAtomic :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CLLong"
  sizeofCLLong :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CULLong"
  sizeofCULLong :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CBool"
  sizeofCBool :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CIntPtr"
  sizeofCIntPtr :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CUIntPtr"
  sizeofCUIntPtr :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CIntMax"
  sizeofCIntMax :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CUIntMax"
  sizeofCUIntMax :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CClock"
  sizeofCClock :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CTime"
  sizeofCTime :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CFloat"
  sizeofCFloat :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_sizeof_CDouble"
  sizeofCDouble :: IO FC.CSize

{-------------------------------------------------------------------------------
  Storable: alignof
-------------------------------------------------------------------------------}

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CChar"
  alignofCChar :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CSChar"
  alignofCSChar :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CUChar"
  alignofCUChar :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CShort"
  alignofCShort :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CUShort"
  alignofCUShort :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CInt"
  alignofCInt :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CUInt"
  alignofCUInt :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CLong"
  alignofCLong :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CULong"
  alignofCULong :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CPtrdiff"
  alignofCPtrdiff :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CSize"
  alignofCSize :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CWchar"
  alignofCWchar :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CSigAtomic"
  alignofCSigAtomic :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CLLong"
  alignofCLLong :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CULLong"
  alignofCULLong :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CBool"
  alignofCBool :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CIntPtr"
  alignofCIntPtr :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CUIntPtr"
  alignofCUIntPtr :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CIntMax"
  alignofCIntMax :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CUIntMax"
  alignofCUIntMax :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CClock"
  alignofCClock :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CTime"
  alignofCTime :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CFloat"
  alignofCFloat :: IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_alignof_CDouble"
  alignofCDouble :: IO FC.CSize

{-------------------------------------------------------------------------------
  Preturb
-------------------------------------------------------------------------------}

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CChar"
  preturbCChar :: FC.CLong -> FC.CChar -> IO FC.CChar

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CSChar"
  preturbCSChar :: FC.CLong -> FC.CSChar -> IO FC.CSChar

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CUChar"
  preturbCUChar :: FC.CLong -> FC.CUChar -> IO FC.CUChar

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CShort"
  preturbCShort :: FC.CLong -> FC.CShort -> IO FC.CShort

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CUShort"
  preturbCUShort :: FC.CLong -> FC.CUShort -> IO FC.CUShort

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CInt"
  preturbCInt :: FC.CLong -> FC.CInt -> IO FC.CInt

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CUInt"
  preturbCUInt :: FC.CLong -> FC.CUInt -> IO FC.CUInt

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CLong"
  preturbCLong :: FC.CLong -> FC.CLong -> IO FC.CLong

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CULong"
  preturbCULong :: FC.CLong -> FC.CULong -> IO FC.CULong

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CPtrdiff"
  preturbCPtrdiff :: FC.CLong -> FC.CPtrdiff -> IO FC.CPtrdiff

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CSize"
  preturbCSize :: FC.CLong -> FC.CSize -> IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CWchar"
  preturbCWchar :: FC.CLong -> FC.CWchar -> IO FC.CWchar

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CSigAtomic"
  preturbCSigAtomic :: FC.CLong -> FC.CSigAtomic -> IO FC.CSigAtomic

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CLLong"
  preturbCLLong :: FC.CLong -> FC.CLLong -> IO FC.CLLong

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CULLong"
  preturbCULLong :: FC.CLong -> FC.CULLong -> IO FC.CULLong

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CBool"
  preturbCBool :: FC.CLong -> FC.CBool -> IO FC.CBool

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CIntPtr"
  preturbCIntPtr :: FC.CLong -> FC.CIntPtr -> IO FC.CIntPtr

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CUIntPtr"
  preturbCUIntPtr :: FC.CLong -> FC.CUIntPtr -> IO FC.CUIntPtr

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CIntMax"
  preturbCIntMax :: FC.CLong -> FC.CIntMax -> IO FC.CIntMax

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CUIntMax"
  preturbCUIntMax :: FC.CLong -> FC.CUIntMax -> IO FC.CUIntMax

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CClock"
  preturbCClock :: FC.CLong -> FC.CClock -> IO FC.CClock

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CTime"
  preturbCTime :: FC.CLong -> FC.CTime -> IO FC.CTime

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CFloat"
  preturbCFloat :: FC.CLong -> FC.CFloat -> IO FC.CFloat

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_preturb_CDouble"
  preturbCDouble :: FC.CLong -> FC.CDouble -> IO FC.CDouble
