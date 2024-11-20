{-# LANGUAGE CApiFFI #-}

module HsBindgen.TestLib.CLib (
    -- * Transform
    transformCChar
  , transformCSChar
  , transformCUChar
  , transformCShort
  , transformCUShort
  , transformCInt
  , transformCUInt
  , transformCLong
  , transformCULong
  , transformCPtrdiff
  , transformCSize
  , transformCWchar
  , transformCSigAtomic
  , transformCLLong
  , transformCULLong
  , transformCBool
  , transformCIntPtr
  , transformCUIntPtr
  , transformCIntMax
  , transformCUIntMax
  , transformCClock
  , transformCTime
  -- , transformCUSeconds
  , transformCSUSeconds
  , transformCFloat
  , transformCDouble
  ) where

import Foreign.C qualified as FC

{-------------------------------------------------------------------------------
  Transform
-------------------------------------------------------------------------------}

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CChar"
  transformCChar :: FC.CChar -> IO FC.CChar

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CSChar"
  transformCSChar :: FC.CSChar -> IO FC.CSChar

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CUChar"
  transformCUChar :: FC.CUChar -> IO FC.CUChar

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CShort"
  transformCShort :: FC.CShort -> IO FC.CShort

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CUShort"
  transformCUShort :: FC.CUShort -> IO FC.CUShort

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CInt"
  transformCInt :: FC.CInt -> IO FC.CInt

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CUInt"
  transformCUInt :: FC.CUInt -> IO FC.CUInt

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CLong"
  transformCLong :: FC.CLong -> IO FC.CLong

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CULong"
  transformCULong :: FC.CULong -> IO FC.CULong

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CPtrdiff"
  transformCPtrdiff :: FC.CPtrdiff -> IO FC.CPtrdiff

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CSize"
  transformCSize :: FC.CSize -> IO FC.CSize

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CWchar"
  transformCWchar :: FC.CWchar -> IO FC.CWchar

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CSigAtomic"
  transformCSigAtomic :: FC.CSigAtomic -> IO FC.CSigAtomic

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CLLong"
  transformCLLong :: FC.CLLong -> IO FC.CLLong

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CULLong"
  transformCULLong :: FC.CULLong -> IO FC.CULLong

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CBool"
  transformCBool :: FC.CBool -> IO FC.CBool

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CIntPtr"
  transformCIntPtr :: FC.CIntPtr -> IO FC.CIntPtr

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CUIntPtr"
  transformCUIntPtr :: FC.CUIntPtr -> IO FC.CUIntPtr

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CIntMax"
  transformCIntMax :: FC.CIntMax -> IO FC.CIntMax

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CUIntMax"
  transformCUIntMax :: FC.CUIntMax -> IO FC.CUIntMax

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CClock"
  transformCClock :: FC.CClock -> IO FC.CClock

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CTime"
  transformCTime :: FC.CTime -> IO FC.CTime

{- TODO remove or fix
foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CUSeconds"
  transformCUSeconds :: FC.CUSeconds -> IO FC.CUSeconds
-}

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CSUSeconds"
  transformCSUSeconds :: FC.CSUSeconds -> IO FC.CSUSeconds

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CFloat"
  transformCFloat :: FC.CFloat -> IO FC.CFloat

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CDouble"
  transformCDouble :: FC.CDouble -> IO FC.CDouble
