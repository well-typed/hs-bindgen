{-# LANGUAGE CApiFFI #-}

module HsBindgen.TestLib.CLib (
    -- * Transform
    transformCChar
  , transformCInt
  , transformCFloat
  , transformCDouble
  ) where

import Foreign.C qualified as FC

{-------------------------------------------------------------------------------
  Transform
-------------------------------------------------------------------------------}

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CChar"
  transformCChar :: FC.CChar -> IO FC.CChar

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CInt"
  transformCInt :: FC.CInt -> IO FC.CInt

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CFloat"
  transformCFloat :: FC.CFloat -> IO FC.CFloat

foreign import capi unsafe "hs_bindgen_testlib.h hsbg_transform_CDouble"
  transformCDouble :: FC.CDouble -> IO FC.CDouble
