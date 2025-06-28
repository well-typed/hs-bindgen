{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC

foreign import ccall safe "&simpleGlobal" simpleGlobal :: F.Ptr FC.CInt
