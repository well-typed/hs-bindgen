{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-dodgy-foreign-imports #-}

module Example where

import qualified Foreign.C as FC

a :: FC.CInt
a = (5 :: FC.CInt)
