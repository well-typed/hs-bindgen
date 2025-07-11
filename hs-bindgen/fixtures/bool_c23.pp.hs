{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI

$(CAPI.addCSource "#include \"bool_c23.h\"\n")

foreign import capi safe "&b" b :: F.Ptr FC.CBool
