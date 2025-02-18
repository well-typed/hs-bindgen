-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Test01 where

import HsBindgen.Lib
import System.FilePath ((</>))
import Foreign.C.Types

#ifdef MIN_VERSION_th_compat
import Language.Haskell.TH.Syntax.Compat (getPackageRoot)
#else
import Language.Haskell.TH.Syntax (getPackageRoot)
#endif

$(getPackageRoot >>= \dir -> templateHaskell [] [dir </> "examples"] "test-th-01.h")

