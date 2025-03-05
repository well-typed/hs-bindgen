-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Test02 where

import Data.String (IsString (..))
import Data.Word qualified
import Language.Haskell.TH qualified as TH
import System.FilePath ((</>), joinPath)

import HsBindgen.Lib
import HsBindgen.Runtime.LibC qualified

#ifdef MIN_VERSION_th_compat
import Language.Haskell.TH.Syntax.Compat (getPackageRoot)
#else
import Language.Haskell.TH.Syntax (getPackageRoot)
#endif

$(do
    dir <- getPackageRoot
    let args = defaultClangArgs {
            clangQuoteIncludePathDirs = [fromString (dir </> "examples")]
          }
    extBindings <- TH.runIO $ loadExtBindings args [
        joinPath [dir, "..", "hs-bindgen-runtime", "base.yaml"]
      , joinPath [dir, "..", "hs-bindgen-runtime", "hs-bindgen-runtime.yaml"]
      ]
    genBindings "test_02.h" extBindings args
 )
