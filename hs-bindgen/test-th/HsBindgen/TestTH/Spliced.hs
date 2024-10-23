-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}

module HsBindgen.TestTH.Spliced where

import Language.Haskell.TH (runIO)
import HsBindgen.Lib
import Misc
import System.FilePath ((</>))
import Foreign

$(runIO (findPackageDirectory "hs-bindgen") >>= \dir -> templateHaskell (dir </> "examples" </> "test-th-01.h"))

-- usage

val :: CMyStruct
val = MkCMyStruct
    { cMyStruct_field1 = 0
    , cMyStruct_field2 = 1
    }

pokeVal :: Ptr CMyStruct -> IO ()
pokeVal ptr = poke ptr val
