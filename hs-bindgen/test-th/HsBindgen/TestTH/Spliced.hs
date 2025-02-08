-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}

module HsBindgen.TestTH.Spliced where

import Language.Haskell.TH (runIO)
import HsBindgen.Lib
import Misc
import System.FilePath ((</>))
import Foreign
import Foreign.C.Types

$(runIO (findPackageDirectory "hs-bindgen") >>= \dir -> templateHaskell (dir </> "examples" </> "test-th-01.h"))

-- usage

val :: MyStruct
val = MyStruct
    { myStruct_field1 = 0
    , myStruct_field2 = 1
    }

pokeVal :: Ptr MyStruct -> IO ()
pokeVal ptr = poke ptr val

myPlus :: CLong -> CLong -> CLong
myPlus x y = pLUS x y
