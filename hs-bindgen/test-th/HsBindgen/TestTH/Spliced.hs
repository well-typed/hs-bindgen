-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}

module HsBindgen.TestTH.Spliced where

import Language.Haskell.TH (runIO)
import HsBindgen.Lib
import Misc
import System.FilePath ((</>))
import Foreign
import Foreign.C.Types
import Def

-- add this to break everything
-- import C.Expr.Posix32

$(runIO (findPackageDirectory "hs-bindgen") >>= \dir -> templateHaskell (Just dir) (dir </> "examples" </> "test-th-01.h"))

-- usage

val :: MyStruct
val = MyStruct
    { myStruct_field1 = 0
    , myStruct_field2 = 1
    }

pokeVal :: Ptr MyStruct -> IO ()
pokeVal ptr = poke ptr val

-- this splices works because the instances
-- from CC.Expr.BuildPlatform are still
-- visible here
--
-- But we, and users, must be careful
-- to no import anything also importing other C.Expr.<platform> modules.
foo :: CInt -> CInt -> CInt
foo x y = $bar x y
