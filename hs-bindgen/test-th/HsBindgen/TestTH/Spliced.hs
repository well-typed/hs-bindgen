-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module HsBindgen.TestTH.Spliced where

import HsBindgen.Lib
import System.FilePath ((</>))
import Foreign
import Foreign.C.Types

#ifdef MIN_VERSION_th_compat
import Language.Haskell.TH.Syntax.Compat (getPackageRoot)
#else
import Language.Haskell.TH.Syntax (getPackageRoot)
#endif

$(getPackageRoot >>= \dir -> templateHaskell (dir </> "examples" </> "test-th-01.h"))

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
