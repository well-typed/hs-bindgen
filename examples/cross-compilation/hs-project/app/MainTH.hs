module Main (main) where

import Optics ((%), (&), (.~))

import HsBindgen.TH

import ArchSizes
import PrintStructs

-- TH splice generates bindings for arch_types.h at compile time. During
-- cross-compilation, the splice runs inside iserv under QEMU.
let cfg :: Config
    cfg = def
      & #clang % #extraIncludeDirs .~ [Pkg "../c-src"]
    cfgTh :: ConfigTH
    cfgTh = def
 in withHsBindgen cfg cfgTh $
      hashInclude "arch_types.h"

main :: IO ()
main = do
    putStrLn "==========================================================="
    putStrLn "  Struct Sizes from Generated Bindings (hs-bindgen TH mode)"
    putStrLn "==========================================================="
    putStrLn ""

    printStructHsBindgen "ArchInfo"     (undefined :: ArchInfo)
    printStructHsBindgen "PointerArray" (undefined :: PointerArray)
    printStructHsBindgen "NestedStruct" (undefined :: NestedStruct)

    putStrLn "==========================================================="
    putStrLn "  Struct Sizes from hsc2hs (computed at build time)"
    putStrLn "==========================================================="
    putStrLn ""

    printStructHsc2hs "ArchInfo" archInfoSize archInfoAlignment
    printSizeHsc2hs   "PointerArray" pointerArraySize
    printSizeHsc2hs   "NestedStruct" nestedStructSize

    putStrLn "==========================================================="
    putStrLn "  Values should match between hs-bindgen and hsc2hs."
    putStrLn "  Both are determined by the target architecture."
    putStrLn "==========================================================="
