module Main (main) where

import ArchSizes
import ArchTypes.Generated
import PrintStructs

main :: IO ()
main = do
    putStrLn "==========================================================="
    putStrLn "  Struct Sizes from Generated Bindings (hs-bindgen)"
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
