module Main (main) where

import Foreign.Storable (Storable (..))

import ArchSizes
import ArchTypes.Generated

main :: IO ()
main = do
    putStrLn "==========================================================="
    putStrLn "  Struct Sizes from Generated Bindings (hs-bindgen)"
    putStrLn "==========================================================="
    putStrLn ""

    putStrLn "ArchInfo:"
    putStrLn $ "  sizeOf    = " ++ show (sizeOf (undefined :: ArchInfo))
    putStrLn $ "  alignment = " ++ show (alignment (undefined :: ArchInfo))
    putStrLn ""

    putStrLn "PointerArray:"
    putStrLn $ "  sizeOf    = " ++ show (sizeOf (undefined :: PointerArray))
    putStrLn $ "  alignment = " ++ show (alignment (undefined :: PointerArray))
    putStrLn ""

    putStrLn "NestedStruct:"
    putStrLn $ "  sizeOf    = " ++ show (sizeOf (undefined :: NestedStruct))
    putStrLn $ "  alignment = " ++ show (alignment (undefined :: NestedStruct))
    putStrLn ""

    putStrLn "==========================================================="
    putStrLn "  Struct Sizes from hsc2hs (computed at build time)"
    putStrLn "==========================================================="
    putStrLn ""

    putStrLn "ArchInfo:"
    putStrLn $ "  size      = " ++ show archInfoSize
    putStrLn $ "  alignment = " ++ show archInfoAlignment
    putStrLn ""

    putStrLn "PointerArray:"
    putStrLn $ "  size      = " ++ show pointerArraySize
    putStrLn ""

    putStrLn "NestedStruct:"
    putStrLn $ "  size      = " ++ show nestedStructSize
    putStrLn ""

    putStrLn "==========================================================="
    putStrLn "  Values should match between hs-bindgen and hsc2hs."
    putStrLn "  Both are determined by the target architecture."
    putStrLn "==========================================================="
