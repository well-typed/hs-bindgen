module Main (main) where

import Foreign.Storable (Storable (..))

import ArchTypes.Generated

main :: IO ()
main = do
    putStrLn "═══════════════════════════════════════════════════════════"
    putStrLn "  Struct Sizes from Generated Bindings"
    putStrLn "═══════════════════════════════════════════════════════════"
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

    putStrLn "═══════════════════════════════════════════════════════════"
    putStrLn "  These values are determined at binding generation time,"
    putStrLn "  based on the target architecture specified to hs-bindgen."
    putStrLn "═══════════════════════════════════════════════════════════"
