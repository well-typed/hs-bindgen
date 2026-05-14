module PrintStructs
    ( printStructHsBindgen
    , printStructHsc2hs
    , printSizeHsc2hs
    ) where

import Foreign.Storable (Storable (..))

printStructHsBindgen :: Storable a => String -> a -> IO ()
printStructHsBindgen name x = do
    putStrLn (name ++ ":")
    putStrLn $ "  sizeOf    = " ++ show (sizeOf x)
    putStrLn $ "  alignment = " ++ show (alignment x)
    putStrLn ""

printStructHsc2hs :: String -> Int -> Int -> IO ()
printStructHsc2hs name size_ alignment_ = do
    putStrLn (name ++ ":")
    putStrLn $ "  size      = " ++ show size_
    putStrLn $ "  alignment = " ++ show alignment_
    putStrLn ""

printSizeHsc2hs :: String -> Int -> IO ()
printSizeHsc2hs name size_ = do
    putStrLn (name ++ ":")
    putStrLn $ "  size      = " ++ show size_
    putStrLn ""
