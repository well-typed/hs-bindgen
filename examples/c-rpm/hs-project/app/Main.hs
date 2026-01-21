module Main where

import Foreign (with)
import Foreign.C.String (castCCharToChar, peekCString, withCString)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)

import HsBindgen.Runtime.ConstPtr (ConstPtr (..))

import RPM.Argv (ARGV_const_t (..), ARGV_t (..))
import RPM.Argv.Safe qualified as RPM

main :: IO ()
main = do
  putStrLn "RPM String Utilities Demo"
  putStrLn "=========================="
  putStrLn ""

  -- Example 1: Split a string into words
  putStrLn "Example 1: Splitting a path string"
  putStrLn "-----------------------------------"
  let pathString = "/usr/bin:/usr/local/bin:/opt/bin"
  putStrLn $ "Input: \"" ++ pathString ++ "\""

  argv1 <- RPM.argvNew
  with argv1 $ \argvPtr -> do
    _ <- withCString pathString $ \strPtr ->
      withCString ":" $ \sepPtr ->
        RPM.argvSplit argvPtr (ConstPtr strPtr) (ConstPtr sepPtr)

    argv1' <- peek argvPtr
    count1 <- RPM.argvCount (ARGV_const_t (ConstPtr (un_ARGV_t argv1')))
    putStrLn $ "Split into " ++ show count1 ++ " parts:"

    arrayPtr1 <- peek (un_ARGV_t argv1')
    when (arrayPtr1 /= nullPtr) $ do
      cstrs <- peekArray (fromIntegral count1) arrayPtr1
      let strings = map castCCharToChar cstrs
      mapM_ (\(i, s) -> putStrLn $ "  [" ++ show (i :: Int) ++ "] " ++ s) (zip [0..] [strings])

    _ <- RPM.argvFree argv1'
    pure ()

  putStrLn ""

  -- Example 2: Split and rejoin with different separator
  putStrLn "Example 2: Transform separator"
  putStrLn "-------------------------------"
  let csvString = "apple,banana,cherry,date"
  putStrLn $ "Input: \"" ++ csvString ++ "\""
  putStrLn "Action: Split by ',' and join with ' | '"

  argv2 <- RPM.argvNew
  with argv2 $ \argvPtr -> do
    -- Split by comma
    _ <- withCString csvString $ \strPtr ->
      withCString "," $ \sepPtr ->
        RPM.argvSplit argvPtr (ConstPtr strPtr) (ConstPtr sepPtr)

    argv2' <- peek argvPtr

    -- Join with pipe
    withCString " | " $ \joinSep -> do
      resultPtr <- RPM.argvJoin (ARGV_const_t (ConstPtr (un_ARGV_t argv2'))) (ConstPtr joinSep)
      when (resultPtr /= nullPtr) $ do
        result <- peekCString resultPtr
        putStrLn $ "Output: \"" ++ result ++ "\""

    _ <- RPM.argvFree argv2'
    pure ()

  putStrLn ""
  putStrLn "✓ Success!"

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = pure ()
