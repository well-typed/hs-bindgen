module Manual.Tools (
   section
 , subsection
 ) where

section :: String -> IO ()
section s = do
  putStrLn ""
  putStrLn $ "*** " <> s <> " ***"
  putStrLn ""

subsection :: String -> IO ()
subsection s = do
  putStrLn ""
  putStrLn $ "** " <> s <> " **"
  putStrLn ""
