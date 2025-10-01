module Manual.Tools (
   section
 , subsection
 , subsubsection
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

subsubsection :: String -> IO ()
subsubsection s = do
  putStrLn ""
  putStrLn $ "* " <> s <> " *"
  putStrLn ""
