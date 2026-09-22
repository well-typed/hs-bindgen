module Main (main) where

import Example1 qualified as Ex1
import Example1.Safe qualified as Ex1
import Example2.A qualified as Ex2
import Example2.B qualified as Ex2
import Example2.C.Safe qualified as Ex2
import Example3.A qualified as Ex3
import Example3.B qualified as Ex3
import Example3.C.Safe qualified as Ex3
import Example4.A qualified as Ex4
import Example4.B qualified as Ex4
import Example4.C.Safe qualified as Ex4

main :: IO ()
main = do
    putStrLn "Manual: FFI types"

    putStr "Example 1: "
    print =<< Ex1.foo (Ex1.B (Ex1.A 1))
    putStr "Example 2: "
    print =<< Ex2.foo (Ex2.B (Ex2.A 2))
    putStr "Example 3: "
    print =<< Ex3.foo (Ex3.B (Ex3.A 3))
    putStr "Example 4: "
    print =<< Ex4.foo (Ex4.B (Ex4.fromCInt 4))
