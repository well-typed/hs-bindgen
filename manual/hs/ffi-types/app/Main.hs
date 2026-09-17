module Main (main) where

import Example1.A qualified as Ex1
import Example1.B qualified as Ex1
import Example1.C.Safe qualified as Ex1
import Example2.A qualified as Ex2
import Example2.B qualified as Ex2
import Example2.C.Safe qualified as Ex2
import Example3.A qualified as Ex3
import Example3.B qualified as Ex3
import Example3.C.Safe qualified as Ex3
import Example4.A qualified as Ex4
import Example4.B qualified as Ex4
import Example4.C.Safe qualified as Ex4
import Example5.A qualified as Ex5
import Example5.B qualified as Ex5
import Example5.C.Safe qualified as Ex5
import Example7.A qualified as Ex7
import Example7.B qualified as Ex7
import Example7.C.Safe qualified as Ex7
import Example8.A qualified as Ex8
import Example8.B qualified as Ex8
import Example8.C.Safe qualified as Ex8

main :: IO ()
main = do
    putStrLn "Manual: FFI types"

    putStr "Example 1: "
    print =<< Ex1.foo (Ex1.B (Ex1.A 1))

    putStr "Example 2: "
    print =<< Ex2.bar (Ex2.B (Ex2.A 2))

    putStr "Example 3: "
    print =<< Ex3.bar (Ex3.B (Ex3.A 3))

    putStr "Example 4: "
    print =<< Ex4.foo (Ex4.B (Ex4.A 4))

    putStr "Example 5: "
    print =<< Ex5.foo (Ex5.B (Ex5.fromCInt 5))

    putStr "Example 6: "
    putStrLn "This example has no output by design"

    putStr "Example 7: "
    print =<< Ex7.foo (Ex7.B (Ex7.A 7))

    putStr "Example 8: "
    print =<< Ex8.foo (Ex8.B (Ex8.A 8))

