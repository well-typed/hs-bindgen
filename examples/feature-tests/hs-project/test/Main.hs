module Main (main) where

import Test.Tasty

import Test.Callbacks.Arrays.KnownSize qualified
import Test.Callbacks.Arrays.UnknownSize qualified
import Test.Callbacks.Basic qualified
import Test.Callbacks.Structs qualified
import Test.Callbacks.Unions qualified
import Test.PointerManipulation qualified
import Test.PointerManipulation.Structs qualified
import Test.Types.Anonymous qualified
import Test.Types.Bitfields qualified

main :: IO ()
main = defaultMain $ testGroup "feature-tests" [
      Test.Callbacks.Arrays.KnownSize.tests
    , Test.Callbacks.Arrays.UnknownSize.tests
    , Test.Callbacks.Basic.tests
    , Test.Callbacks.Structs.tests
    , Test.Callbacks.Unions.tests
    , Test.PointerManipulation.tests
    , Test.PointerManipulation.Structs.tests
    , Test.Types.Anonymous.tests
    , Test.Types.Bitfields.tests
    ]
