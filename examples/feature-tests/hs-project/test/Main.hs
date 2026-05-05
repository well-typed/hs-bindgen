module Main (main) where

import Test.Tasty

import Test.Callbacks.Arrays.KnownSize qualified
import Test.Callbacks.Arrays.UnknownSize qualified
import Test.Callbacks.Basic qualified
import Test.Callbacks.Structs qualified
import Test.Callbacks.Unions qualified
import Test.PointerManipulation qualified
import Test.PointerManipulation.Enums qualified
import Test.PointerManipulation.MacroTypes qualified
import Test.PointerManipulation.Structs qualified
import Test.PointerManipulation.Structs.Bitfields qualified
import Test.PointerManipulation.Typedefs qualified
import Test.PointerManipulation.Unions qualified
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
    , Test.PointerManipulation.Enums.tests
    , Test.PointerManipulation.MacroTypes.tests
    , Test.PointerManipulation.Structs.tests
    , Test.PointerManipulation.Structs.Bitfields.tests
    , Test.PointerManipulation.Typedefs.tests
    , Test.PointerManipulation.Unions.tests
    , Test.Types.Anonymous.tests
    , Test.Types.Bitfields.tests
    ]
