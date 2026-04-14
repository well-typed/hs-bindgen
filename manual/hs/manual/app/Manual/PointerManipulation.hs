{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module Manual.PointerManipulation (examples) where

import Foreign
import Optics
import Text.Printf

import Manual.Tools
import PointerManipulation qualified as Gen

examples :: IO ()
examples = do
    section "Pointer manipulation API"
    do
      subsection "Example 1"
      withExampleRectangle example1

      subsection "Example 2"
      withExampleRectangle example2

example1 :: Ptr Gen.Rectangle -> IO ()
example1 rectPtr = do
    rect <- peek rectPtr
    let rect' = rect & #rectangle_topleft % #point_x .~ 2
    poke rectPtr rect'

example2 :: Ptr Gen.Rectangle -> IO ()
example2 rectPtr =
    poke rectPtr.rectangle_topleft.point_x 2

exampleRectangle :: Gen.Rectangle
exampleRectangle = Gen.Rectangle (Gen.Point 0 3) (Gen.Point 2 7)

withExampleRectangle :: (Ptr Gen.Rectangle -> IO a) -> IO a
withExampleRectangle k = do
    let rect = exampleRectangle
    printf "Before: %s\n" (show rect)
    with rect $ \ptr -> do
      x <- k ptr
      peek ptr >>= printf "After: %s\n" . show
      pure x
