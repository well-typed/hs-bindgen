{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Manual.UnprefixedFieldNames (examples) where

import Prelude

import Manual.Tools
import UnprefixedFieldNames

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Unprefixed field names"

    subsection "Using unprefixed field names with DuplicateRecordFields"
    -- With DuplicateRecordFields, multiple structs can have fields with the same name
    let point = Point 10 20
        size  = Size 100 200

    -- Access fields using OverloadedRecordDot
    putStrLn $ "Point x: " <> show (point.x)
    putStrLn $ "Point y: " <> show (point.y)
    putStrLn $ "Size width: " <> show (size.width)
    putStrLn $ "Size height: " <> show (size.height)

    subsection "Updating fields with OverloadedRecordUpdate"
    -- Update specific fields using OverloadedRecordUpdate
    let point' :: Point
        point' = point { x = 30 }
    putStrLn $ "Updated point x: " <> show (point'.x)
    putStrLn $ "Unchanged point y: " <> show (point'.y)

    subsection "Unwrapping newtype values"
    -- Newtypes have field named 'unwrap' (not 'unwrapValue')
    let value = Value 42
    putStrLn $ "Value: " <> show (value.unwrap)

    subsection "Rect with overlapping field names"
    -- Rect reuses x, y from Point and width, height from Size
    let rect = Rect 5 10 50 100
    putStrLn $ "Rect x: " <> show (rect.x)
    putStrLn $ "Rect y: " <> show (rect.y)
    putStrLn $ "Rect width: " <> show (rect.width)
    putStrLn $ "Rect height: " <> show (rect.height)

    -- Update rect using record update syntax
    let rect' :: Rect
        rect' = rect { x = 15, y = 25 }
    putStrLn $ "Updated rect: " <> show (rect'.x) <> ", " <> show (rect'.y)
