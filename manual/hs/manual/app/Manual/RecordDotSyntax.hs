-- Enable getter syntax
{-# LANGUAGE OverloadedRecordDot #-}

-- Enable setter syntax
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}

-- Other language extensions
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Manual.RecordDotSyntax (examples) where

import Control.Monad (unless)
import Foreign.C.Types (CInt)
import GHC.Stack (HasCallStack)

import HsBindgen.Runtime.Overloading
import HsBindgen.Runtime.Struct qualified as Struct
import HsBindgen.Runtime.Union qualified as Union

import Manual.Tools (section, subsection)
import RecordDotSyntax
import RecordDotSyntax.Global
import RecordDotSyntax.Unsafe (c_move_x)

examples :: IO ()
examples = do
    section "Record dot syntax"

    exampleTaggedUnions

{-------------------------------------------------------------------------------
  Example: tagged unions
-------------------------------------------------------------------------------}

exampleTaggedUnions :: IO ()
exampleTaggedUnions = do
    subsection "Example: tagged unions"

    checkEqShape c_example_Rectangle  hs_example_Rectangle
    checkEqShape hs_example_Rectangle ds_example_Rectangle
    putStrLn $ showShape c_example_Rectangle

    let rect = c_example_Rectangle
    c_rect' <- c_move_x 5 rect
    let ds_rect' = ds_move_x 5 rect

    checkEqShape c_rect' ds_rect'
    putStrLn $ showShape c_rect'

hs_example_Rectangle :: Shape
hs_example_Rectangle = Shape {
      tag = Rectangle_tag
    , anon'rectangle = Union.set @"rectangle" $
        Rectangle {
            top_left  = Point { x = 3, y = 7 }
          , bot_right = Point { x = 9, y = -17 }
          }
    }

ds_example_Rectangle :: Shape
ds_example_Rectangle = (Struct.zero @Shape) {
      tag = Rectangle_tag
    , rectangle.top_left.x  = 3
    , rectangle.top_left.y  = 7
    , rectangle.bot_right.x = 9
    , rectangle.bot_right.y = -17
    }

ds_move_x :: HasCallStack => CInt -> Shape -> Shape
ds_move_x delta s = case s.tag of
    Rectangle_tag -> s {
        rectangle.top_left.x  = s.rectangle.top_left.x  + delta
      , rectangle.bot_right.x = s.rectangle.bot_right.x + delta
      }
    Circle_tag -> s { circle.mid.x = s.circle.mid.x + delta }
    _ -> error $ "ds_move_x: unknown shape tag: " ++ show s.tag

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

showShape :: HasCallStack => Shape -> String
showShape s = case s.tag of
    Rectangle_tag -> show s.rectangle
    Circle_tag -> show s.circle
    _ -> error $ "showShape: unknown shape tag: " ++ show s.tag

eqShape :: HasCallStack => Shape -> Shape -> Bool
eqShape s1 s2 = case (s1.tag, s2.tag) of
    (Rectangle_tag, Rectangle_tag) -> s1.rectangle == s2.rectangle
    (Circle_tag,    Circle_tag   ) -> s1.circle == s2.circle
    (tag1, tag2) -> error $ concat [
        "eqShape: unknown shape tags: "
      , show tag1
      , " /= "
      , show tag2
      ]

checkEqShape :: HasCallStack => Shape -> Shape -> IO ()
checkEqShape s1 s2 = unless (eqShape s1 s2) $
    error $ concat [
        "checkEqShape: "
      , showShape s1
      , " /= "
      , showShape s2
      ]
