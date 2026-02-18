module Main where

import Control.Monad (unless)
import Foreign qualified as F
import System.Exit (exitFailure)

import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Rect (Rect (..))
import Rect.Safe qualified as Rect

main :: IO ()
main = do
    let r = Rect
              { rect_x      = 1.0
              , rect_y      = 2.0
              , rect_width  = 5.0
              , rect_height = 3.0
              }

    F.with r $ \ptr -> do
      let ptrc = PtrConst.unsafeFromPtr ptr
      area      <- Rect.rect_area ptrc
      perimeter <- Rect.rect_perimeter ptrc
      putStrLn $ "Rectangle at (" ++ show (rect_x r) ++ ", " ++ show (rect_y r) ++ ")"
      putStrLn $ "  width:     " ++ show (rect_width r)
      putStrLn $ "  height:    " ++ show (rect_height r)
      putStrLn $ "  area:      " ++ show area
      putStrLn $ "  perimeter: " ++ show perimeter

      -- Assert expected results; fails if struct layout is mismatched
      unless (area == 15.0) $ do
        putStrLn $ "FAIL: expected area == 15.0, got " ++ show area
        exitFailure
      unless (perimeter == 16.0) $ do
        putStrLn $ "FAIL: expected perimeter == 16.0, got " ++ show perimeter
        exitFailure
