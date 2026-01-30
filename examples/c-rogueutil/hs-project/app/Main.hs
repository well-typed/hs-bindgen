{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Foreign.C (newCString, withCString)

import HsBindgen.Runtime.PtrConst qualified as PtrConst

import RogueUtil.Generated qualified as RU

main :: IO ()
main = do
  _ <- RU.saveDefaultColor

  RU.cls
  RU.hidecursor

  RU.setColor (fromIntegral $ RU.unwrapColor_code RU.YELLOW)
  RU.setBackgroundColor (fromIntegral $ RU.unwrapColor_code RU.BLUE)
  titleStr <- newCString "=== RogueUtil Haskell Bindings Demo ==="
  RU.printXY 20 2 (RU.RUTIL_STRING $ PtrConst.unsafeFromPtr titleStr)
  RU.resetColor

  rows <- RU.trows
  cols <- RU.tcols
  RU.setColor (fromIntegral $ RU.unwrapColor_code RU.CYAN)
  sizeMsg <- newCString $ "Terminal size: " ++ show cols ++ "x" ++ show rows
  RU.printXY 20 4 (RU.RUTIL_STRING $ PtrConst.unsafeFromPtr sizeMsg)
  RU.resetColor

  let colors = [RU.RED, RU.YELLOW, RU.GREEN, RU.CYAN, RU.BLUE, RU.MAGENTA]
  forM_ (zip [0 :: Int ..] colors) $ \(i, color) -> do
    RU.setColor (fromIntegral $ RU.unwrapColor_code color)
    withCString ("█████ " ++ show color) $ \str ->
      RU.printXY 20 (6 + fromIntegral i) (RU.RUTIL_STRING $ PtrConst.unsafeFromPtr str)
  RU.resetColor

  RU.locate 20 14
  RU.showcursor
  pressKeyMsg <- newCString "Press any key to exit..."
  RU.anykey (RU.RUTIL_STRING $ PtrConst.unsafeFromPtr pressKeyMsg)

  RU.cls
  RU.resetColor
