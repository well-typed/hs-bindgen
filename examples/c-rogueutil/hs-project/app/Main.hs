{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Foreign.C (newCString, withCString)

import RogueUtil.Generated qualified as RU
import RogueUtil.Generated.Safe qualified as Safe

main :: IO ()
main = do
  _ <- Safe.saveDefaultColor

  Safe.cls
  Safe.hidecursor

  Safe.setColor (fromIntegral $ RU.un_Color_code RU.YELLOW)
  Safe.setBackgroundColor (fromIntegral $ RU.un_Color_code RU.BLUE)
  titleStr <- newCString "=== RogueUtil Haskell Bindings Demo ==="
  Safe.printXY 20 2 (RU.RUTIL_STRING titleStr)
  Safe.resetColor

  rows <- Safe.trows
  cols <- Safe.tcols
  Safe.setColor (fromIntegral $ RU.un_Color_code RU.CYAN)
  sizeMsg <- newCString $ "Terminal size: " ++ show cols ++ "x" ++ show rows
  Safe.printXY 20 4 (RU.RUTIL_STRING sizeMsg)
  Safe.resetColor

  let colors = [RU.RED, RU.YELLOW, RU.GREEN, RU.CYAN, RU.BLUE, RU.MAGENTA]
  forM_ (zip [0..] colors) $ \(i, color) -> do
    Safe.setColor (fromIntegral $ RU.un_Color_code color)
    withCString ("█████ " ++ show color) $ \str ->
      Safe.printXY 20 (6 + fromIntegral i) (RU.RUTIL_STRING str)
  Safe.resetColor

  Safe.locate 20 14
  Safe.showcursor
  pressKeyMsg <- newCString "Press any key to exit..."
  Safe.anykey (RU.RUTIL_STRING pressKeyMsg)

  Safe.cls
  Safe.resetColor
