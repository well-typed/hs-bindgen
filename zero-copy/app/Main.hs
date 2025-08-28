{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Foreign

import HsBindgen.Runtime.ConstantArray qualified as CA

import Generated qualified as G
import ZeroCopy  qualified as Z

main :: IO ()
main = do
    with (G.Rect (G.Point 1 2) (G.Point 3 4)) $ G.show_rect

    with (Z.Rect (Z.Point 1 2) (Z.Point 3 4)) $ \ptr -> do
      poke ptr.bottomright.x 99
      Z.show_rect ptr

    with (Z.set_pointVsArray_asArray $ CA.fromList [55, 66]) $ \ptr -> do
      y <- peek ptr.asPoint.y
      print y
