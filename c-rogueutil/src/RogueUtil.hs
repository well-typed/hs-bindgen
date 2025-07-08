{-# LANGUAGE TypeOperators #-}

module RogueUtil
  ( module RogueUtil.Generated
  , colorPrint
  )
  where

import RogueUtil.Generated
import Formatting (Format, fprint)
import Control.Monad.IO.Class (MonadIO)

-- | 'colorPrint' rogueutil function was not generated because hs-bindgen does
-- not support functions with varargs. However, this function is similar to
-- printf in the sense that it formats a variable number of arguments with
-- a C-style formatting string. With this being said, we can provide our own
-- manual binding for this function, using the formatting library.
--
colorPrint :: MonadIO m => Color_code -> Color_code -> Format (m ()) (t -> IO ()) -> t -> IO ()
colorPrint color bgColor f a = do
  setColor (fromIntegral $ un_Color_code color)
  setBackgroundColor (fromIntegral $ un_Color_code bgColor)
  fprint f a
  resetColor
