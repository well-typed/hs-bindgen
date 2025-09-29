module HsBindgen.Backend.Hs.Haddock.Config where

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Haddock configuration
-------------------------------------------------------------------------------}

data HaddockConfig = HaddockConfig {
      pathStyle :: PathStyle
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Default

data PathStyle = Short
               | Full
  deriving stock (Show, Eq, Generic)

instance Default PathStyle where
  def = Short
