module HsBindgen.Backend.Hs.Haddock.Config where

import Data.Default (Default)
import GHC.Generics (Generic)

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
  deriving anyclass Default
