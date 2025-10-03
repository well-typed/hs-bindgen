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

-- | Path style used in generated Haddock comments
data PathStyle =
    -- | Abbreviate paths
    Short
    -- | Print full paths, potentially exposing the local file tree
  | Full
  deriving stock (Show, Eq, Generic)

instance Default PathStyle where
  def = Short
