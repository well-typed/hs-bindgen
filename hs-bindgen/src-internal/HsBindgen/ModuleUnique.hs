module HsBindgen.ModuleUnique (
    ModuleUnique (..),
) where

import HsBindgen.Imports

-- | An identifier string used to generate morally module-private but externally visible symbols
-- Such identifiers are e.g. C symbols.
newtype ModuleUnique = ModuleUnique { unModuleUnique :: String }
  deriving newtype (Show)

instance IsString ModuleUnique where
    fromString = coerce
