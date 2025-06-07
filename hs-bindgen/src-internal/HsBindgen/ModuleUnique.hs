{-# LANGUAGE DeriveLift #-}
module HsBindgen.ModuleUnique (
    ModuleUnique (..),
) where

import HsBindgen.Imports
import Language.Haskell.TH.Syntax (Lift)

-- | An identifier string used to generate morally module-private but externally visible symbols
-- Such identifiers are e.g. C symbols.
newtype ModuleUnique = ModuleUnique { unModuleUnique :: String }
  deriving newtype (Show)
  deriving stock Lift

instance IsString ModuleUnique where
    fromString = coerce
