module HsBindgen.Hs.AST.Name (
    -- * Definition
    HsName(..)
  , Namespace(..)
    -- * Conversion
  , toHsName
  ) where

import Data.String
import Data.Text (Text)

import HsBindgen.C.AST (CName(..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Namespace
--
-- See section 1.4, "Namespaces" of the Haskell report
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-130001.4>
data Namespace =
    NsVar
  | NsConstr
  | NsTypeVar
  | NsTypeConstr
  | NsTypeClass
  | NsModuleName

newtype HsName (ns :: Namespace) = HsName { getHsName :: Text }
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | Name mangling
class ToHsName (ns :: Namespace) where
  toHsName :: CName -> HsName ns

-- | (Wrong) catch-all instance
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/189>
-- We need to properly implement this.
instance ToHsName ns where
  toHsName = HsName . getCName