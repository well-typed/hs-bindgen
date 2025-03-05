module HsBindgen.Hs.AST.Name (
    -- * Definition
    HsName(..)
    -- ** Namespace
  , Namespace(..)
  , SNamespace(..)
  , namespaceOf
  , SingNamespace(..)
  ) where

import Data.String

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Namespace
--
-- See section 1.4, "Namespaces" of the Haskell report
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-130001.4>
data Namespace =
    NsTypeConstr
  | NsConstr
  | NsVar
  deriving (Eq, Ord, Show)

-- | Namespace singleton
data SNamespace :: Namespace -> Star where
  SNsTypeConstr :: SNamespace 'NsTypeConstr
  SNsConstr     :: SNamespace 'NsConstr
  SNsVar        :: SNamespace 'NsVar

-- | Get the namespace of a namespace singleton
namespaceOf :: SNamespace ns -> Namespace
namespaceOf = \case
    SNsTypeConstr -> NsTypeConstr
    SNsConstr     -> NsConstr
    SNsVar        -> NsVar

-- | Namespace singleton class
class SingNamespace ns where
  singNamespace :: SNamespace ns

instance SingNamespace 'NsTypeConstr where singNamespace = SNsTypeConstr
instance SingNamespace 'NsConstr     where singNamespace = SNsConstr
instance SingNamespace 'NsVar        where singNamespace = SNsVar

-- | Haskell name in namespace @ns@
newtype HsName (ns :: Namespace) = HsName { getHsName :: Text }
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)

