module HsBindgen.Hs.AST.Name (
    -- * Definition
    HsName(..)
  , Namespace(..)
    -- * Conversion
  , toHsName
  ) where

import Data.Char qualified as Char
import Data.String
import Data.Text (Text)
import Data.Text qualified as T

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

-- TODO <https://github.com/well-typed/hs-bindgen/issues/189>
--
-- The following instances just adjust the case of the first letter of the name
-- as required, according to the target namespace.  We will add a context and
-- options as well as consider edge cases in future commits.

instance ToHsName NsVar where
  toHsName = toHsName' Char.toLower

instance ToHsName NsConstr where
  toHsName = toHsName' Char.toUpper

instance ToHsName NsTypeVar where
  toHsName = toHsName' Char.toLower

instance ToHsName NsTypeConstr where
  toHsName = toHsName' Char.toUpper

instance ToHsName NsTypeClass where
  toHsName = toHsName' Char.toUpper

instance ToHsName NsModuleName where
  toHsName = toHsName' Char.toUpper

toHsName' ::
     (Char -> Char)  -- ^ case conversion for first character
  -> CName
  -> HsName ns
toHsName' f = HsName . aux . getCName
  where
    aux :: Text -> Text
    aux t = case T.uncons t of
      Just (c, t')
        | Char.isLetter c -> T.cons (f c) t'
      _otherwise -> t
