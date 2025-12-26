-- | This module is intended for unqualified import because some declarations
-- | are reexported from the public Template Haskell interface.
module HsBindgen.Backend.Category (
    -- * Binding categories
    TermCategory(..)
  , Category(..)
  , allCategories
  , ByCategory(..)
  , mapWithCategory
  , ByCategory_(..)
  , mapWithCategory_
  , lensForCategory
  , lensForTermCategory
    -- * Binding category levels
  , CategoryLvl(..)
    -- * Binding category choices
  , RenameTerm(..)
  , Choice(..)
  , useSafeCategory
  , useUnsafeCategory
  ) where

import Data.Functor.Const (Const (..))
import Optics.Core (Lens', iso)
import Optics.Iso (Iso')

import HsBindgen.Imports hiding (toList)

data TermCategory =
    -- | Foreign import bindings with a @safe@ foreign import modifier.
    CSafe
    -- | Foreign import bindings with an @unsafe@ foreign import modifier.
  | CUnsafe
    -- | Pointers to functions; generally @unsafe@.
  | CFunPtr
    -- | Temporary category for bindings to global variables or constants.
  | CGlobal
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Binding category.
data Category =
    -- | Types (top-level bindings).
    CType
  | CTerm TermCategory
  deriving stock (Show, Eq, Ord)

allCategories :: [Category]
allCategories = [CType] ++ map CTerm [minBound .. maxBound]

-- | Like 'Data.Map.Strict.mapWithKey'.
mapWithCategory ::
     (f LvlType -> g LvlType)
  -> (TermCategory -> f LvlTerm -> g LvlTerm)
  -> ByCategory f
  -> ByCategory g
mapWithCategory f g ByCategory{..} =
  ByCategory {
      cType   = f         cType
    , cSafe   = g CSafe   cSafe
    , cUnsafe = g CUnsafe cUnsafe
    , cFunPtr = g CFunPtr cFunPtr
    , cGlobal = g CGlobal cGlobal
    }

-- | A strict, total map from 'Category' to 'a'.
type ByCategory :: (CategoryLvl -> Star) -> Star
data ByCategory f = ByCategory {
      cType   :: !(f LvlType)
    , cSafe   :: !(f LvlTerm)
    , cUnsafe :: !(f LvlTerm)
    , cFunPtr :: !(f LvlTerm)
    , cGlobal :: !(f LvlTerm)
  }
  deriving stock (Generic)

deriving instance (Eq (f LvlType),   Eq (f LvlTerm))         => Eq (ByCategory f)
deriving instance (Show (f LvlType), Show (f LvlTerm))       => Show (ByCategory f)
deriving instance (Default (f LvlType), Default (f LvlTerm)) => Default (ByCategory f)

newtype ByCategory_ a = ByCategory_ { getByCategory_ :: ByCategory (Const a) }
  deriving stock (Show, Eq, Generic)

instance Functor ByCategory_ where
  fmap f (ByCategory_ x) = ByCategory_ $
    mapWithCategory (applyConst f) (\_ -> applyConst f) x

mapWithCategory_ :: (Category -> a -> b) -> ByCategory_ a -> ByCategory_ b
mapWithCategory_ f (ByCategory_ x) =
  ByCategory_ $ mapWithCategory (applyConst (f CType)) (applyConst . f . CTerm) x

toList :: ByCategory_ a -> [a]
toList (ByCategory_ (ByCategory t s u f g)) = [
      getConst t
    , getConst s
    , getConst u
    , getConst f
    , getConst g
    ]

instance Foldable ByCategory_ where
  foldMap f = foldMap f . toList

instance Semigroup a => Semigroup (ByCategory_ a) where
  (ByCategory_ l) <> (ByCategory_ r) =
    ByCategory_ $ ByCategory {
        cType   = l.cType   <> r.cType
      , cSafe   = l.cSafe   <> r.cSafe
      , cUnsafe = l.cUnsafe <> r.cUnsafe
      , cFunPtr = l.cFunPtr <> r.cFunPtr
      , cGlobal = l.cGlobal <> r.cGlobal
      }

instance Monoid a => Monoid (ByCategory_ a) where
  mempty = ByCategory_ $ ByCategory mempty mempty mempty mempty mempty

isoByCategory :: Iso' (ByCategory_ a) (ByCategory (Const a))
isoByCategory = iso getByCategory_ ByCategory_

isoConst :: Iso' (Const a b) a
isoConst = iso getConst Const

lensForCategory :: Category -> Lens' (ByCategory_ a) a
lensForCategory = \case
    CType     -> isoByCategory % #cType % isoConst
    CTerm cat -> isoByCategory % lensForTermCategory cat % isoConst

lensForTermCategory :: TermCategory -> Lens' (ByCategory f) (f LvlTerm)
lensForTermCategory = \case
  CSafe   -> #cSafe
  CUnsafe -> #cUnsafe
  CFunPtr -> #cFunPtr
  CGlobal -> #cGlobal

-- | A category may contain types or terms.
data CategoryLvl = LvlType | LvlTerm

newtype RenameTerm = RenameTerm (Text -> Text)

instance Show RenameTerm where
  show = const "<RenameTerm>"

instance Default RenameTerm where
  def = RenameTerm id

-- | Include or exclude categories.
--
-- Possibly rename declarations in categories of 'Level' 'LvlTerm'. We only
-- allow renaming of 'LvlTerm' because for 'LvlType' we would also need to
-- rename the use sites, instances etc.
type Choice :: CategoryLvl -> Star
data Choice lvl where
  ExcludeCategory     :: Choice lvl
  IncludeTypeCategory :: Choice LvlType
  IncludeTermCategory :: RenameTerm -> Choice LvlTerm

deriving instance Show (Choice lvl)

instance Default (Choice LvlType) where
  def = IncludeTypeCategory
instance Default (Choice LvlTerm) where
  def = IncludeTermCategory def

-- | Use 'CType', 'CSafe', and 'CGlobal'; do not rename declarations.
useSafeCategory :: ByCategory Choice
useSafeCategory = ByCategory {
      cType   = IncludeTypeCategory
    , cSafe   = IncludeTermCategory def
    , cUnsafe = ExcludeCategory
    , cFunPtr = ExcludeCategory
    , cGlobal = IncludeTermCategory def
    }

-- | Use 'CType', 'CUnsafe', and 'CGlobal'; do not rename declarations.
useUnsafeCategory :: ByCategory Choice
useUnsafeCategory = ByCategory {
      cType   = IncludeTypeCategory
    , cSafe   = ExcludeCategory
    , cUnsafe = IncludeTermCategory def
    , cFunPtr = ExcludeCategory
    , cGlobal = IncludeTermCategory def
    }

applyConst :: (a -> b) -> Const a c -> Const b c
applyConst f = Const . f . getConst
