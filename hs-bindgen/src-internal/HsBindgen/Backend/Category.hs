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
    -- * Binding category choices
  , RenameTerm(..)
  , Choice(..)
  , useSafeCategory
  , useUnsafeCategory
  , useFunPtrCategory
    -- * Boot files
  , HsBootPolicy(..)
  ) where

import Data.Functor.Const (Const (..))
import Optics.Core (Lens', iso)
import Optics.Iso (Iso')

import HsBindgen.Backend.Level
import HsBindgen.Imports hiding (toList)

{-------------------------------------------------------------------------------
  Boot files
-------------------------------------------------------------------------------}

-- | Whether to generate a boot file for a types module
--
-- With 'GenerateHsBoot', a types module gets a boot file, and the code behind
-- its callbacks goes to a callback module. With 'DoNotGenerateHsBoot', that
-- code stays in the types module and there is no boot file.
--
-- In library mode, the plan sets this per unit: 'GenerateHsBoot' for a unit
-- whose header is in an import cycle, 'DoNotGenerateHsBoot' for every other.
-- Single-header mode always uses 'DoNotGenerateHsBoot'.
data HsBootPolicy =
    GenerateHsBoot
  | DoNotGenerateHsBoot
  deriving stock (Show, Eq, Generic)

data TermCategory =
    -- | The code behind callbacks: the @"wrapper"@ and @"dynamic"@ imports, the
    -- functions around them that convert arguments and results with @toFFIType@
    -- and @fromFFIType@, and the @ToFunPtr@ and @FromFunPtr@ instances.
    --
    -- The FFI conversions need the @FFIType@ equations of the argument and
    -- result types, which may come from other modules. A boot file cannot
    -- carry type family equations, so a module that sees those types through a
    -- boot file cannot reduce @FFIType@ at them. The solution is to leave
    -- this code to a callback module when a boot file is generated.
    --
    -- Types modules get boot files in library mode when they form an import
    -- cycle. The global callback module of
    -- library mode uses this category too. Otherwise this category is empty
    -- and everything stays in 'CType'. In Template Haskell mode it is always
    -- empty.
    CCallback
    -- | Foreign import bindings with a @safe@ foreign import modifier.
  | CSafe
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

allCategories :: NonEmpty Category
allCategories = CType :| map CTerm [minBound .. maxBound]

-- | Like 'Data.Map.Strict.mapWithKey'.
mapWithCategory ::
     (f LvlType -> g LvlType)
  -> (TermCategory -> f LvlTerm -> g LvlTerm)
  -> ByCategory f
  -> ByCategory g
mapWithCategory f g x = ByCategory {
      cType     = f           x.cType
    , cCallback = g CCallback x.cCallback
    , cSafe     = g CSafe     x.cSafe
    , cUnsafe   = g CUnsafe   x.cUnsafe
    , cFunPtr   = g CFunPtr   x.cFunPtr
    , cGlobal   = g CGlobal   x.cGlobal
    }

-- | A strict, total map from 'Category' to @a@.
type ByCategory :: (Level -> Star) -> Star
data ByCategory f = ByCategory {
      cType     :: !(f LvlType)
    , cCallback :: !(f LvlTerm)
    , cSafe     :: !(f LvlTerm)
    , cUnsafe   :: !(f LvlTerm)
    , cFunPtr   :: !(f LvlTerm)
    , cGlobal   :: !(f LvlTerm)
  }
  deriving stock (Generic)

deriving instance (Eq (f LvlType),   Eq (f LvlTerm))         => Eq (ByCategory f)
deriving instance (Show (f LvlType), Show (f LvlTerm))       => Show (ByCategory f)
deriving instance (Default (f LvlType), Default (f LvlTerm)) => Default (ByCategory f)

newtype ByCategory_ a = ByCategory_ (ByCategory (Const a))
  deriving stock (Show, Eq, Generic)

getByCategory_ :: ByCategory_ a -> ByCategory (Const a)
getByCategory_ (ByCategory_ x) = x

instance Functor ByCategory_ where
  fmap f (ByCategory_ x) = ByCategory_ $
    mapWithCategory (applyConst f) (\_ -> applyConst f) x

mapWithCategory_ :: (Category -> a -> b) -> ByCategory_ a -> ByCategory_ b
mapWithCategory_ f (ByCategory_ x) =
  ByCategory_ $ mapWithCategory (applyConst (f CType)) (applyConst . f . CTerm) x

toList :: ByCategory_ a -> [a]
toList (ByCategory_ (ByCategory t c s u f g)) = [
      getConst t
    , getConst c
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
        cType     = l.cType     <> r.cType
      , cCallback = l.cCallback <> r.cCallback
      , cSafe     = l.cSafe     <> r.cSafe
      , cUnsafe   = l.cUnsafe   <> r.cUnsafe
      , cFunPtr   = l.cFunPtr   <> r.cFunPtr
      , cGlobal   = l.cGlobal   <> r.cGlobal
      }

instance Monoid a => Monoid (ByCategory_ a) where
  mempty = ByCategory_ $ ByCategory mempty mempty mempty mempty mempty mempty

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
  CCallback -> #cCallback
  CSafe     -> #cSafe
  CUnsafe   -> #cUnsafe
  CFunPtr   -> #cFunPtr
  CGlobal   -> #cGlobal

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
type Choice :: Level -> Star
data Choice lvl where
  ExcludeCategory     :: Choice lvl
  IncludeTypeCategory :: Choice LvlType
  IncludeTermCategory :: RenameTerm -> Choice LvlTerm

deriving instance Show (Choice lvl)

instance Semigroup (Choice LvlType) where
  ExcludeCategory <> y = y
  x <> ExcludeCategory = x
  IncludeTypeCategory <> IncludeTypeCategory = IncludeTypeCategory

instance Semigroup (Choice LvlTerm) where
  ExcludeCategory <> y = y
  x <> ExcludeCategory = x
  IncludeTermCategory _r1 <> IncludeTermCategory r2 = IncludeTermCategory r2

instance Monoid (Choice LvlType) where
  mempty = ExcludeCategory

instance Monoid (Choice LvlTerm) where
  mempty = ExcludeCategory

instance Default (Choice LvlType) where
  def = IncludeTypeCategory
instance Default (Choice LvlTerm) where
  def = IncludeTermCategory def

-- | Use 'CType', 'CCallback', 'CSafe', and 'CGlobal'; do not rename
-- declarations.
useSafeCategory :: ByCategory Choice
useSafeCategory = ByCategory {
      cType     = IncludeTypeCategory
    , cCallback = IncludeTermCategory def
    , cSafe     = IncludeTermCategory def
    , cUnsafe   = ExcludeCategory
    , cFunPtr   = ExcludeCategory
    , cGlobal   = IncludeTermCategory def
    }

-- | Use 'CType', 'CCallback', 'CUnsafe', and 'CGlobal'; do not rename
-- declarations.
useUnsafeCategory :: ByCategory Choice
useUnsafeCategory = ByCategory {
      cType     = IncludeTypeCategory
    , cCallback = IncludeTermCategory def
    , cSafe     = ExcludeCategory
    , cUnsafe   = IncludeTermCategory def
    , cFunPtr   = ExcludeCategory
    , cGlobal   = IncludeTermCategory def
    }

-- | Use 'CType', 'CCallback', 'CFunPtr', and 'CGlobal'; do not rename
-- declarations.
useFunPtrCategory :: ByCategory Choice
useFunPtrCategory = ByCategory {
      cType     = IncludeTypeCategory
    , cCallback = IncludeTermCategory def
    , cSafe     = ExcludeCategory
    , cUnsafe   = ExcludeCategory
    , cFunPtr   = IncludeTermCategory def
    , cGlobal   = IncludeTermCategory def
    }

applyConst :: (a -> b) -> Const a c -> Const b c
applyConst f = Const . f . getConst
