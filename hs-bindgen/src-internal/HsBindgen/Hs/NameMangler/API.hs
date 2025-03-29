module HsBindgen.Hs.NameMangler.API (
    -- * Definition
    NameMangler'(..)
  , NameSpec(..)
    -- * Dealing with errors
  , NameManglerErr(..)
  , NameMangler
  , mangle
  ) where

import Control.Exception

import HsBindgen.C.AST
import HsBindgen.Hs.AST.Name

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Name mangler functions
newtype NameMangler' m = NameMangler {
      mangle' :: forall ns. SingNamespace ns => NameSpec ns -> m (HsName ns)
    }

-- | Specification of the Haskell name we're trying to create
data NameSpec ns where
  -- | Top-level variable (e.g. a function name)
  NameVar :: CName -> NameSpec NsVar

  -- | Field of a struct or union
  NameField :: DeclPath -> CName -> NameSpec NsVar

  -- | Type constructor
  NameTycon :: DeclPath -> NameSpec NsTypeConstr

  -- | Data constructor
  NameDatacon :: DeclPath -> NameSpec NsConstr

  -- | Destructor name
  --
  -- > data Tycon = Datacon { decon :: ... }
  NameDecon :: DeclPath -> NameSpec NsVar

  -- | Union getter
  NameGetter :: DeclPath -> CName -> NameSpec NsVar

  -- | Union builder
  NameBuilder :: DeclPath -> CName -> NameSpec NsVar

deriving stock instance Show (NameSpec ns)
deriving stock instance Eq   (NameSpec ns)
deriving stock instance Ord  (NameSpec ns)

{-------------------------------------------------------------------------------
  Dealing with errors
-------------------------------------------------------------------------------}

data NameManglerErr where
    -- | We were unable to produce a name, and therefore need a user override
    RequireOverride :: NameSpec ns -> NameManglerErr

deriving stock instance Show NameManglerErr

instance Exception NameManglerErr where
  displayException = \case
      RequireOverride spec -> concat [
          "Unable to produce a name for " ++ show spec ++ ". "
        , "Please provide an override."
        ]

type NameMangler = NameMangler' (Either NameManglerErr)

-- | Run the default NameMangler monad
mangle :: SingNamespace ns => NameMangler -> NameSpec ns -> HsName ns
mangle nm spec =
    case mangle' nm spec of
      Left  err  -> throw err
      Right name -> name

