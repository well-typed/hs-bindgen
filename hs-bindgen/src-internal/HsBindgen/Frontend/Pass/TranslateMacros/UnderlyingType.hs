-- | Underlying type registration
--
-- Intended for unqualified import.
module HsBindgen.Frontend.Pass.TranslateMacros.UnderlyingType (
    UnderlyingTypes
  , RegisteredUnderlyingType(..)
  , UnderlyingType(..)
  , registerUnderlyingType
  , ErrorDuplicate(..)
  , lookupUnderlyingType
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs

-- | A mapping of declaration IDs to their underlying types (if applicable)
--
-- A missing entry represents 'MissingUnderlyingType'.
type UnderlyingTypes = Map C.DeclId RegisteredUnderlyingType

data RegisteredUnderlyingType =
    -- | A type was registered as having no underlying type
    NoUnderlyingType
    -- | A type was registered as having an underlying type
  | UnderlyingType Hs.Type
  deriving stock (Show, Eq)

data UnderlyingType =
    -- | No underlying type information was registered for this type
    MissingUnderlyingType
    -- | Underlying type information was registered for this type
  | RegisteredUnderlyingType RegisteredUnderlyingType
  deriving stock (Show, Eq)

registerUnderlyingType ::
     C.DeclId
  -> RegisteredUnderlyingType
  -> UnderlyingTypes
  -> Either ErrorDuplicate UnderlyingTypes
registerUnderlyingType declId ty tys = Map.alterF f declId tys
  where
    f :: Maybe RegisteredUnderlyingType -> Either ErrorDuplicate (Maybe RegisteredUnderlyingType)
    f Nothing    = Right (Just ty)
    f (Just ty') = Left (ErrorDuplicate ty' ty)

-- | We are registering underlying type information more than once for a single
-- declaration ID
data ErrorDuplicate = ErrorDuplicate {
      -- | The underlying type that was already registered
      old :: RegisteredUnderlyingType
      -- | The underlying type that we are trying to register
    , new :: RegisteredUnderlyingType
    }
  deriving stock (Show, Eq)

lookupUnderlyingType :: C.DeclId -> UnderlyingTypes -> UnderlyingType
lookupUnderlyingType declId tys = case Map.lookup declId tys of
    Nothing -> MissingUnderlyingType
    Just ty -> RegisteredUnderlyingType ty
