-- | Registering sugar information for Haskell types
--
-- Intended for unqualified import.
module HsBindgen.Frontend.Pass.TranslateTypes.SugarInfo (
    SugarInfos
  , emptySugarInfos
  , RegSugarInfo(..)
  , SugarInfo(..)
  , registerSugarInfo
  , ErrorDuplicate(..)
  , lookupSugarInfo
  , lookupSugarInfoHs
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))

import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs

-- | A mapping of declaration IDs to the type that they desugar to
--
-- A missing entry represents 'MissingSugarInfo'.
data SugarInfos = SugarInfos {
    cMap :: Map C.DeclId RegSugarInfo
  , hsMap :: Map (Hs.Name Hs.NsTypeConstr) RegSugarInfo
  }

emptySugarInfos :: SugarInfos
emptySugarInfos = SugarInfos {
    cMap = Map.empty
  , hsMap = Map.empty
  }

data RegSugarInfo =
    -- | A type was registered as not being sugar
    NotSugar
    -- | A type was registered as being sugar
  | Sugar Hs.Type
  deriving stock (Show, Eq)

data SugarInfo =
    -- | No sugar info was registered for this type
    MissingSugarInfo
    -- | Sugar info was registered for this type
  | RegSugarInfo RegSugarInfo
  deriving stock (Show, Eq)

registerSugarInfo ::
     DeclIdPair
  -> RegSugarInfo
  -> SugarInfos
  -> Either ErrorDuplicate SugarInfos
registerSugarInfo declIdPair ty sugars = do
    cMap' <- Map.alterF f declIdPair.cName sugars.cMap
    hsMap' <- Map.alterF f (Hs.assertNs (Proxy @Hs.NsTypeConstr) declIdPair.hsName) sugars.hsMap
    pure SugarInfos {
        cMap = cMap'
      , hsMap = hsMap'
      }
  where
    f :: Maybe RegSugarInfo -> Either ErrorDuplicate (Maybe RegSugarInfo)
    f Nothing    = Right (Just ty)
    f (Just ty') = Left (ErrorDuplicate ty' ty)

-- | We are registering sugar info more than once for a single declaration ID
data ErrorDuplicate = ErrorDuplicate {
      -- | The sugar info that was already registered
      old :: RegSugarInfo
      -- | The sugar info that we are trying to register
    , new :: RegSugarInfo
    }
  deriving stock (Show, Eq)

lookupSugarInfo :: C.DeclId -> SugarInfos -> SugarInfo
lookupSugarInfo declId sugars = case Map.lookup declId sugars.cMap of
    Nothing -> MissingSugarInfo
    Just ty -> RegSugarInfo ty

lookupSugarInfoHs :: Hs.Name Hs.NsTypeConstr -> SugarInfos -> SugarInfo
lookupSugarInfoHs name sugars = case Map.lookup name sugars.hsMap of
    Nothing -> MissingSugarInfo
    Just ty -> RegSugarInfo ty
