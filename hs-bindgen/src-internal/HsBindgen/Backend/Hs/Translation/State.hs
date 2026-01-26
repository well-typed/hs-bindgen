module HsBindgen.Backend.Hs.Translation.State (
    HsM
  , runHsM
  , TranslationState(..)
  , emptyTranslationState
  ) where

import Control.Monad.State.Lazy
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.Translation.Instances
import HsBindgen.Imports

newtype HsM a = HsM (State TranslationState a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadState TranslationState)

runHsM :: HsM a -> a
runHsM (HsM m) = evalState m emptyTranslationState

data TranslationState = TranslationState {
      instanceMap :: InstanceMap
    }
  deriving stock (Generic)

emptyTranslationState :: TranslationState
emptyTranslationState = TranslationState Map.empty
