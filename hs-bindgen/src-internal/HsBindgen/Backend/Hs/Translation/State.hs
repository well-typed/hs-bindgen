module HsBindgen.Backend.Hs.Translation.State (
    HsM
  , runHsM
  , TranslationState(..)
  , emptyTranslationState
  , modifyInstanceMap
  , modifyInstanceMap'
  ) where

import Control.Monad.State.Lazy
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.Translation.Instances

newtype HsM a = HsM (State TranslationState a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadState TranslationState)

runHsM :: HsM a -> a
runHsM (HsM m) = evalState m emptyTranslationState

data TranslationState = TranslationState {
    instanceMap :: InstanceMap
  }

emptyTranslationState :: TranslationState
emptyTranslationState = TranslationState Map.empty

modifyInstanceMap ::
     MonadState TranslationState m
  => (InstanceMap -> InstanceMap)
  -> m ()
modifyInstanceMap f = modify $ \s ->
    s { instanceMap = f (instanceMap s) }

modifyInstanceMap' ::
     MonadState TranslationState m
  => (InstanceMap -> InstanceMap)
  -> m ()
modifyInstanceMap' f = modify' $ \s ->
    s { instanceMap = f (instanceMap s) }
