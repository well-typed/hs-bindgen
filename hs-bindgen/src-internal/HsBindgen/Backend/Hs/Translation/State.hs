module HsBindgen.Backend.Hs.Translation.State (
    TranslationState(..)
  , emptyTranslationState
  , modifyInstanceMap
  , modifyInstanceMap'
  ) where

import Control.Monad.State.Class (MonadState (..), modify, modify')
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.Translation.Instances

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
