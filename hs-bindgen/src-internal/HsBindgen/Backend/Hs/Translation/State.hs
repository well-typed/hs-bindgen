module HsBindgen.Backend.Hs.Translation.State (
    TranslationState(..)
  , emptyTranslationState
  , modifyInstanceMap
  , modifyInstanceMap'
  , modifyNewtypeMap
  , modifyNewtypeMap'
  ) where

import Control.Monad.State.Class (MonadState (..), modify, modify')
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.Translation.Instances
import HsBindgen.Backend.Hs.Translation.Type

data TranslationState = TranslationState {
    instanceMap :: InstanceMap
  , newtypeMap :: NewtypeMap
  }

emptyTranslationState :: TranslationState
emptyTranslationState = TranslationState Map.empty Map.empty

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

modifyNewtypeMap ::
     MonadState TranslationState m
  => (NewtypeMap -> NewtypeMap)
  -> m ()
modifyNewtypeMap f = modify $ \s ->
    s { newtypeMap = f (newtypeMap s) }

modifyNewtypeMap' ::
     MonadState TranslationState m
  => (NewtypeMap -> NewtypeMap)
  -> m ()
modifyNewtypeMap' f = modify' $ \s ->
    s { newtypeMap = f (newtypeMap s) }
