module HsBindgen.Backend.Hs.Translation.State (
    HsM
  , runHsM
  , TranslationState(..)
  , emptyTranslationState
  , modifyInstanceMap
  , modifyInstanceMap'
  , modifyNewtypeMap
  , modifyNewtypeMap'
  ) where

import Control.Monad.State.Lazy
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.Translation.Instances
import HsBindgen.Backend.Hs.Translation.Type

newtype HsM a = HsM (State TranslationState a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadState TranslationState)

runHsM :: HsM a -> a
runHsM (HsM m) = evalState m emptyTranslationState

data TranslationState = TranslationState {
    -- | A mapping of type names to their class instances
    instanceMap :: InstanceMap
    -- | A mapping of newtype names to their underlying type
    --
    -- If a name is not in the mapping, then it is assumed not to be a newtype.
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
