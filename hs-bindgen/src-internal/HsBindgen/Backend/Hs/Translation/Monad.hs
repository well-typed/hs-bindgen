-- |
--
-- Intended for qualified import.
--
-- > import HsBindgen.Backend.Hs.Translation.Monad (HsM)
-- > import HsBindgen.Backend.Hs.Translation.Monad qualified as HsM
--
module HsBindgen.Backend.Hs.Translation.Monad (
    HsM
  , runHsM
  , St(..)
  , initSt
  , Env(..)
  , initEnv
  ) where

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.Translation.Instances
import HsBindgen.Config.Prelims
import HsBindgen.Frontend.Pass.TranslateTypes.SugarInfo (SugarInfos)
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst

newtype HsM a = HsM (ReaderT Env (State St) a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadState St)
  deriving newtype (MonadReader Env)

runHsM ::
     Env
  -> HsM a
  -> a
runHsM env (HsM m) =
    evalState (runReaderT m env) initSt

data St = St {
      instanceMap :: InstanceMap
    }
  deriving stock (Generic)

initSt :: St
initSt = St Map.empty

data Env = Env {
    uniqueId           :: UniqueId
  , baseModuleName     :: BaseModuleName
  , supportedInstances :: Inst.SupportedInstances
  , sugars             :: SugarInfos
  }

initEnv ::
     UniqueId
  -> BaseModuleName
  -> Inst.SupportedInstances
  -> SugarInfos
  -> Env
initEnv uniqueId baseModuleName supportedInstances sugars =
    Env {
        uniqueId           = uniqueId
      , baseModuleName     = baseModuleName
      , supportedInstances = supportedInstances
      , sugars             = sugars
      }
