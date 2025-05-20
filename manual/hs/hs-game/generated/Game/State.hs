{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.State where

import qualified Foreign as F
import Prelude (Eq, Ord, Show)

data Game_state_details

newtype Game_state = Game_state
  { un_Game_state :: F.Ptr Game_state_details
  }

deriving newtype instance F.Storable Game_state

deriving stock instance Eq Game_state

deriving stock instance Ord Game_state

deriving stock instance Show Game_state
