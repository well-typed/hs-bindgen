{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Game.World where

import qualified Game.State
import Prelude (IO)

foreign import capi safe "game_world.h move_world" move_world :: Game.State.Game_state -> IO ()
