{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Game.Player where

import qualified Game.State
import Prelude (IO)

foreign import capi safe "game_player.h move_player" move_player :: Game.State.Game_state -> IO ()
