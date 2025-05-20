{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Player where

import qualified Game.State
import Prelude (IO)

-- #include "game_player.h"
-- void Game.Player_move_player (game_state arg1);

foreign import capi safe "game_player.h move_player" move_player :: Game.State.Game_state -> IO ()
