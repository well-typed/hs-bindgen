{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.World where

import qualified Game.State
import Prelude (IO)

-- #include "game_world.h"
-- void Game.World_move_world (game_state arg1);

foreign import capi safe "game_world.h move_world" move_world :: Game.State.Game_state -> IO ()
