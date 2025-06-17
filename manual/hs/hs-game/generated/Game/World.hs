{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.World where

import qualified Game.State
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include \"game_world.h\"\nvoid GameWorld_move_world (game_state arg1) { move_world(arg1); }\n")

foreign import ccall safe "GameWorld_move_world" move_world :: Game.State.Game_state -> IO ()
