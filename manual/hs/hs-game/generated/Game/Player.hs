{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Player where

import qualified Game.State
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include \"game_player.h\"\nvoid GamePlayer_move_player (game_state arg1) { move_player(arg1); }\n")

foreign import ccall safe "GamePlayer_move_player" move_player :: Game.State.Game_state -> IO ()
