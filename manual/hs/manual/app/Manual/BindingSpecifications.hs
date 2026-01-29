module Manual.BindingSpecifications (examples) where

import Foreign as F

import Game.Player.Safe
import Game.State
import Game.World.Safe
import Manual.Tools
import Vector.Length.Safe
import Vector.Rotate.Safe
import Vector.Safe

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "External binding specifications"

    v <- new_vector 2 1
    print =<< peek v
    print =<< vector_length v
    v' <- vector_rotate v (30 * pi / 180)
    print =<< peek v'
    print =<< vector_length v'

    move_world  $ Game_state nullPtr
    move_player $ Game_state nullPtr
