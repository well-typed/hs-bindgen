{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-unused-matches #-}

-- Some options to make this module faster to compile
{-# OPTIONS_GHC -O0 -fmax-pmcheck-models=1 #-}

module C.Expr.Posix32
  ( module C.Operator.Classes
  , module C.Expr.Posix32
  ) where

-- c-expr
import C.Type ( Platform(..), WordWidth(..), OS(..) )
import C.Operator.Classes
import C.Operator.GenInstances
  ( cExprInstances )

--------------------------------------------------------------------------------

$( cExprInstances
    ( Platform
        { platformWordWidth = WordWidth32
        , platformOS        = Posix
        }
    ) )
