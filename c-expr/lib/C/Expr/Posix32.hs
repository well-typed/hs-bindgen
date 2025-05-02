{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-unused-matches #-}

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
