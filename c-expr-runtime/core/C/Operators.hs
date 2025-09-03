module C.Operators
  ( -- * C operators and their types
    Op(..), UnaryOp(..), BinaryOp(..)
  , pprOp, pprOpApp
  , opResType

  ) where

-- vec
import Data.Vec.Lazy

-- c-expr
import C.Type
import C.Operator.Internal


--------------------------------------------------------------------------------

-- | Compute the result type of a C operator applied to
-- arguments of the given types.
opResType :: Eq a
          => Platform
          -> Op arity             -- ^ C operator
          -> Vec arity ( Type a ) -- ^ types of its arguments
          -> Maybe ( Type a )
opResType plat op args =
  fst <$> opResTypeAndImpl plat op args
