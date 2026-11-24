-- | The arguments and the result of a function
--
-- Intended for unqualified import.
module HsBindgen.IR.FunSig (
    FunSig(..)
  , collectLefts
  ) where

import Data.Either (partitionEithers)
import Data.List.NonEmpty qualified as NonEmpty

import HsBindgen.Imports

-- | The arguments and the result of a function
data FunSig a r = FunSig {
      -- | In signature order
      args :: [a]
    , res  :: r
    }
  deriving stock (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance Bifunctor FunSig where
  bimap f g sig = FunSig{args = map f sig.args, res = g sig.res}

instance Bifoldable FunSig where
  bifoldMap f g sig = foldMap f sig.args <> g sig.res

instance Bitraversable FunSig where
  bitraverse f g sig = FunSig <$> traverse f sig.args <*> g sig.res

-- | The signature if all positions are 'Right', or the 'Left's of all
-- positions, in signature order
collectLefts :: FunSig (Either l a) (Either l b) -> Either (NonEmpty l) (FunSig a b)
collectLefts sig = case (partitionEithers sig.args, sig.res) of
    (([]    , argsOk), Right resOk) -> Right FunSig{args = argsOk, res = resOk}
    ((l : ls, _     ), Right _    ) -> Left (l :| ls)
    ((ls    , _     ), Left l     ) ->
      Left $ NonEmpty.prependList ls (l :| [])
