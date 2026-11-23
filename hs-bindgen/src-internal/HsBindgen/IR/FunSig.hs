-- | The arguments and the result of a function
--
-- Intended for unqualified import.
module HsBindgen.IR.FunSig (
    FunSig(..)
  , collectErrors
  ) where

import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Bitraversable (Bitraversable (bitraverse))
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

-- | The signature without errors, or the errors of all positions, in
-- signature order
collectErrors :: FunSig (Either e a) (Either e b) -> Either (NonEmpty e) (FunSig a b)
collectErrors sig = case (partitionEithers sig.args, sig.res) of
    (([]        , argsOk), Right resOk) -> Right FunSig{args = argsOk, res = resOk}
    ((err : errs, _     ), Right _    ) -> Left (err :| errs)
    ((errs      , _     ), Left err   ) ->
      Left $ NonEmpty.prependList errs (err :| [])
