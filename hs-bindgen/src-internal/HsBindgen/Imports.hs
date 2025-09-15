{-# OPTIONS_GHC -Wno-orphans #-}
-- | Common imports
module HsBindgen.Imports (
    module X,
    Star,
    singleton,
) where

import Control.Applicative as X (liftA2)
import Control.Exception as X (Exception, bracket, throwIO)
import Control.Monad as X (ap, forM, forM_, guard, unless, void, when)
import Control.Monad.Identity as X (Identity (..))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Control.Monad.IO.Unlift as X (MonadUnliftIO (withRunInIO))
import Data.Array.Byte as X
import Data.Bifunctor as X (Bifunctor (bimap, first, second))
import Data.Coerce as X (coerce)
import Data.Default as X (Default (def))
import Data.Foldable as X (Foldable (foldl', toList), traverse_)
import Data.IntMap.Strict as X (IntMap)
import Data.IntSet as X (IntSet)
import Data.Kind qualified
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Map.Strict as X (Map)
import Data.Maybe as X (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Nat as X (Nat (S, Z))
import Data.Set as X (Set)
import Data.Some as X (Some (..))
import Data.String as X (IsString (fromString))
import Data.Text as X (Text)
import Data.Vec.Lazy as X (Vec (..))
import Data.Void as X (Void, absurd)
import Debug.Trace as X (traceM, traceShow, traceShowId)
import GHC.Generics as X (Generic)
import GHC.Stack as X (HasCallStack)
import GHC.TypeLits as X (Symbol)
import Numeric.Natural as X (Natural)

-- | @Type@ is very clashy name: there's TH.Type, we may want to use Type for
-- representation of C types, etc.
--
-- Let's use Star to refer to Haskell's kind.
type Star = Data.Kind.Type

singleton :: a -> [a]
singleton x = [x]
