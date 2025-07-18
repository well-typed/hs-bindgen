{-# OPTIONS_GHC -Wno-orphans #-}
-- | Common imports
module HsBindgen.Imports (
    module X,
    Star,
    singleton,
) where

import Data.Kind qualified

import Control.Applicative as X (liftA2)
import Control.Exception as X (Exception, throwIO, bracket)
import Control.Monad as X (void, ap, forM, forM_, guard, when, unless)
import Control.Monad.Identity as X (Identity (..))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Control.Monad.IO.Unlift as X (MonadUnliftIO (withRunInIO))
import Data.Bifunctor as X (Bifunctor (bimap, first, second))
import Data.Coerce as X (coerce)
import Data.Default as X (Default (def))
import Data.Foldable as X (Foldable (foldl', toList), traverse_)
import Data.Maybe as X (catMaybes, mapMaybe, fromMaybe, isJust, isNothing)
import Data.Some as X (Some (..))
import Data.String as X (IsString (fromString))
import Data.Void as X (Void, absurd)
import GHC.Generics as X (Generic)
import GHC.Stack as X (HasCallStack)
import GHC.TypeLits as X (Symbol)
import Numeric.Natural as X (Natural)

-- types
import Data.IntMap.Strict as X (IntMap)
import Data.IntSet as X (IntSet)
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Map.Strict as X (Map)
import Data.Nat as X (Nat (Z, S))
import Data.Set as X (Set)
import Data.Text as X (Text)
import Data.Vec.Lazy as X (Vec (..))

-- these are nice to be always available while developing,
-- without needing to add/remove imports.
import Debug.Trace as X (traceShowId, traceShow, traceM)
import HsBindgen.Debug as X
import Data.Array.Byte as X

-- | @Type@ is very clashy name: there's TH.Type, we may want to use Type for
-- representation of C types, etc.
--
-- Let's use Star to refer to Haskell's kind.
type Star = Data.Kind.Type

singleton :: a -> [a]
singleton x = [x]
