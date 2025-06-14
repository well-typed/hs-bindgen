-- | Monad for parsing types
--
-- Intended for unqualified import (unless context is unambiguous).
--
-- > import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseType)
-- > import HsBindgen.Frontend.Pass.Parse.Type.Monad qualified as ParseType
module HsBindgen.Frontend.Pass.Parse.Type.Monad (
    ParseType -- opaque
  , run
    -- * Errors
  , unknownTypeKind
  ) where

import Control.Monad.IO.Class
import Data.Text qualified as Text

import Clang.Enum.Simple
import Clang.LowLevel.Core
import HsBindgen.Errors
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition

  Parsing declarations and parsing types have different requirements, so each
  gets its own monad.
-------------------------------------------------------------------------------}

newtype ParseType a = Wrap {
      unwrap :: IO a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadIO
    )

run :: MonadIO m => ParseType a -> m a
run = liftIO . unwrap

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Unknown type
--
-- TODO: It would be better if we could report a source location here, but clang
-- does not make it easy to associate a 'CXType' with a location. Perhaps it
-- would be possible to add a 'SingleLoc' argument to 'fromCXType', but it might
-- be hard to get right in all cases, and a /wrong/ source location might be
-- worse than none at all.
unknownTypeKind ::
     (MonadIO m, HasCallStack)
  => CXTypeKind -> CXType -> m x
unknownTypeKind kind _ = do
    spelling <- clang_getTypeKindSpelling (simpleEnum kind)
    panicIO $ concat [
        "Unknown cursor of kind "
      , show kind
      , " ("
      , Text.unpack spelling
      , ")"
      ]
