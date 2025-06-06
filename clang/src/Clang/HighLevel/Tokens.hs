module Clang.HighLevel.Tokens (
    Token(..)
  , TokenSpelling(..)
  , clang_tokenize
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Stack

import Clang.Enum.Simple
import Clang.HighLevel.SourceLoc (Range, MultiLoc, SingleLoc)
import Clang.HighLevel.SourceLoc qualified as SourceLoc
import Clang.LowLevel.Core qualified as Core
import Clang.LowLevel.Core hiding (clang_tokenize)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Token a = Token {
      tokenKind       :: !(SimpleEnum CXTokenKind)
    , tokenSpelling   :: !a
    , tokenExtent     :: !(Range MultiLoc)
    , tokenCursorKind :: !(SimpleEnum CXCursorKind)
    }
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic)

newtype TokenSpelling = TokenSpelling {
      getTokenSpelling :: Text
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Extraction
-------------------------------------------------------------------------------}

-- | Get all tokens in the specified range
clang_tokenize ::
     (MonadIO m, HasCallStack)
  => CXTranslationUnit
  -> Range SingleLoc
     -- ^ Range
     --
     -- We use 'Range' 'SingleLoc' here instead of 'CXSourceRange' in order to
     -- avoid ambiguity; see 'Clang.HighLevel.SourceLoc.Multi' for discussion.
  -> m [Token TokenSpelling]
clang_tokenize unit range = liftIO $ do
    range' <- SourceLoc.fromRange unit range
    bracket
        (Core.clang_tokenize unit range')
        (uncurry $ Core.clang_disposeTokens unit) $ \(tokens, numTokens) -> do
      cursors <- clang_annotateTokens unit tokens numTokens
      forM [0 .. pred numTokens] $ \i -> do
        cursor <- index_CXCursorArray cursors i
        toToken unit (index_CXTokenArray tokens i) cursor

toToken ::
     MonadIO m
  => CXTranslationUnit -> CXToken -> CXCursor -> m (Token TokenSpelling)
toToken unit token cursor = do
    tokenKind       <- clang_getTokenKind token
    tokenSpelling   <- TokenSpelling <$> clang_getTokenSpelling unit token
    tokenExtent     <- SourceLoc.clang_getTokenExtent unit token
    tokenCursorKind <- clang_getCursorKind cursor
    return Token{
        tokenKind
      , tokenSpelling
      , tokenExtent
      , tokenCursorKind
      }
