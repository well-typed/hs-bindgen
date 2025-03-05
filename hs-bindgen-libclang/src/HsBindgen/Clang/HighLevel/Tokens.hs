module HsBindgen.Clang.HighLevel.Tokens (
    Token(..)
  , TokenSpelling(..)
  , clang_tokenize
  ) where

import Control.Exception
import Control.Monad
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Stack
import Text.Show.Pretty (PrettyVal(..))

import HsBindgen.Clang.HighLevel.SourceLoc (Range, MultiLoc, SingleLoc)
import HsBindgen.Clang.HighLevel.SourceLoc qualified as SourceLoc
import HsBindgen.Runtime.Enum.Simple

import HsBindgen.Clang.LowLevel.Core qualified as Core
import HsBindgen.Clang.LowLevel.Core hiding (clang_tokenize)

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
  deriving anyclass (PrettyVal)

newtype TokenSpelling = TokenSpelling {
      getTokenSpelling :: Text
    }
  deriving stock (Show, Eq, Generic)

instance PrettyVal TokenSpelling where
  prettyVal = prettyVal . show

{-------------------------------------------------------------------------------
  Extraction
-------------------------------------------------------------------------------}

-- | Get all tokens in the specified range
clang_tokenize ::
     HasCallStack
  => CXTranslationUnit
  -> Range SingleLoc
     -- ^ Range
     --
     -- We use 'Range' 'SingleLoc' here instead of 'CXSourceRange' in order to
     -- avoid ambiguity; see 'HsBindgen.Clang.HighLevel.SourceLoc.Multi' for
     -- discussion.
  -> IO [Token TokenSpelling]
clang_tokenize unit range = do
    range' <- SourceLoc.fromRange unit range
    bracket
        (Core.clang_tokenize unit range')
        (uncurry $ Core.clang_disposeTokens unit) $ \(tokens, numTokens) -> do
      cursors <- clang_annotateTokens unit tokens numTokens
      forM [0 .. pred numTokens] $ \i -> do
        cursor <- index_CXCursorArray cursors i
        toToken unit (index_CXTokenArray tokens i) cursor

toToken :: CXTranslationUnit -> CXToken -> CXCursor -> IO (Token TokenSpelling)
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
