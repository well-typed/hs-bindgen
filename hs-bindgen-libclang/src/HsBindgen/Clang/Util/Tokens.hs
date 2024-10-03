module HsBindgen.Clang.Util.Tokens (
    Token(..)
  , TokenSpelling(..)
  , clang_tokenize
  ) where

import Control.Exception
import Control.Monad
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal(..))

import HsBindgen.Clang.Util.SourceLoc.Type
import HsBindgen.Clang.Util.SourceLoc qualified as SourceLoc
import HsBindgen.Patterns

import HsBindgen.Clang.Core qualified as Core
import HsBindgen.Clang.Core hiding (
    clang_tokenize
  )

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
     CXTranslationUnit
  -> Range SingleLoc
     -- ^ Range
     --
     -- We use 'Range' 'SingleLoc' here instead of 'CXSourceRange' in order to
     -- avoid ambiguity; see 'HsBindgen.Clang.Util.SourceLoc.Multi' for
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
