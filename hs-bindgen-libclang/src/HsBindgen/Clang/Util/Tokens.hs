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

import HsBindgen.Clang.Util.SourceLoc (SourceRange)
import HsBindgen.Clang.Util.SourceLoc qualified as SourceLoc
import HsBindgen.Patterns

import HsBindgen.Clang.Core qualified as Core
import HsBindgen.Clang.Core hiding (
    clang_tokenize
  )

{-------------------------------------------------------------------------------
  Token extraction and manipulation
-------------------------------------------------------------------------------}

data Token a = Token {
      tokenKind     :: !(SimpleEnum CXTokenKind)
    , tokenSpelling :: !a
    , tokenExtent   :: !SourceRange
    }
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic)
  deriving anyclass (PrettyVal)

newtype TokenSpelling = TokenSpelling {
      getTokenSpelling :: Text
    }
  deriving stock (Show, Eq, Generic)

instance PrettyVal TokenSpelling where
  prettyVal = prettyVal . show

-- | Get all tokens in the specified range
clang_tokenize :: CXTranslationUnit -> CXSourceRange -> IO [Token TokenSpelling]
clang_tokenize unit range = do
    bracket
        (Core.clang_tokenize unit range)
        (uncurry $ Core.clang_disposeTokens unit) $ \(array, numTokens) ->
      forM [0 .. pred numTokens] $ \i -> do
        toToken unit $ index_CXTokenArray array i

toToken :: CXTranslationUnit -> CXToken -> IO (Token TokenSpelling)
toToken unit token = do
    tokenKind     <- clang_getTokenKind token
    tokenSpelling <- TokenSpelling <$> clang_getTokenSpelling unit token
    tokenExtent   <- SourceLoc.clang_getTokenExtent unit token
    return Token{
        tokenKind
      , tokenSpelling
      , tokenExtent
      }
