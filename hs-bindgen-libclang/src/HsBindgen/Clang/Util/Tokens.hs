module HsBindgen.Clang.Util.Tokens (
    clang_tokenize
  ) where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)

import HsBindgen.Clang.Core qualified as Core
import HsBindgen.Clang.Core hiding (
    clang_tokenize
  )

{-------------------------------------------------------------------------------
  Token extraction and manipulation
-------------------------------------------------------------------------------}

-- | Get all tokens in the specified range
clang_tokenize :: CXTranslationUnit -> CXSourceRange -> IO [ByteString]
clang_tokenize unit range = do
    bracket
        (Core.clang_tokenize unit range)
        (uncurry $ Core.clang_disposeTokens unit) $ \(array, numTokens) ->
      forM [0 .. pred numTokens] $ \i ->
        clang_getTokenSpelling unit (index_CXTokenArray array i)