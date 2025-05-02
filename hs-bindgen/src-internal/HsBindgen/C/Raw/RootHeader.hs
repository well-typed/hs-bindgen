-- | Root header (header that includes all headers to be processed)
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.Raw.RootHeader qualified as RootHeader
module HsBindgen.C.Raw.RootHeader (
    name
  , content
  ) where

import Clang.Paths

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

name :: SourcePath
name = SourcePath "hs-bindgen-root.h"

content :: [CHeaderIncludePath] -> String
content = unlines . map toLine
  where
    toLine :: CHeaderIncludePath -> String
    toLine = \case
      CHeaderSystemIncludePath path -> "#include <" ++ path ++ ">"
      CHeaderQuoteIncludePath  path -> "#include \"" ++ path ++ "\""

