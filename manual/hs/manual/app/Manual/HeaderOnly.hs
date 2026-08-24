module Manual.HeaderOnly (examples) where

import Text.Printf

import HeaderOnly.Unsafe
import Manual.Tools

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

-- | The bindings are generated without @HEADER_ONLY_IMPLEMENTATION@, so this
-- call resolves to the symbol in @cbits/header_only.c@.
examples :: IO ()
examples = do
    section "Header-only library"

    horns <- header_only_horns
    printf "header_only_horns() = %d\n" (toInteger horns)
