-- | Architecture-dependent struct sizes computed by hsc2hs.
--
-- This module demonstrates hsc2hs usage in a cross-compilation context.
-- hsc2hs compiles a small C program to determine struct sizes and
-- alignments, so during cross-compilation it needs the target's C
-- compiler. Nix's cross-GHC ships hsc2hs under a target-prefixed name
-- (e.g. @aarch64-unknown-linux-gnu-hsc2hs@) that cabal does not discover
-- automatically — pass it explicitly with @--with-hsc2hs@.

module ArchSizes
    ( archInfoSize
    , archInfoAlignment
    , pointerArraySize
    , nestedStructSize
    ) where

#include "arch_types.h"

archInfoSize :: Int
archInfoSize = #{size struct ArchInfo}

archInfoAlignment :: Int
archInfoAlignment = #{alignment struct ArchInfo}

pointerArraySize :: Int
pointerArraySize = #{size struct PointerArray}

nestedStructSize :: Int
nestedStructSize = #{size struct NestedStruct}
