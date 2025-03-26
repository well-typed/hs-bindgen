module Clang.LowLevel.Core.Pointers (
    CXFile(..)
  ) where

import Foreign

{-------------------------------------------------------------------------------
  CXFile
-------------------------------------------------------------------------------}

-- | A particular source file that is part of a translation unit.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__FILES.html#gacfcea9c1239c916597e2e5b3e109215a>
newtype CXFile = CXFile (Ptr ())
  deriving stock (Show)
  deriving newtype (Storable)
