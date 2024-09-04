module HsBindgen.Clang.Core.Constants (
    sizeof_CXToken
  ) where

#include <clang-c/Index.h>

sizeof_CXToken :: Int
sizeof_CXToken = #size CXToken