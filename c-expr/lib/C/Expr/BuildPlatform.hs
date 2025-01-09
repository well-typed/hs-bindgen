{-# LANGUAGE CPP #-}

#include <MachDeps.h>

module C.Expr.BuildPlatform
  ( module C.Operator.Classes
  ) where

-- c-expr
import C.Operator.Classes

#ifdef mingw32_HOST_OS

#  if WORD_SIZE_IN_BITS < 32

#    error "C.Expr: Windows: word size must be 64 bits"

#  else

import C.Expr.Win64 ()

#  endif

#else

#  if WORD_SIZE_IN_BITS ==32

import C.Expr.Posix32 ()

#  elif WORD_SIZE_IN_BITS == 64

import C.Expr.Posix64 ()

#  else

#  error "C.Expr: POSIX: word size must be 32 or 64 bits"

#  endif

#endif
