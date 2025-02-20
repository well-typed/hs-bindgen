{-# LANGUAGE CPP #-}

#include <MachDeps.h>

module C.Expr.HostPlatform
  ( module C.Operator.Classes
  ) where

-- c-expr
import C.Operator.Classes

-- Confusingly, mingw32_HOST_OS is also defined on 64-bit Windows
#ifdef mingw32_HOST_OS

#  if WORD_SIZE_IN_BITS == 64
import C.Expr.Win64 ()
#  else
#    error "C.Expr: Windows: word size must be 64 bits"
#  endif

#else

#  if WORD_SIZE_IN_BITS == 32
import C.Expr.Posix32 ()
#  elif WORD_SIZE_IN_BITS == 64
import C.Expr.Posix64 ()
#  else
#    error "C.Expr: POSIX: word size must be 32 or 64 bits"
#  endif

#endif
