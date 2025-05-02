{-# LANGUAGE CPP #-}

#include <MachDeps.h>

-- Confusingly, mingw32_HOST_OS is also defined on 64-bit Windows
#ifdef mingw32_HOST_OS
#  if WORD_SIZE_IN_BITS == 64
#    define CExprPlatform C.Expr.Win64
#  else
#    error "C.Expr: Windows: word size must be 64 bits"
#  endif
#else
#  if WORD_SIZE_IN_BITS == 32
#    define CExprPlatform C.Expr.Posix32
#  elif WORD_SIZE_IN_BITS == 64
#    define CExprPlatform C.Expr.Posix64
#  else
#    error "C.Expr: POSIX: word size must be 32 or 64 bits"
#  endif
#endif

module C.Expr.HostPlatform
  ( module C.Operator.Classes
  , module CExprPlatform
  ) where
import C.Operator.Classes
import CExprPlatform
