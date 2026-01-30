{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Generate FFI wrappers and 'ToFunPtr' class instances for 287 types.
--
module HsBindgen.Runtime.TH.Instances where

import Foreign.C.Types

import HsBindgen.Runtime.Internal.FunPtr.Class
import HsBindgen.Runtime.TH.Types

-- | Generate instances for all @IO a@ functions
--
-- > 17 FFI wrappers
-- > 17 FFI dynamic
-- > 17 ToFunPtr instances
-- > 17 FromFunPtr instances
--
-- Total = 68
--
$(do
  decls <- sequence
    [ generateInstance retTy
    | retTy <- allIOTypes
    ]
  return $ concat decls)

-- | Generate instances for all @a -> IO b@ functions
--
-- > (17 + 17) * 2 = 70 FFI wrappers
-- > (17 + 17) * 2 = 70 FFI dynamic
-- > (17 + 17) * 2 = 70 ToFunPtr instances
-- > (17 + 17) * 2 = 70 FromFunPtr instances
--
-- Total = 280
--
$(do
  decls <- sequence
    [ generateInstance [t| $argTy -> $retTy |]
    | argTy <- allPrimTypes ++ allPtrTypes
    , retTy <- commonReturnTypes
    ]
  return $ concat decls)

-- | Generate instances for @a -> b -> IO c@ functions.
--
-- We can't generate all binary functions because that would mean
--
-- > (17 + 18) * (17 + 18) * 18 = 22050 instances
--
-- Instead we only generate instances for the most common C callback
-- signatures.
--
-- > (5 + 5) * (5 + 5) * 2 = 200 FFI wrappers
-- > (5 + 5) * (5 + 5) * 2 = 200 FFI dynamic
-- > (5 + 5) * (5 + 5) * 2 = 200 ToFunPtr instances
-- > (5 + 5) * (5 + 5) * 2 = 200 FromFunPtr instances
--
-- Total = 800
--
$(do
  decls <- sequence
    [ generateInstance [t| $argTy1 -> $argTy2 -> $retTy |]
    | argTy1 <- commonPrimTypes ++ commonPtrTypes
    , argTy2 <- commonPrimTypes ++ commonPtrTypes
    , retTy <- commonReturnTypes
    ]
  return $ concat decls)
