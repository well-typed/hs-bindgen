{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Function pointer utilities and type class for converting Haskell functions
-- to C function pointers.
--
-- This module provides a type class 'ToFunPtr' that allows for a uniform
-- interface to convert Haskell functions to C function pointers.
--
-- This module also provides TH splices that generate FFI wrappers and
-- 'ToFunPtr' instances that are called in different modules to paralellize
-- compilation and compile time code generation.
--
module HsBindgen.Runtime.FunPtr.Common (
    -- * Type class
    ToFunPtr(..)
  , FromFunPtr(..)

    -- * Template Haskell FFI wrapper generation and 'ToFunPtr' instance
    -- generation

    -- ** Types
  , allPrimTypes
  , allPtrTypes
  , allIOTypes
    -- *** Common Types
  , commonPrimTypes
  , commonPtrTypes
  , commonReturnTypes
    -- ** Generate the instances
  , generateInstance
  ) where

import Foreign qualified as F
import GHC.Ptr qualified as Ptr
import Language.Haskell.TH
import Foreign.C.Types
import Data.Void (Void)

-- | Type class for converting Haskell functions to C function pointers.
--
class ToFunPtr a where
  -- | Convert a Haskell function to a C function pointer.
  --
  -- The caller is responsible for freeing the function pointer using
  -- 'F.freeHaskellFunPtr' when it is no longer needed.
  --
  toFunPtr :: a -> IO (F.FunPtr a)

-- | Type class for converting C function pointers to Haskell functions.
--
class FromFunPtr a where
  -- | Convert C function pointer into a Haskell function.
  fromFunPtr :: F.FunPtr a -> a


-- | Get primitive marshallable types
--
allPrimTypes :: [Q Type]
allPrimTypes =
  [ [t| CChar      |]
  , [t| CSChar     |]
  , [t| CUChar     |]
  , [t| CInt       |]
  , [t| CUInt      |]
  , [t| CShort     |]
  , [t| CUShort    |]
  , [t| CLong      |]
  , [t| CULong     |]
  , [t| CPtrdiff   |]
  , [t| CSize      |]
  , [t| CLLong     |]
  , [t| CULLong    |]
  , [t| CBool      |]
  , [t| CFloat     |]
  , [t| CDouble    |]
  , [t| Int        |]
  ]

-- | Get pointer types for all primitive types
--
allPtrTypes :: [Q Type]
allPtrTypes = [t| Ptr.Ptr Void |]
            : [ [t| Ptr.Ptr $t |]
              | t <- allPrimTypes
              ]

-- | Get IO types for all primitive types
--
allIOTypes :: [Q Type]
allIOTypes = [t| IO () |]
           : [ [t| IO $t |]
             | t <- allPrimTypes
             ]

-- | A list of the most common primitive types found in C callbacks.
--
commonPrimTypes :: [Q Type]
commonPrimTypes =
  [ [t| CChar   |]
  , [t| CInt    |]
  , [t| CUInt   |]
  , [t| CDouble |]
  , [t| CFloat  |]
  ]

-- | Common pointer types, including the crucial @void*@ and @char*@.
--
commonPtrTypes :: [Q Type]
commonPtrTypes =
  [ [t| Ptr.Ptr Void  |]
  , [t| Ptr.Ptr CChar |]
  , [t| Ptr.Ptr CInt  |]
  ]

-- | The most common return types for callbacks are @void@ and integer status
-- codes.
--
commonReturnTypes :: [Q Type]
commonReturnTypes =
  [ [t| IO ()   |]
  , [t| IO CInt |]
  ]

-- | Generate a foreign import wrapper and 'ToFunPtr' instance for a
-- particular type
--
generateInstance :: Q Type -> Q [Dec]
generateInstance funTyQ = do
  funTy <- funTyQ

  wrapperName <- newName ("wrapper_" ++ sanitizeTypeName (show funTy))
  dynamicName <- newName ("dynamic_" ++ sanitizeTypeName (show funTy))

  foreignImportWrapper <- forImpD CCall
                                  Safe
                                  "wrapper"
                                  wrapperName
                                  [t| $funTyQ -> IO (F.FunPtr $funTyQ) |]

  foreignImportDynamic <- forImpD CCall
                                  Safe
                                  "dynamic"
                                  dynamicName
                                  [t| F.FunPtr $funTyQ -> $funTyQ |]

  toFunPtrInstance <- instanceD (return [])
                                [t| ToFunPtr $funTyQ |]
                                [ funD (mkName "toFunPtr")
                                  [ clause [] (normalB (varE wrapperName)) []
                                  ]
                                ]

  fromFunPtrInstance <- instanceD (return [])
                                  [t| FromFunPtr $funTyQ |]
                                  [ funD (mkName "fromFunPtr")
                                    [ clause [] (normalB (varE dynamicName)) []
                                    ]
                                  ]
  return [ foreignImportWrapper
         , foreignImportDynamic
         , toFunPtrInstance
         , fromFunPtrInstance
         ]
  where
    -- | Sanitize type name for use in generated names
    sanitizeTypeName :: String -> String
    sanitizeTypeName = concatMap sanitizeChar
      where
        sanitizeChar c
          | c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) = [c]
          | otherwise = "_"

