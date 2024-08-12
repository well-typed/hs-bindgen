{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Types where

-- base
import Foreign
import Foreign.C.Types

-- containers
import qualified Data.Map as Map

-- inline-c
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

{-------------------------------------------------------------------------------
  Context
-------------------------------------------------------------------------------}

type FunPtr_Void_Int = FunPtr (CInt -> IO ())

data HaskellStruct = HaskellStruct {
      haskellStructA :: CInt
    , haskellStructB :: CInt
    }

instance Storable HaskellStruct where
  sizeOf    _ = 8
  alignment _ = 4

  peek s = do
      haskellStructA <- peekByteOff s 0
      haskellStructB <- peekByteOff s 4
      return HaskellStruct{haskellStructA, haskellStructB}

  poke s HaskellStruct{haskellStructA, haskellStructB} = do
      pokeByteOff s 0 haskellStructA
      pokeByteOff s 4 haskellStructB

exampleContext :: C.Context
exampleContext = mempty
  { C.ctxTypesTable = Map.fromList
    [ ( C.TypeName "ExampleStruct"  , [t| HaskellStruct |] )
    , ( C.TypeName "FunPtr_Void_Int", [t| FunPtr_Void_Int |] )
    ]
  }
