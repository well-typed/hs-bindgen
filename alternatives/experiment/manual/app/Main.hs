{-# LANGUAGE CPP #-}

module Main (main) where

import Foreign

import HsBindgenCExample

main :: IO ()
main = do
    cHelloWorld
    cShowInt 1234
    alloca $ \ptr -> do
      poke ptr $ HaskellStruct 1234 5678
      cShowStruct ptr
    cCallFunPtr addrOf_cShowInt
    funPtr <- cReturnFunPtr
    callFunPtr_Void_Int funPtr 12345678

#ifdef INCLUDE_INVALID
    invalid_cHelloWorld_extraParam 0
    alloca $ \ptr ->
      invalid_cShowInt_wrongParam ptr
#endif
