{-# LANGUAGE CPP #-}

module Main (main) where

import Foreign

import HsBindgenCExample
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    -- Simplest example: call C function with no arguments, no result
    cHelloWorld

    -- C function with a single argument
    cShowInt 1234

    -- C function taking a pointer to a struct as argument
    alloca $ \ptr -> do
      poke ptr $ HaskellStruct 1234 5678
      cShowStruct ptr

    -- C function taking a pointer to a function, passing another C function
    cCallFunPtr addrOf_cShowInt

    -- C function returning a pointer to a function, executing it in Haskell
    funPtrFromC <- cReturnFunPtr
    callFunPtr_Void_Int funPtrFromC 12345678

    -- C function taking a pointer to a function, passing a Haskell function
    funPtrFromHaskell <- wrapFunPtr_Void_Int $ \x ->
      putStrLn $ "fromHaskell: " ++ show x
    cCallFunPtr funPtrFromHaskell

#ifdef INCLUDE_INVALID
    invalid_cHelloWorld_extraParam 0
    alloca $ \ptr ->
      invalid_cShowInt_wrongParam ptr
#endif
