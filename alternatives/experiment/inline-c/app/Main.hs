{-# LANGUAGE CPP #-}

module Main (main) where

-- base
import Foreign
import System.IO

-- try-inline-c
import Types
import HsBindgenCExample

--------------------------------------------------------------------------------

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
    addr <- addrOf_cShowInt
    cCallFunPtr addr

    -- C function returning a pointer to a function, executing it in Haskell
    funPtrFromC <- cReturnFunPtr
    callFunPtr_Void_Int funPtrFromC 12345678

    -- C function taking a pointer to a function, passing a Haskell function
    funPtrFromHaskell <- wrapFunPtr_Void_Int $ \x ->
      putStrLn $ "fromHaskell: " ++ show x
    cCallFunPtr funPtrFromHaskell

#ifdef INCLUDE_INVALID
    alloca $ \ptr ->
      invalid_cShowInt_wrongParam ptr
#endif
