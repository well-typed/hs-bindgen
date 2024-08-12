{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Handwritten bindings, using inline-c.
module HsBindgenCExample where

-- base
import Foreign
import Foreign.C.Types

-- inline-c
import qualified Language.C.Inline as C

-- try-inline-c
import Types

--------------------------------------------------------------------------------

C.context (C.baseCtx <> C.funCtx <> exampleContext)
C.include "hs-bindgen-c-example.h"

{-------------------------------------------------------------------------------
  Valid declarations
-------------------------------------------------------------------------------}

cHelloWorld :: IO ()
cHelloWorld = [C.exp| void { hs_bindgen_c_example_helloworld(); } |]

cShowInt :: CInt -> IO ()
cShowInt i =
  [C.exp| void { hs_bindgen_c_example_showInt($(int i)); } |]

cShowStruct :: Ptr HaskellStruct -> IO ()
cShowStruct str =
  [C.exp| void { hs_bindgen_c_example_showStruct($(ExampleStruct* str)); }|]

cCallFunPtr :: FunPtr_Void_Int -> IO ()
cCallFunPtr ptr =
  [C.exp| void { hs_bindgen_c_example_callFunPtr($(FunPtr_Void_Int ptr)); } |]

addrOf_cShowInt :: IO FunPtr_Void_Int
addrOf_cShowInt =
  [C.exp| FunPtr_Void_Int { &hs_bindgen_c_example_showInt } |]

cReturnFunPtr :: IO FunPtr_Void_Int
cReturnFunPtr =
  [C.exp| FunPtr_Void_Int { hs_bindgen_c_example_returnFunPtr() } |]

callFunPtr_Void_Int :: FunPtr_Void_Int -> CInt -> IO ()
callFunPtr_Void_Int ptr i =
  [C.exp| void { $(FunPtr_Void_Int ptr)($(int i)) } |]

wrapFunPtr_Void_Int :: (CInt -> IO ()) -> IO FunPtr_Void_Int
wrapFunPtr_Void_Int f =
  [C.exp| void (*)(int) { $fun:(void (*f)(int)) } |]

{-------------------------------------------------------------------------------
  Invalid declarations
-------------------------------------------------------------------------------}

#ifdef INCLUDE_INVALID

invalid_cShowInt_wrongParam :: Ptr CInt -> IO ()
invalid_cShowInt_wrongParam i
  = [C.exp| void { hs_bindgen_c_example_showInt($(int i)) } |]

#endif
