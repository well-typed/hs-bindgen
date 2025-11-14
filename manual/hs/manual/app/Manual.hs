{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Main executable for the hs-bindgen manual.

This module orchestrates all the examples from the manual, which are organized
into separate modules corresponding to the manual's structure:

* 'Manual.Structs' - Examples for struct bindings
* 'Manual.Enums' - Examples for enum bindings
* 'Manual.Typedefs' - Examples for typedef bindings
* 'Manual.Macros' - Examples for macro bindings
* 'Manual.Unions' - Examples for union bindings
* 'Manual.GeneratedNames' - Examples for name generation (awkward names)
* 'Manual.BindingSpecifications' - Examples for external binding specifications
* 'Manual.Globals' - Examples for global variables and constants
* 'Manual.Arrays' - Examples for array bindings
* 'Manual.Functions' - Examples for functions and function pointers
* 'Manual.Complex' - Examples for complex number types
* 'Manual.Callbacks' - Examples for callbacks (passing Haskell functions to C)
* 'Manual.ZeroCopy' - Examples for zero-copy bindings
-}
module Manual (main) where

import System.IO.Unsafe

import Example.Unsafe
import Example

import qualified Manual.Arrays
import qualified Manual.BindingSpecifications
import qualified Manual.Callbacks
import qualified Manual.Complex
import qualified Manual.Enums
import qualified Manual.Functions
import qualified Manual.GeneratedNames
import qualified Manual.Globals
import qualified Manual.Macros
import qualified Manual.Structs
import qualified Manual.Typedefs
import qualified Manual.Unions
import qualified Manual.ZeroCopy

import Foreign

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    -- Create a sample triple that's used by multiple examples
    let triple = mkTriple 1 2 3

    -- Run all manual examples in the order they appear in the manual
    Manual.Structs.examples
    Manual.Enums.examples triple
    Manual.Typedefs.examples triple
    Manual.Macros.examples
    Manual.Unions.examples
    Manual.GeneratedNames.examples
    Manual.BindingSpecifications.examples
    Manual.Globals.examples
    Manual.Arrays.examples
    Manual.Functions.examples
    Manual.Complex.examples
    Manual.Callbacks.examples
    Manual.ZeroCopy.examples

{-------------------------------------------------------------------------------
  Helper - shared across modules
-------------------------------------------------------------------------------}

-- | Create a sample triple for use in examples
mkTriple :: Int -> Int -> Int -> Triple
mkTriple a b c = unsafePerformIO $
    alloca $ \ptr -> do
      mk_triple (fromIntegral a) (fromIntegral b) (fromIntegral c) ptr
      peek ptr
