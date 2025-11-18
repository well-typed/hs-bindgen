{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Main executable for the hs-bindgen manual.

This module orchestrates all the examples from the manual, which are organized
into separate modules corresponding to the manual's structure:

== Type System
* 'Manual.Types.Structs' - Examples for struct bindings
* 'Manual.Types.Enums' - Examples for enum bindings
* 'Manual.Types.Typedefs' - Examples for typedef bindings
* 'Manual.Types.Unions' - Examples for union bindings
* 'Manual.Types.Arrays' - Examples for array bindings
* 'Manual.Types.Complex' - Examples for complex number types

== Functions
* 'Manual.Functions.FirstOrder' - Examples for functions and function pointers
* 'Manual.Functions.HigherOrder' - Examples for callbacks (passing Haskell functions to C)

== Other
* 'Manual.Macros' - Examples for macro bindings
* 'Manual.GeneratedNames' - Examples for name generation (awkward names)
* 'Manual.BindingSpecifications' - Examples for external binding specifications
* 'Manual.Globals' - Examples for global variables and constants
* 'Manual.ZeroCopy' - Examples for zero-copy bindings
-}
module Manual (main) where

import System.IO.Unsafe

import Example.Unsafe
import Example

import qualified Manual.BindingSpecifications
import qualified Manual.Functions.FirstOrder
import qualified Manual.Functions.HigherOrder
import qualified Manual.GeneratedNames
import qualified Manual.Globals
import qualified Manual.Macros
import qualified Manual.Types.Arrays
import qualified Manual.Types.Complex
import qualified Manual.Types.Enums
import qualified Manual.Types.Structs
import qualified Manual.Types.Typedefs
import qualified Manual.Types.Unions
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
    Manual.Types.Structs.examples
    Manual.Types.Enums.examples triple
    Manual.Types.Typedefs.examples triple
    Manual.Macros.examples
    Manual.Types.Unions.examples
    Manual.GeneratedNames.examples
    Manual.BindingSpecifications.examples
    Manual.Globals.examples
    Manual.Types.Arrays.examples
    Manual.Functions.FirstOrder.examples
    Manual.Types.Complex.examples
    Manual.Functions.HigherOrder.examples
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
