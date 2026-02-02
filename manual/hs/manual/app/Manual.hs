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

import Foreign
import System.IO.Unsafe

import Example
import Example.Unsafe
import Manual.BindingSpecifications qualified
import Manual.Functions.FirstOrder qualified
import Manual.Functions.HigherOrder qualified
import Manual.GeneratedNames qualified
import Manual.Globals qualified
import Manual.Macros qualified
import Manual.Types.Arrays qualified
import Manual.Types.Complex qualified
import Manual.Types.Enums qualified
import Manual.Types.Structs qualified
import Manual.Types.Typedefs qualified
import Manual.Types.Unions qualified
import Manual.ZeroCopy qualified

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
