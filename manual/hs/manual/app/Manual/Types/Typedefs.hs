module Manual.Types.Typedefs (examples) where

import Foreign as F
import System.IO.Unsafe

import Manual.Tools

import Example
import Example.Unsafe

{-------------------------------------------------------------------------------
  Using typedefs
-------------------------------------------------------------------------------}

sumTriple :: Triple -> Sum
sumTriple triple = unsafePerformIO $
    with triple $ \ptr -> sum_triple ptr

averageTriple :: Triple -> Average
averageTriple triple = unsafePerformIO $
    with triple $ \ptr -> average_triple ptr

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: Triple -> IO ()
examples triple = do
    section "Typedefs"

    print (sumTriple triple)
    print (averageTriple triple)
