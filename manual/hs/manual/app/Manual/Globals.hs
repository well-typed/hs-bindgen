{-# LANGUAGE OverloadedRecordDot #-}

module Manual.Globals (examples) where

import Foreign as F

import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import Globals qualified
import Globals.Global qualified as Globals
import Manual.Tools

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Global variables"
    config <- peek Globals.globalConfig
    print config
    poke Globals.globalConfig $
      config{Globals.globalConfig_numThreads = 3}
    config' <- peek Globals.globalConfig
    print config'

    subsection "Non-extern globals"
    print =<< peek Globals.nonExternGlobalInt

    subsection "Constants"
    print Globals.globalConstant
    print Globals.anotherGlobalConstant

    subsubsection "Constant examples"
    print Globals.constArray1
    print =<< IA.peekArray 5 (PtrConst.unsafeToPtr Globals.constArray2)
    print Globals.constTuple
    print =<< F.peek Globals.nonConstTuple
    print =<< F.peek Globals.int
    print Globals.constInt
    print =<< F.peek Globals.ptrToInt
    print =<< F.peek Globals.ptrToConstInt
    print Globals.constPtrToInt
    print Globals.constPtrToConstInt
