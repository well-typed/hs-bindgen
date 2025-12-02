{-# LANGUAGE OverloadedRecordDot #-}

module Manual.Globals (examples) where

import Foreign as F

import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.ConstPtr

import Manual.Tools

import Globals qualified
import Globals.Global qualified as Globals

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Global variables"
    config <- peek Globals.globalConfig_ptr
    print config
    poke Globals.globalConfig_ptr $
      config{Globals.globalConfig_numThreads = 3}
    config' <- peek Globals.globalConfig_ptr
    print config'

    subsection "Non-extern globals"
    print =<< peek Globals.nonExternGlobalInt_ptr

    subsection "Constants"
    print Globals.globalConstant
    print Globals.anotherGlobalConstant

    subsubsection "Constant examples"
    print Globals.constArray1
    print =<< IA.peekArray 5 Globals.constArray2_ptr.unConstPtr
    print Globals.constTuple
    print =<< F.peek Globals.nonConstTuple_ptr
    print =<< F.peek Globals.int_ptr
    print Globals.constInt
    print =<< F.peek Globals.ptrToInt_ptr
    print =<< F.peek Globals.ptrToConstInt_ptr
    print Globals.constPtrToInt
    print Globals.constPtrToConstInt
