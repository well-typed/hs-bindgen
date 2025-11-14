module Manual.Globals (examples) where

import Foreign as F

import HsBindgen.Runtime.IncompleteArray qualified as IA

import Manual.Tools

import Example
import Example.Global
import Example.Unsafe
import Globals qualified
import Globals.Global qualified as Globals

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: IO ()
examples = do
    section "Globals"

    subsection "Variables"
    config <- peek globalConfig_ptr
    print config
    poke globalConfig_ptr $ config{globalConfig_numThreads = 3}
    printGlobalConfig
    config' <- peek globalConfig_ptr
    print config'

    print =<< peek Globals.globalInt_ptr
    print =<< peek Globals.externGlobalInt_ptr

    subsection "Constants"
    print Globals.globalConstant
    print Globals.anotherGlobalConstant
    print Globals.staticConst
    print Globals.classless
    print Globals.constArray1
    print =<< IA.peekArray 5 Globals.constArray2_ptr
    print Globals.constTuple
    print =<< F.peek Globals.nonConstTuple_ptr
    -- TODO: the stub loses type qualifier information for everything but the
    -- outer type, so we get type warnings here. See issue #994.
    print =<< F.peek Globals.ptrToConstInt_ptr
    print Globals.constPtrToInt
    print Globals.constPtrToConstInt
