-- | A representative sample of wrappers for the rest of libclang's
-- @Clang.LowLevel.FFI@ (beyond the AST-walk slice in @ClangFFI.Wrappers@), covering
-- the purely lifted functions: those whose every argument and result is a handle,
-- an enum, a scalar, a @ConstPtr@, or a lifted @Ptr@ out-parameter.
--
-- The lifted surface is large but uniform: most of diagnostics, target info, the
-- @clang_is*@ predicates, tokens, and the evaluation functions lift with a bare
-- @toHighLevel auto@ or one closer. Rather than list them all, this keeps one
-- wrapper per distinct shape. The by-value (@R@ \/ @W@) functions are omitted here,
-- but the combinators do reach them through 'bracketUnlifted' \/ 'outputUnlifted';
-- see the cursor wrappers in @ClangFFI.Wrappers@.
module ClangFFI.Lifted (
    -- * Bare @auto@
    numDiagnostics
  , diagnosticSeverity
  , getFile
    -- * @output@ with a borrowed-buffer copy
  , getFileContents
    -- * @auto@ input, @Bool@ closer
  , isDeclaration
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.C.Types (CSize)

import Clang.Enum.Simple (SimpleEnum)
import Clang.Internal.ConstPtr (ConstPtr (..))
import Clang.LowLevel.Core.Enums (CXCursorKind, CXDiagnosticSeverity)
import Clang.LowLevel.Core.Instances ()
import Clang.LowLevel.Core.Pointers (CXDiagnostic, CXFile, CXTranslationUnit)
import Clang.LowLevel.FFI

import HsBindgen.Runtime.HighLevel (input, output, resultPure, toHighLevel)
import HsBindgen.Runtime.HighLevel.Defaults (auto, defaultOut)
import HsBindgen.Runtime.HighLevel.Marshaller (scalar)

-- | @clang_getNumDiagnostics@. Bare @auto@: a handle in, a scalar out
-- (@CUInt -> Word@).
numDiagnostics :: CXTranslationUnit -> IO Word
numDiagnostics = toHighLevel auto nowrapper_getNumDiagnostics

-- | @clang_getDiagnosticSeverity@. @auto@ with a @SimpleEnum@ result.
diagnosticSeverity :: CXDiagnostic -> IO (SimpleEnum CXDiagnosticSeverity)
diagnosticSeverity = toHighLevel auto nowrapper_getDiagnosticSeverity

-- | @clang_getFile@. @auto@ end to end, including 'withCStringIn' for the @String@
-- path.
getFile :: CXTranslationUnit -> String -> IO CXFile
getFile = toHighLevel auto nowrapper_getFile

-- | @clang_getFileContents@. The size comes back through a lifted @Ptr CSize@
-- out-parameter ('output') and the contents pointer is the (borrowed) result; the
-- combinators thread them independently, so the copy that joins them into a
-- 'ByteString' is at the call site (done immediately, so the borrow is valid).
getFileContents :: CXTranslationUnit -> CXFile -> IO ByteString
getFileContents tu file = do
    (n, p) <-
      toHighLevel
        (input (scalar id) $ input (scalar id) $ output defaultOut $ resultPure id)
        nowrapper_getFileContents
        tu
        file
    BS.packCStringLen (unConstPtr p, fromIntegral (n :: CSize))

-- | @clang_isDeclaration@. @auto@ fills the input; 'resultPure' turns the @CUInt@
-- into a 'Bool' (there is no @DefaultRes CUInt Bool@). The other eight @clang_is*@
-- predicates are identical.
isDeclaration :: SimpleEnum CXCursorKind -> IO Bool
isDeclaration = toHighLevel (auto $ resultPure (/= 0)) nowrapper_isDeclaration
