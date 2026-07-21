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
    -- * @output@ joined with the return value in the closer
  , getFileContents
    -- * An explicit closer, for a conversion no default makes
  , isDeclaration
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.C.Types (CChar, CSize)

import Clang.Enum.Simple (SimpleEnum)
import Clang.Internal.ConstPtr (ConstPtr (..))
import Clang.LowLevel.Core.Enums (CXCursorKind, CXDiagnosticSeverity)
import Clang.LowLevel.Core.Instances ()
import Clang.LowLevel.Core.Pointers (CXDiagnostic, CXFile, CXTranslationUnit)
import Clang.LowLevel.FFI

import HsBindgen.HighLevel (output, resultIO, toHighLevel)
import HsBindgen.HighLevel.Auto (auto, autoInputs, autoWith)
import HsBindgen.HighLevel.Defaults (defaultOut)

-- | @clang_getNumDiagnostics@. Bare @auto@: a handle in, a scalar out
-- (@CUInt -> Word@).
numDiagnostics :: CXTranslationUnit -> IO Word
numDiagnostics = toHighLevel nowrapper_getNumDiagnostics auto

-- | @clang_getDiagnosticSeverity@. @auto@ with a @SimpleEnum@ result.
diagnosticSeverity :: CXDiagnostic -> IO (SimpleEnum CXDiagnosticSeverity)
diagnosticSeverity = toHighLevel nowrapper_getDiagnosticSeverity auto

-- | @clang_getFile@. @auto@ end to end, including 'withCStringIn' for the @String@
-- path.
getFile :: CXTranslationUnit -> String -> IO CXFile
getFile = toHighLevel nowrapper_getFile auto

-- | @clang_getFileContents@. libclang returns a pointer it still owns, valid only
-- until the translation unit is disposed, so the bytes are copied out before this
-- returns rather than handed over.
getFileContents :: CXTranslationUnit -> CXFile -> IO ByteString
getFileContents = toHighLevel nowrapper_getFileContents
                $ autoInputs        -- CXTranslationUnit, CXFile
                $ output defaultOut -- size_t *size (out)
                $ resultIO packContents
  where
    packContents :: CSize -> ConstPtr CChar -> IO ByteString
    packContents n p = BS.packCStringLen (unConstPtr p, fromIntegral n)

-- | @clang_isDeclaration@. libclang returns @unsigned@ for a predicate, a C
-- convention no default converts, so the @\/= 0@ is spelled out.
--
-- The other eight @clang_is*@ predicates are identical.
isDeclaration :: SimpleEnum CXCursorKind -> IO Bool
isDeclaration = toHighLevel nowrapper_isDeclaration (autoWith (/= 0))
