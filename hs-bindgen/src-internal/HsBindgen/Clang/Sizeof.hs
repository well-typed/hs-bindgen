module HsBindgen.Clang.Sizeof (getSizeofs) where

import Control.Monad
import Data.Map.Strict qualified as Map
import Data.Text
import Text.Printf (printf)

import Clang.Args
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types (foldContinue)
import Clang.HighLevel.Types qualified as HighLevel
import Clang.LowLevel.Core

import HsBindgen.Clang
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Pass.Parse.Decl.Monad
import HsBindgen.Language.C
import HsBindgen.Util.Tracer

getSizeofs :: Tracer ClangMsg -> ClangArgs -> IO Sizeofs
getSizeofs tr args = withClang tr clangSetup $ \unit -> do
    rootCursor <- clang_getTranslationUnitCursor unit
    sizeofTuples <- varDecls rootCursor
    pure $ mkSizeofs sizeofTuples
  where
    clangInput = ClangInputMemory "sizeofs.h" virtualHeader
    clangSetup = defaultClangSetup args clangInput

varDecls :: CXCursor -> IO [(Text, NumBytes)]
varDecls rootCursor =
    HighLevel.clang_visitChildren rootCursor $ HighLevel.simpleFold $ \curr -> do
      dispatchWithArg curr $ \case
        CXCursor_VarDecl -> varDecl >=> HighLevel.foldContinueWith
        _ -> const foldContinue

varDecl :: CXCursor -> IO (Text, NumBytes)
varDecl curr = do
    name <- clang_getCursorSpelling curr
    typ <- clang_getCursorType curr
    sizeof <- parseSizeof typ
    pure (name, sizeof)

-- | Parse the size of a type as a 'NumBytes'
parseSizeof :: CXType -> IO NumBytes
parseSizeof typ = do
    sizeof <- clang_Type_getSizeOf typ
    spelling <- clang_getTypeSpelling typ
    case sizeof of
      1 -> pure One
      2 -> pure Two
      4 -> pure Four
      8 -> pure Eight
      _ ->
        -- sizes over 8 bytes are not supported, because they have no
        -- corresponding FFI type. The largest types that GHC supports are
        -- Word64/Int64, not Word128/Int128 or larger.
        panicPure $
        printf
          "parseSizeof: unexpected sizeof(%s) = %d" spelling (fromIntegral sizeof :: Int)

mkSizeofs :: [(Text, NumBytes)] -> Sizeofs
mkSizeofs xs = Sizeofs {
      -- * Character types
      schar      = m Map.! "scharVar"
    , uchar      = m Map.! "ucharVar"
    , char       = m Map.! "charVar"
        -- * Integer types
    , short      = m Map.! "shortVar"
    , ushort     = m Map.! "ushortVar"
    , int        = m Map.! "intVar"
    , uint       = m Map.! "uintVar"
    , long       = m Map.! "longVar"
    , ulong      = m Map.! "ulongVar"
    , longlong   = m Map.! "longlongVar"
    , ulonglong  = m Map.! "ulonglongVar"
      -- * Floating point types
    , float      = m Map.! "floatVar"
    , double     = m Map.! "doubleVar"
      -- * Others
    , bool       = m Map.! "boolVar"
    }
  where
    m = Map.fromList xs

-- | A virtual header with a variable for each of the arithmetic types.
virtualHeader :: String
virtualHeader =
    "signed   char scharVar;\
    \unsigned char ucharVar;\
    \         char  charVar;\
    \signed   short  shortVar;\
    \unsigned short ushortVar;\
    \signed   int  intVar;\
    \unsigned int uintVar;\
    \signed   long  longVar;\
    \unsigned long ulongVar;\
    \signed   long long  longlongVar;\
    \unsigned long long ulonglongVar;\
    \         float floatVar;\
    \         double doubleVar;\
    \         _Bool boolVar;"
