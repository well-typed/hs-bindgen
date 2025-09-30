module HsBindgen.Test.Internal (
    -- * C function prefix
    CFunPrefix
  , getCFunPrefix
    -- * Pretty printing
  , prettyHsName
  ) where

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Text qualified as T
import System.FilePath qualified as FilePath
import Text.SimplePrettyPrint

import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  C function prefix
-------------------------------------------------------------------------------}

type CFunPrefix = String

getCFunPrefix ::
     FilePath  -- ^ C test header filename
  -> CFunPrefix
getCFunPrefix = List.map aux . FilePath.dropExtension
  where
    aux :: Char -> Char
    aux c
      | Char.isAlphaNum c = Char.toLower c
      | otherwise         = '_'

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

prettyHsName :: Hs.Name ns -> CtxDoc
prettyHsName = string . T.unpack . Hs.getName
