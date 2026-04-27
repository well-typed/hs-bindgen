module HsBindgen.Test.Internal (
    -- * C function prefix
    CFunPrefix
  , getCFunPrefix
  ) where

import Data.Char qualified as Char
import Data.List qualified as List
import System.FilePath qualified as FilePath

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
