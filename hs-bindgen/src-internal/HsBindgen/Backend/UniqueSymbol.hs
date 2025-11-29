-- | Unique symbols (for use in generated C code)
--
-- Intended for unqualified import.
module HsBindgen.Backend.UniqueSymbol (
    -- * Generating unique names
    UniqueSymbol(..)
  , unsafeUniqueHsName
  , uniqueCName
  , globallyUnique
  ) where

import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as B
import Data.Char (isLetter)
import Data.Text qualified as Text
import GHC.Unicode (isDigit)

import HsBindgen.Config.Prelims
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Generating unique names
-------------------------------------------------------------------------------}

data UniqueSymbol = UniqueSymbol{
      -- | Generated unique symbol
      unique :: String

      -- | Source of the unique symbol
      --
      -- This is intended to be included in the generated output as a comment,
      -- facilitating both reading the source code ("what is this function?") as
      -- well as debugging problems with generated unique symbols (for example
      -- if they unexpectedly change).
    , source :: String
    }
  deriving (Show, Eq, Generic)

-- | Construct Haskell name from unique symbol
--
-- Caller must ensure that namespace rules are adhered to.
unsafeUniqueHsName :: UniqueSymbol -> Hs.Name ns
unsafeUniqueHsName = Hs.Name . Text.pack . unique

uniqueCName :: UniqueSymbol -> C.Name
uniqueCName = C.Name . Text.pack . unique

-- | Globally unique symbol
--
-- The C namespace is entirely flat, so when we generate new C functions, we
-- must make sure to pick globally unique names. We do this by generating names
-- of the following shape:
--
-- > "hs_bindgen_" ++ hashOf 'UniqueId' 'BaseModuleName' 'String'
--
-- where @'String'@ is some arbitrary string, which the caller must ensure is
-- unique for a given @'UniqueId'@ and @'BaseModuleName'@.
--
-- We only use the hash in the name to avoid problems with linkers that only use
-- a fixed length prefix of names; callers should ensure that
-- @UniqueSymbol.source@ in included in a suitable comment.
globallyUnique :: UniqueId -> BaseModuleName -> String -> UniqueSymbol
globallyUnique (UniqueId uniqueId) baseModuleName str = UniqueSymbol{
      unique
    , source
    }
  where
    unique :: String
    unique = "hs_bindgen_" ++ hashString source

    source :: String
    source = concat [
          sanitizeUniqueId uniqueId
        , "_"
        , baseModuleNameToString baseModuleName
        , "_"
        , str
        ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

sanitizeUniqueId :: String -> String
sanitizeUniqueId = go
  where
    go :: String -> String
    go [] = []
    go (x:xs)
      | isLetter x          = x   : go xs
      | isDigit  x          = x   : go xs
      | x `elem` ['.', '_'] = '_' : go xs
      | otherwise           =       go xs

-- | Construct the actual hash
--
-- * We use `cryptohash-sha256` to avoid potential dynamic linker problems
--   (https://github.com/haskell-haskey/xxhash-ffi/issues/4).
-- * We use ByteString to avoid hash changes induced by a change of how Text
--   is encoded in GHC 9.2.
hashString :: String -> String
hashString = B.unpack . B.take 16 . B16.encode . hash . B.pack
