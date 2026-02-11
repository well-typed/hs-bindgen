{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsBindgen.Backend.HsModule.Names (
    -- * Name type
    NameType(..)

    -- * Resolved name
  , ResolvedName(..)
  , getResolvedName

    -- * Backend name
  , BackendName(..)
  ) where

import Data.Char qualified as Char
import Language.Haskell.TH.Syntax qualified as TH

import HsBindgen.Backend.SHs.Global
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Name type
-------------------------------------------------------------------------------}

-- | Name type
data NameType =
    -- | An identifier, e.g., @foo@
    IdentifierName
    -- | An identifier with a magic hash at the end, e.g., @foo#@
  | IdentifierMagicHashName
    -- | An operator, e.g., @(+)@
  | OperatorName
  deriving (Eq, Ord, Show)

nameType :: String -> NameType
nameType nm
  | all isIdentChar nm
  = IdentifierName
    -- nm is non-empty because of the first guard
  | all isIdentChar (init nm) && isMagicHashChar (last nm)
  = IdentifierMagicHashName
  | otherwise
  = OperatorName
  where
    isMagicHashChar :: Char -> Bool
    isMagicHashChar c = c == '#'

    isIdentChar :: Char -> Bool
    isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''

{-------------------------------------------------------------------------------
  Resolved name
-------------------------------------------------------------------------------}

-- | A base name attached with information about the 'NameType' and the Haskell
--   import statement.
data ResolvedName = ResolvedName {
      string   :: String
    , typ      :: NameType
    , hsImport :: Hs.Import
    }
  deriving (Eq, Ord, Show)

-- -- TODO D Tuples!
-- -- | Name for tuples
-- tupleResolvedName :: Bool -> Word -> ResolvedName
-- tupleResolvedName wantType i = ResolvedName tup IdentifierName Nothing
--   where
--     tup
--       | i == 0
--       = "()"
--       | i == 1
--       = if wantType
--         then "Solo"
--         else "MkSolo"
--       | otherwise
--       = "(" ++ replicate (fromIntegral (i - 1)) ',' ++ ")"

getResolvedName :: Global -> ResolvedName
getResolvedName g = ResolvedName{
      string   = baseName
    , typ      = nameType baseName
    , hsImport = getHsImport g
    }
  where
    name :: TH.Name
    name = getName g

    baseName :: String
    baseName = TH.nameBase name

{-------------------------------------------------------------------------------
  Backend name
-------------------------------------------------------------------------------}

-- | Backend name representation
data BackendName =
    -- | Local (not imported) name
    LocalBackendName NameType String
  | -- | Resolved name
    ResolvedBackendName ResolvedName
  deriving (Eq, Show)
