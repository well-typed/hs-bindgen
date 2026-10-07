{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module HsBindgen.Backend.HsModule.Names (
    -- * Name type
    NameType(..)

    -- * Resolved name
  , ResolvedName(..)
  , resolveGlobal
  ) where

import Data.Char qualified as Char
import Language.Haskell.TH.Syntax qualified as TH

import HsBindgen.Backend.Global
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

-- | A base name attached with information about the 'NameType', the Haskell
--   import statement, and the namespace.
data ResolvedName = ResolvedName {
      string   :: String
    , typ      :: NameType
    , hsImport :: Hs.Import
    , ns       :: Hs.Namespace
    }
  deriving (Eq, Ord, Show)

resolveGlobal :: Global c -> ResolvedName
resolveGlobal g = ResolvedName{
      string   = baseName
    , typ      = nameType baseName
    , hsImport = g.imprt
    , ns       = globalCatNamespace g.cat
    }
  where
    baseName :: String
    baseName = TH.nameBase g.name
