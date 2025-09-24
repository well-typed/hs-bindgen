{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Binding specification version management
--
-- This /private/ module may only be used by "HsBindgen.BindingSpec" and
-- sub-modules.
--
-- Intended for unqualified import.
module HsBindgen.BindingSpec.Private.Version (
    -- * BindingSpecVersion
    BindingSpecVersion
  , constBindingSpecVersion
  , parseBindingSpecVersion
  , isCompatBindingSpecVersions
  ) where

import Data.Aeson.Types qualified as Aeson
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Text qualified as Text
import Language.Haskell.TH.Syntax qualified as THS
import Text.Read (readMaybe)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Errors (failCode)
import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))

{-------------------------------------------------------------------------------
  BindingSpecVersion
-------------------------------------------------------------------------------}

-- | Binding specification version
--
-- Binding specification versions are in @MAJOR.MINOR@ format where
-- @MAJOR >= 1@ and @MINOR >= 0@.
data BindingSpecVersion = UnsafeBindingSpecVersion Int Int
  deriving stock (Eq, Ord, THS.Lift)

instance Aeson.FromJSON BindingSpecVersion where
  parseJSON = Aeson.withText "BindingSpecVersion" $ \t ->
    case parseBindingSpecVersion t of
      Right v -> return v
      Left  e -> Aeson.parseFail $
        "Invalid binding specification version: " ++ e ++ ": " ++ show t

instance Aeson.ToJSON BindingSpecVersion where
  toJSON = Aeson.String . Text.pack . show

instance PrettyForTrace BindingSpecVersion where
  prettyForTrace = PP.string . show

instance Show BindingSpecVersion where
  show (UnsafeBindingSpecVersion x y) = show x ++ '.' : show y

-- | Construct a 'BindingSpecVersion'
mkBindingSpecVersion ::
     Int  -- ^ Major version (@>= 1@)
  -> Int  -- ^ Minor version (@>= 0@)
  -> Either String BindingSpecVersion
mkBindingSpecVersion major minor
    | major < 1 = Left "major version less than 1"
    | minor < 0 = Left "minor version less than 0"
    | otherwise = Right (UnsafeBindingSpecVersion major minor)

-- | Construct a 'BindingSpecVersion', validating it a compile-time
constBindingSpecVersion ::
     (MonadFail m, THS.Quote m)
  => Int  -- ^ Major version
  -> Int  -- ^ Minor version
  -> THS.Code m BindingSpecVersion
constBindingSpecVersion major minor = case mkBindingSpecVersion major minor of
    Right v -> [|| v ||]
    Left  e -> failCode $ "Invalid binding specification version: " ++ e

-- | Parse a 'BindingSpecVersion'
parseBindingSpecVersion :: Text -> Either String BindingSpecVersion
parseBindingSpecVersion t =
    case List.uncons <$> span Char.isDigit (Text.unpack t) of
      (majorS, Just ('.', minorS)) ->
        case (readMaybe majorS, readMaybe minorS) of
          (Just major, Just minor) -> mkBindingSpecVersion major minor
          _otherwise               -> Left "not in MAJOR.MINOR format"
      _otherwise -> Left "not in MAJOR.MINOR format"

-- | Check 'BindingSpecVersion' compatibility
--
-- Compatible binding specifications have the same representation.  Compatible
-- binding specification versions have the same major version.
isCompatBindingSpecVersions :: BindingSpecVersion -> BindingSpecVersion -> Bool
isCompatBindingSpecVersions
  (UnsafeBindingSpecVersion majorL _)
  (UnsafeBindingSpecVersion majorR _) = majorL == majorR
