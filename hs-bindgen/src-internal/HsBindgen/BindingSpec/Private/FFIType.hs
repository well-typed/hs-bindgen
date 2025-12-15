-- | FFI types
--
-- Intended for qualified import.
--
-- > import HsBindgen.BindingSpec.Private.FFIType qualified as FFIType
--
module HsBindgen.BindingSpec.Private.FFIType (
    FFIType (..)
    -- * Re-exports
  , BasicForeignType (..)
  , BuiltinForeignType (..)
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text qualified as Text
import GHC.Generics
import Text.Read (readMaybe)

import HsBindgen.Runtime.BaseForeignType

data FFIType =
    Function
  | Array
  | Data
  | Basic BasicForeignType
  | Builtin BuiltinForeignType
  deriving stock (Show, Eq, Ord, Generic, Read)

instance Aeson.FromJSON FFIType where
  parseJSON = Aeson.withText "FFIType" $ \t ->
    let s = Text.unpack t
    in  case readMaybe s of
          Just bft -> return bft
          Nothing   -> Aeson.parseFail $ "unknown FFI type: " ++ s

instance Aeson.ToJSON FFIType where
  toJSON = Aeson.String . Text.pack . show
