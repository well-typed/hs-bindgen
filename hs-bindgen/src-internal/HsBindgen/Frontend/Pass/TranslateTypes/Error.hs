-- | Errors of the @TranslateTypes@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.Error (
    TranslateTypesError (..)
  , FunPositionError (..)
  , NoFFIType (..)
  , positionErrorsLevel
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.Pass.TranslateTypes.Position
import HsBindgen.Imports
import HsBindgen.Util.Tracer

-- | Why we cannot generate a binding for a function
newtype TranslateTypesError =
    -- | The arguments and the result we cannot pass, in signature order
    TranslateTypesFunctionError (NonEmpty FunPositionError)
  deriving stock (Show, Eq)

instance PrettyForTrace TranslateTypesError where
  prettyForTrace (TranslateTypesFunctionError xs) =
      PP.hang "Cannot translate function signature:" 2 $
        PP.vcat $ map prettyForTrace (toList xs)

instance IsTrace Level TranslateTypesError where
  getDefaultLogLevel (TranslateTypesFunctionError xs) = positionErrorsLevel xs
  getSource  = const HsBindgen
  getTraceId = const "translate-types"

positionErrorsLevel :: NonEmpty FunPositionError -> Level
positionErrorsLevel xs = maximum (fmap logLevel xs)
  where
    logLevel :: FunPositionError -> Level
    logLevel = \case
      FunPositionNoFFIType{} -> Warning

-- | Why we cannot pass a function argument or result
data FunPositionError =
    -- | Passed directly without an FFI type
    FunPositionNoFFIType NoFFIType
  deriving stock (Show, Eq)

instance PrettyForTrace FunPositionError where
  prettyForTrace = \case
      FunPositionNoFFIType      x -> prettyForTrace x

data NoFFIType = NoFFIType {
      position :: FunPosition
      -- | Rendered in C syntax
    , cType    :: Text
    }
  deriving stock (Show, Eq)

instance PrettyForTrace NoFFIType where
  prettyForTrace x = PP.hsep [
        prettyForTrace x.position
      , "of C type"
      , PP.singleQuotes (PP.text x.cType)
      , "has no FFI type"
      ]

