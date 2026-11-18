-- | Errors of the @TranslateTypes@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.Error (
    TranslateTypesError (..)
  , FunPositionError (..)
  , NoFFIType (..)
  , FunPosition (..)
  , FunArgPosition (..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
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

-- | A bug if any position has an unexpected type, a warning otherwise
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

-- | Position in a function signature
data FunPosition =
    FunPositionArg FunArgPosition
  | FunPositionRes
  deriving stock (Show, Eq)

instance PrettyForTrace FunPosition where
  prettyForTrace = \case
      FunPositionArg arg -> PP.hsep $
          ["argument", PP.show arg.index]
       ++ [PP.parens (prettyForTrace name) | Just name <- [arg.name]]
      FunPositionRes -> "result"

data FunArgPosition = FunArgPosition {
      -- | Starting at 1
      index :: Int
    , name  :: Maybe C.ScopedName
    }
  deriving stock (Show, Eq)
