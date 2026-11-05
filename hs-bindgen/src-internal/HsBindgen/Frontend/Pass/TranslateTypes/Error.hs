-- | Errors of the @TranslateTypes@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.Error (
    TranslateTypesError (..)
  , NoFFIType (..)
  , FunPosition (..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Util.Tracer

-- | Why we cannot generate a binding for a declaration
data TranslateTypesError =
    -- | Function arguments or result passed by value without an FFI type
    TranslateTypesNoFFIType (NonEmpty NoFFIType)
  deriving stock (Show, Eq)

instance PrettyForTrace TranslateTypesError where
  prettyForTrace = \case
      TranslateTypesNoFFIType xs ->
        PP.hang "Cannot pass by value, no FFI type:" 2 $
          PP.vcat $ map prettyForTrace (toList xs)

instance IsTrace Level TranslateTypesError where
  getDefaultLogLevel = \case
    TranslateTypesNoFFIType{} -> Warning
  getSource  = const HsBindgen
  getTraceId = const "translate-types"

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
      ]

-- | Position in a function signature
data FunPosition =
    FunPositionArg
      Int                  -- ^ Starting at 1
      (Maybe C.ScopedName)
  | FunPositionRes
  deriving stock (Show, Eq)

instance PrettyForTrace FunPosition where
  prettyForTrace = \case
      FunPositionArg i mName -> PP.hsep $
          ["argument", PP.show i]
       ++ [PP.parens (prettyForTrace name) | Just name <- [mName]]
      FunPositionRes -> "result"
