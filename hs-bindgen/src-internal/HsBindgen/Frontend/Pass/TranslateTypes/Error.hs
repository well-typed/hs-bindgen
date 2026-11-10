-- | Errors of the @TranslateTypes@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.Error (
    TranslateTypesError (..)
  , NoFFIType (..)
  , UnexpectedType (..)
  , UnexpectedTypeKind (..)
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
    -- | Function arguments or result of a type that C or an earlier pass rules
    -- out
  | TranslateTypesUnexpectedType (NonEmpty UnexpectedType)
  deriving stock (Show, Eq)

-- | Combine the errors of the arguments and the result of a function
--
-- An unexpected type outranks a missing FFI type: it is a bug, and the
-- function fails either way.
instance Semigroup TranslateTypesError where
  x <> y = case (x, y) of
      (TranslateTypesNoFFIType xs, TranslateTypesNoFFIType ys) ->
        TranslateTypesNoFFIType (xs <> ys)
      (TranslateTypesUnexpectedType xs, TranslateTypesUnexpectedType ys) ->
        TranslateTypesUnexpectedType (xs <> ys)
      (TranslateTypesUnexpectedType{}, TranslateTypesNoFFIType{}) -> x
      (TranslateTypesNoFFIType{}, TranslateTypesUnexpectedType{}) -> y

instance PrettyForTrace TranslateTypesError where
  prettyForTrace = \case
      TranslateTypesNoFFIType xs ->
        PP.hang "Cannot pass by value, no FFI type:" 2 $
          PP.vcat $ map prettyForTrace (toList xs)
      TranslateTypesUnexpectedType xs ->
        PP.hang "Unexpected type in function signature:" 2 $
          PP.vcat $ map prettyForTrace (toList xs)

instance IsTrace Level TranslateTypesError where
  getDefaultLogLevel = \case
    TranslateTypesNoFFIType{}      -> Warning
    TranslateTypesUnexpectedType{} -> Bug
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

data UnexpectedType = UnexpectedType {
      position :: FunPosition
    , kind     :: UnexpectedTypeKind
      -- | Rendered in C syntax
    , cType    :: Text
    }
  deriving stock (Show, Eq)

instance PrettyForTrace UnexpectedType where
  prettyForTrace x = PP.hsep [
        prettyForTrace x.position
      , "of C type"
      , PP.singleQuotes (PP.text x.cType)
      , "is"
      , prettyForTrace x.kind
      ]

-- | The canonical type of an 'UnexpectedType'
--
-- C rules out both as result types; @AdjustTypes@ adjusts both to pointers as
-- argument types.
data UnexpectedTypeKind =
    UnexpectedArray
  | UnexpectedFunction
  deriving stock (Show, Eq)

instance PrettyForTrace UnexpectedTypeKind where
  prettyForTrace = \case
      UnexpectedArray    -> "an array"
      UnexpectedFunction -> "a function"

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
