-- | Errors of the @TranslateTypes@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.Error (
    TranslateTypesError (..)
  , FunPositionError (..)
  , NoFFIType (..)
  , UnexpectedType (..)
  , UnexpectedTypeKind (..)
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
  getDefaultLogLevel (TranslateTypesFunctionError xs)
    | any isUnexpected xs = Bug
    | otherwise           = Warning
    where
      isUnexpected :: FunPositionError -> Bool
      isUnexpected = \case
        FunPositionNoFFIType{}      -> False
        FunPositionUnexpectedType{} -> True
  getSource  = const HsBindgen
  getTraceId = const "translate-types"

-- | Why we cannot pass a function argument or result
data FunPositionError =
    -- | Passed directly without an FFI type
    FunPositionNoFFIType NoFFIType
    -- | Of a type that C or an earlier pass rules out
  | FunPositionUnexpectedType UnexpectedType
  deriving stock (Show, Eq)

instance PrettyForTrace FunPositionError where
  prettyForTrace = \case
      FunPositionNoFFIType      x -> prettyForTrace x
      FunPositionUnexpectedType x -> prettyForTrace x

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
      , "is unexpectedly"
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
