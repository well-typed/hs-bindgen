-- | Errors of the @TranslateTypes@ pass
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.Error (
    TranslateTypesError (..)
  , FunPositionError (..)
  , NoFFIType (..)
  , UnexpectedType (..)
  , UnexpectedTypeKind (..)
  , positionErrorsLevel
  , FunPosition (..)
  , FunArgPosition (..)
  , FunTypeLocation (..)
  , UseSite (..)
  , declarationSignature
  , enterPosition
  , prettyPositionIn
  ) where

import Data.List (intersperse)
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

instance IsTrace Level TranslateTypesError where
  getDefaultLogLevel (TranslateTypesFunctionError xs) = positionErrorsLevel xs
  getSource  = const HsBindgen
  getTraceId = const "translate-types"

-- | A bug if any position has an unexpected type, a warning otherwise
positionErrorsLevel :: NonEmpty FunPositionError -> Level
positionErrorsLevel xs
    | any isUnexpected xs = Bug
    | otherwise           = Warning
  where
    isUnexpected :: FunPositionError -> Bool
    isUnexpected = \case
      FunPositionNoFFIType{}      -> False
      FunPositionUnexpectedType{} -> True

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

{-------------------------------------------------------------------------------
  Function type locations
-------------------------------------------------------------------------------}

-- | Where a function type sits in a declaration
--
-- The function type is the outermost one in a use-site type of the
-- declaration, or nested in the positions of such a function type.
data FunTypeLocation = FunTypeLocation {
      site :: UseSite
      -- | The positions of the enclosing function types, outermost first
    , path :: [FunPosition]
    }
  deriving stock (Show, Eq)

-- | The use-site types of a declaration that can contain a function type
data UseSite =
    -- | The signature of a function declaration
    --
    -- The function type at the empty path is the signature itself.
    UseSiteFunction
    -- | The type of a regular or indirect field
  | UseSiteField C.ScopedName
    -- | The type of a typedef
  | UseSiteTypedef
    -- | The type of a global
  | UseSiteGlobal
  deriving stock (Show, Eq)

-- | The signature of a function declaration
declarationSignature :: FunTypeLocation
declarationSignature = FunTypeLocation{
      site = UseSiteFunction
    , path = []
    }

-- | The function type in a position of the function type at a location
enterPosition :: FunTypeLocation -> FunPosition -> FunTypeLocation
enterPosition loc pos = loc{path = loc.path ++ [pos]}

-- | For example, "the function type in argument 2 of the function type in
-- field cb"
instance PrettyForTrace FunTypeLocation where
  prettyForTrace loc =
      case map prettyForTrace (reverse loc.path) ++ site of
        []    -> "the function signature"
        parts -> PP.hsep $
            "the function type in"
          : intersperse "of the function type in" parts
    where
      site :: [PP.CtxDoc]
      site = case loc.site of
        UseSiteFunction   -> []
        UseSiteField name -> [PP.hsep ["field", prettyForTrace name]]
        UseSiteTypedef    -> ["the typedef"]
        UseSiteGlobal     -> ["the global"]

-- | A position of the function type at a location
--
-- A position of a function declaration renders as before, without location.
prettyPositionIn :: FunTypeLocation -> FunPosition -> PP.CtxDoc
prettyPositionIn loc pos
    | loc == declarationSignature = prettyForTrace pos
    | otherwise = PP.hsep [prettyForTrace pos, "of", prettyForTrace loc]
