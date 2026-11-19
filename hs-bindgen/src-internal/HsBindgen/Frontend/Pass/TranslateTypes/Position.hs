-- | Positions in function signatures and locations of function types
--
-- This module is intended to be imported unqualified.
module HsBindgen.Frontend.Pass.TranslateTypes.Position (
    -- * Positions
    FunPosition (..)
  , FunArgPosition (..)
    -- * Function type locations
  , FunTypeLocation (..)
  , UseSite (..)
  , atUseSite
  , declarationSignature
  , enterPosition
  , prettyPositionIn
  ) where

import Data.List (intersperse)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.IR.C qualified as C
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Positions
-------------------------------------------------------------------------------}

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

-- | Where a function type is in a declaration
--
-- Starts at a use-site type of the declaration, then follows the arguments and
-- results of the enclosing function types.
data FunTypeLocation = FunTypeLocation {
      site :: UseSite
      -- | The positions of the enclosing function types, outermost first
    , path :: [FunPosition]
    }
  deriving stock (Show, Eq)

-- | Places in a declaration whose type can contain a function type
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

-- | The outermost function type in the type at a use site
atUseSite :: UseSite -> FunTypeLocation
atUseSite site = FunTypeLocation{
      site = site
    , path = []
    }

-- | The signature of a function declaration
declarationSignature :: FunTypeLocation
declarationSignature = atUseSite UseSiteFunction

-- | Step into an argument or the result of the function type at a location
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

-- | Render a position together with its location
--
-- For the signature of a function declaration, the location is left out.
prettyPositionIn :: FunTypeLocation -> FunPosition -> PP.CtxDoc
prettyPositionIn loc pos
    | loc == declarationSignature = prettyForTrace pos
    | otherwise = PP.hsep [prettyForTrace pos, "of", prettyForTrace loc]
