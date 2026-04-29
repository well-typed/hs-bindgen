-- | Partial AST
--
-- The translation from the language-c AST to our AST is a bit awkward, because
-- language-c will feed us information in a piecemeal fashion. We therefore
-- introduce this \"partial AST\" as a step in the middle.
--
-- Intended for unqualified import.
module HsBindgen.Frontend.LanguageC.PartialAST (
    PartialDecl(..)
  , PartialType(..)
  , UnknownType(
        ..
      , CChar, CSChar, CUChar
      , CShort, CSShort, CUShort
      , CInt, CSInt, CUInt
      , CLong, CSLong, CULong
      , CLLong, CSLLong, CULLong
      , CFloat, CDouble, CLDouble
      , CTypeUnknown)
  , Base(..)
  , Size(..)
  , KnownType(..)
  , CName
    -- * Starting point: no information known
  , unknownDecl
  ) where

import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data PartialDecl = PartialDecl{
      name :: Maybe CName
    , typ  :: PartialType
    }
  deriving stock (Show, Generic)

data PartialType =
    PartialUnknown UnknownType
  | PartialKnown KnownType
  deriving stock (Show)

data KnownType =
    KnownType (C.Type ReparseMacroExpansions)

    -- | Special case for top-level functions, so we can record argument names
    --
    -- It's not necessary to do this recursively: we only want argument names
    -- for top-level function declarations (not for function pointers).
  | TopLevelFun [(Maybe CName, C.Type ReparseMacroExpansions)] (C.Type ReparseMacroExpansions)
  deriving stock (Show)

-- | Name
--
-- Since @language-c@ does not really distinguish between a top-level
-- declaration or the \"declaration\" of a function argument, we use 'Text' here
-- rather than 'C.DeclName' or 'C.ScopedName'. Since we do not actually parse
-- top-level struct/enum/union declarations (i.e., deal with \"ordinary\" names
-- only anyway), this causes no trouble.
type CName = Text

{-------------------------------------------------------------------------------
  UnknownType
-------------------------------------------------------------------------------}

-- | The type itself is not yet known, but may have some qualifiers
data UnknownType = UnknownType{
      base      :: [Base]
    , sign      :: [C.PrimSign]
    , size      :: [Size]
    , isConst   :: Bool
    , isComplex :: Bool
    }
  deriving stock (Show, Generic)

data Base = Char | Int | Float | Double
  deriving stock (Show, Eq, Ord, Generic)

data Size = Short | Long
  deriving stock (Show, Eq, Ord, Generic)

{-# COMPLETE
    CChar
  , CSChar
  , CUChar
  , CShort
  , CSShort
  , CUShort
  , CInt
  , CSInt
  , CUInt
  , CLong
  , CSLong
  , CULong
  , CLLong
  , CSLLong
  , CULLong
  , CFloat
  , CDouble
  , CLDouble
  , CTypeUnknown
  #-}

pattern CChar :: UnknownType
pattern CChar <- UnknownType [Char] [] [] _ _

pattern CSChar :: UnknownType
pattern CSChar <- UnknownType [Char] [C.Signed] [] _ _

pattern CUChar :: UnknownType
pattern CUChar <- UnknownType [Char] [C.Unsigned] [] _ _

pattern CShort :: UnknownType
pattern CShort <- UnknownType (optional Int -> True) [] [Short] _ _

pattern CSShort :: UnknownType
pattern CSShort <- UnknownType (optional Int -> True) [C.Signed] [Short] _ _

pattern CUShort :: UnknownType
pattern CUShort <- UnknownType (optional Int -> True) [C.Unsigned] [Short] _ _

pattern CInt :: UnknownType
pattern CInt <- UnknownType [Int] [] [] _ _

pattern CSInt :: UnknownType
pattern CSInt <- UnknownType (optional Int -> True) [C.Signed] [] _ _

pattern CUInt :: UnknownType
pattern CUInt <- UnknownType (optional Int -> True) [C.Unsigned] [] _ _

pattern CLong :: UnknownType
pattern CLong <- UnknownType (optional Int -> True) [] [Long] _ _

pattern CSLong :: UnknownType
pattern CSLong <- UnknownType (optional Int -> True) [C.Signed] [Long] _ _

pattern CULong :: UnknownType
pattern CULong <- UnknownType (optional Int -> True) [C.Unsigned] [Long] _ _

pattern CLLong :: UnknownType
pattern CLLong <- UnknownType (optional Int -> True) [] [Long, Long] _ _

pattern CSLLong :: UnknownType
pattern CSLLong <- UnknownType (optional Int -> True) [C.Signed] [Long, Long] _ _

pattern CULLong :: UnknownType
pattern CULLong <- UnknownType (optional Int -> True) [C.Unsigned] [Long, Long] _ _

pattern CFloat :: UnknownType
pattern CFloat <- UnknownType [Float] [] [] _ _

pattern CDouble :: UnknownType
pattern CDouble <- UnknownType [Double] [] [] _ _

pattern CLDouble :: UnknownType
pattern CLDouble <- UnknownType [Double] [] [Long] _ _

pattern CTypeUnknown :: UnknownType
pattern CTypeUnknown <- UnknownType{}

optional :: Eq a => a -> [a] -> Bool
optional x = \case
    []  -> True
    [y] -> x == y
    _   -> False

{-------------------------------------------------------------------------------
  Starting point: no information known
-------------------------------------------------------------------------------}

unknownDecl :: PartialDecl
unknownDecl = PartialDecl{
      name = Nothing
    , typ  = unknownType
    }

unknownType :: PartialType
unknownType = PartialUnknown UnknownType{
      base      = []
    , sign      = []
    , size      = []
    , isConst   = False
    , isComplex = False
    }
