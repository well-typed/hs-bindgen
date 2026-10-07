module HsBindgen.Config.MangleCandidate.ReservedNames (
    -- $ReservedNames
    ReservedNames(..)
  , reservedNamesIn
  , allReservedNames
  , haskellKeywords
  , ghcExtensionKeywords
  , ghcNonReservedKeywords
  , hsBindgenReservedNames
  , sanityReservedNames
  ) where

import Data.Set qualified as Set
import GHC.Generics (Generically (..))
import Optics.Core (Lens')

import HsBindgen.Backend.Global (preludeNames)
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Reserved Names
-------------------------------------------------------------------------------}

{- $ReservedNames

This module defines various sets of reserved names that are used by the default
name manglers.

Users who create their own name manglers must consider the names that may be
created.  For example, the default name manglers prefix types with @C@, so name
@CInt@ is reserved to avoid confusion if C code defines an @Int@ type, while
name @Int@ does not need to be reserved because the default name manglers will
never create that name.  These sets are exported for convenience, but it is the
responsibility of users who create their own name manglers to reserve names to
work with the implementation of their name manglers.

-}

-- | Reserved names, per namespace
data ReservedNames = ReservedNames {
      typeConstr :: Set Text
    , constr     :: Set Text
    , var        :: Set Text
    }
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically ReservedNames

inNamespace :: Hs.Namespace -> Lens' ReservedNames (Set Text)
inNamespace = \case
    Hs.NsTypeConstr -> #typeConstr
    Hs.NsConstr     -> #constr
    Hs.NsVar        -> #var

reservedNamesIn :: Hs.Namespace -> ReservedNames -> Set Text
reservedNamesIn ns = view (inNamespace ns)

reserve :: Hs.Namespace -> [Text] -> ReservedNames
reserve ns names = mempty & inNamespace ns .~ Set.fromList names

allReservedNames :: ReservedNames
allReservedNames = mconcat [
      reserve Hs.NsVar haskellKeywords
    , reserve Hs.NsVar ghcExtensionKeywords
    , hsBindgenReservedNames
    , reserve Hs.NsTypeConstr sanityReservedNames
    , reserve Hs.NsConstr     sanityReservedNames
    ]

-- | Haskell keywords
--
-- * [Source](https://gitlab.haskell.org/ghc/ghc/-/blob/7d42b2df006c50aecfeea6f6a53b9b198f5764bf/compiler/GHC/Parser/Lexer.x#L781-805)
haskellKeywords :: [Text]
haskellKeywords =
    [ "as"
    , "case"
    , "class"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "else"
    , "hiding"
    , "foreign"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "module"
    , "newtype"
    , "of"
    , "qualified"
    , "then"
    , "type"
    , "where"
    ]

-- | GHC extension keywords
--
-- * [Source](https://gitlab.haskell.org/ghc/ghc/-/blob/7d42b2df006c50aecfeea6f6a53b9b198f5764bf/compiler/GHC/Parser/Lexer.x#L807-829)
-- * [Arrow notation](https://gitlab.haskell.org/ghc/ghc/-/blob/7d42b2df006c50aecfeea6f6a53b9b198f5764bf/compiler/GHC/Parser/Lexer.x#L964-966)
-- * [cases](https://gitlab.haskell.org/ghc/ghc/-/blob/7d42b2df006c50aecfeea6f6a53b9b198f5764bf/compiler/GHC/Parser/Lexer.x#L871)
-- * [role](https://gitlab.haskell.org/ghc/ghc/-/issues/18941)
--
-- Some keywords are context specific and are valid Haskell identifiers netvertheless.
-- We list them but have commented out.
ghcExtensionKeywords :: [Text]
ghcExtensionKeywords = [
      "by"
    , "forall"
    , "mdo"
    , "pattern"
    , "proc"
    , "rec"
    , "static"
    , "using"
    ]

-- | Keywords that /can/ be used as identifiers
--
-- By default the name mangler leaves these alone, because their usage is in
-- principle unproblematic. However, you may wish to add these to the list of
-- reserved names to avoid confusion.
ghcNonReservedKeywords :: [Text]
ghcNonReservedKeywords = [
      "anyclass"
    , "capi"
    , "cases"
    , "ccall"
    , "dynamic"
    , "export"
    , "family"
    , "group"
    , "interruptible"
    , "javascript"
    , "label"
    , "prim"
    , "role"
    , "safe"
    , "stdcall"
    , "stock"
    , "unsafe"
    , "via"
    ]

-- | Names that @hs-bindgen@ uses unqualified, each in its namespace
--
-- These are the names generated modules import from the "Prelude"; see
-- 'preludeNames'. Operators such as @~@ are included, though they are not
-- valid C identifiers.
hsBindgenReservedNames :: ReservedNames
hsBindgenReservedNames =
    foldMap (\name -> reserve name.ns [name.text]) preludeNames

-- | Names of types and their constructors that are reserved because using them
-- could cause confusion
--
-- * "Foreign.C.Types"
sanityReservedNames :: [Text]
sanityReservedNames =
    [ "CBool"
    , "CChar"
    , "CClock"
    , "CDouble"
    , "CFile"
    , "CFloat"
    , "CFpos"
    , "CInt"
    , "CIntMax"
    , "CIntPtr"
    , "CJmpBuf"
    , "CLLong"
    , "CLong"
    , "CPtrdiff"
    , "CSChar"
    , "CSUSeconds"
    , "CShort"
    , "CSigAtomic"
    , "CSize"
    , "CTime"
    , "CUChar"
    , "CUInt"
    , "CUIntMax"
    , "CUIntPtr"
    , "CULLong"
    , "CULong"
    , "CUSeconds"
    , "CUShort"
    , "CWchar"
    ]
