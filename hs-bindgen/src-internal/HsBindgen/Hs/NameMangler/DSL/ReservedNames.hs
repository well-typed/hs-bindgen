module HsBindgen.Hs.NameMangler.DSL.ReservedNames (
    -- $ReservedNames
    allReservedNames
  , reservedVarNames
  , reservedTypeNames
  , haskellKeywords
  , ghcExtensionKeywords
  , ghcNonReservedKeywords
  , hsBindgenReservedTypeNames
  , hsBindgenReservedVarNames
  , sanityReservedTypeNames
  , sanityReservedVarNames
  ) where

import Data.Set qualified as Set

import HsBindgen.Imports

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

allReservedNames :: Set Text
allReservedNames = Set.unions [
      reservedVarNames
    , reservedTypeNames
    ]

reservedVarNames :: Set Text
reservedVarNames = Set.fromList $
       haskellKeywords
    ++ ghcExtensionKeywords
    ++ hsBindgenReservedVarNames
    ++ sanityReservedVarNames

reservedTypeNames :: Set Text
reservedTypeNames = Set.fromList $
       hsBindgenReservedTypeNames
    ++ sanityReservedTypeNames

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

-- | Names in the type namespace that @hs-bindgen@ may use unqualified
--
-- * '~'
hsBindgenReservedTypeNames :: [Text]
hsBindgenReservedTypeNames =
    [ "~"
    ]

-- | Names in the variable namespace that @hs-bindgen@ may use unqualified
--
-- * 'pure'
-- * 'return'
hsBindgenReservedVarNames :: [Text]
hsBindgenReservedVarNames =
    [ "pure"
    , "return"
    ]

-- | Names in the type namespace that are reserved because using them could
-- cause confusion
--
-- * "Foreign.C.Types"
sanityReservedTypeNames :: [Text]
sanityReservedTypeNames =
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

-- | Names in the variable namespace that are reserved because using them could
-- cause confusion
--
-- * (None)
sanityReservedVarNames :: [Text]
sanityReservedVarNames =
    [
    ]

