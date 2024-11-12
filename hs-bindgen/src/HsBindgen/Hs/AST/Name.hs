{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Hs.AST.Name (
    -- * Definition
    Namespace(..)
  , HsName(..)
  , MkHsName(..)
    -- * Contexts
  , ModuleNameContext(..)
  , TypeClassContext(..)
  , TypeConstrContext(..)
  , TypeVarContext(..)
  , ConstrContext(..)
  , VarContext(..)
    -- * NameMangler
  , NameMangler(..)
    -- ** DSL
  , translateName
  , maintainCName
  , camelCaseCName
  , dropInvalidChar
  , escapeInvalidChar
  , isValidChar
  , joinWithConcat
  , joinWithSnakeCase
  , joinWithCamelCase
  , handleReservedNone
  , handleReservedNames
  , appendSingleQuote
  , handleModuleNameParent
    -- ** Reserved Names
    -- $ReservedNames
  , haskellKeywords
  , ghcExtensionKeywords
  , hsBindgenReservedTypeNames
  , hsBindgenReservedVarNames
  , sanityReservedTypeNames
  , sanityReservedVarNames
    -- ** Default Name Manglers
  , defaultNameMangler
  , haskellNameMangler
  ) where

import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.String
import Data.Text qualified as T
import Numeric (showHex)

import HsBindgen.Imports
import HsBindgen.C.AST (CName(..))
import HsBindgen.Util.PHOAS (ShowOpen (..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Namespace
--
-- See section 1.4, "Namespaces" of the Haskell report
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-130001.4>
data Namespace =
    NsModuleName
  | NsTypeClass
  | NsTypeConstr
  | NsTypeVar
  | NsConstr
  | NsVar

-- | Haskell name in namespace @ns@
newtype HsName (ns :: Namespace) = HsName { getHsName :: Text }
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)

instance ShowOpen (HsName ns) where
    showOpen _ = showsPrec

-- | Construct an 'HsName' in namespace @ns@
class MkHsName (ns :: Namespace) where

  -- | Construct an 'HsName' in namespace @ns@
  --
  -- Haskell identifiers must begin with an uppercase or lowercase letter,
  -- according to the type of identifier (namespace).  This function changes
  -- the case of the first letter of the passed name.
  --
  -- If the passed name starts with non-letter characters, those characters
  -- are dropped and the case of the first letter is changed.  If there are
  -- no letters, then name @x@ (or @X@) is returned as a default.
  mkHsName :: Text -> HsName ns

instance MkHsName NsModuleName where
  mkHsName = mkHsName' Char.toUpper

instance MkHsName NsTypeClass where
  mkHsName = mkHsName' Char.toUpper

instance MkHsName NsTypeConstr where
  mkHsName = mkHsName' Char.toUpper

instance MkHsName NsTypeVar where
  mkHsName = mkHsName' Char.toLower

instance MkHsName NsConstr where
  mkHsName = mkHsName' Char.toUpper

instance MkHsName NsVar where
  mkHsName = mkHsName' Char.toLower

mkHsName' :: (Char -> Char) -> Text -> HsName ns
mkHsName' f = HsName . aux
  where
    aux :: Text -> Text
    aux t = case T.uncons t of
      Just (c, t')
        | Char.isLetter c -> T.cons (f c) t'
        | otherwise       -> aux t'
      Nothing             -> T.pack [f 'x']

{-------------------------------------------------------------------------------
  Contexts
-------------------------------------------------------------------------------}

-- | Context for creating Haskell module names
data ModuleNameContext = ModuleNameContext {
      -- | Name of the parent module
      ctxModuleNameParentName :: Maybe (HsName NsModuleName)
    , -- | Module name source (perhaps from file name?)
      ctxModuleNameCName :: CName
    }
  deriving stock (Eq, Show)

-- | Context for creating Haskell type class names
newtype TypeClassContext = TypeClassContext {
      -- | Type class name source
      ctxTypeClassCName :: CName
    }
  deriving stock (Eq, Show)

-- | Context for creating Haskell type constructor names
data TypeConstrContext =
    -- | Context for general cases
    TypeConstrContext {
      -- | C name for the type
      ctxTypeConstrCName :: CName
    }
  | -- | Context for anonymous structures/unions for named fields
    AnonNamedFieldTypeConstrContext {
      -- | Closest named ancestor type context
      ctxAnonNamedFieldTypeConstrAncestorCtx :: TypeConstrContext
    , -- | C field name
      ctxAnonNamedFieldTypeConstrFieldName :: CName
    }
  deriving stock (Eq, Show)

-- | Context for creating Haskell type variable names
data TypeVarContext = TypeVarContext {
      -- | Type that the type variable is for
      ctxTypeVarTypeCtx :: TypeConstrContext
    , -- | Type variable name source
      ctxTypeVarCName :: CName
    }
  deriving stock (Eq, Show)

-- | Context for creating Haskell constructor names
newtype ConstrContext = ConstrContext {
      -- | Type that the constructor is for
      ctxConstrTypeCtx :: TypeConstrContext
    }
  deriving stock (Eq, Show)

-- | Context for creating Haskell variable names
data VarContext =
    -- | Context for general cases
    VarContext {
      -- | C variable name
      ctxVarCName :: CName
    }
  | -- | Context for enumeration fields
    EnumVarContext {
      -- | Enumeration type context
      ctxEnumVarTypeCtx :: TypeConstrContext
    }
  | -- | Context for record fields
    FieldVarContext {
      -- | Record type context
      ctxFieldVarTypeCtx :: TypeConstrContext
    , -- | Record type has a single constructor?
      ctxFieldVarSingleConstr :: Bool
    , -- | C field name
      ctxFieldVarCName :: CName
    }
  deriving stock (Eq, Show)

{-------------------------------------------------------------------------------
  NameMangler
-------------------------------------------------------------------------------}

-- | Name mangler functions
data NameMangler = NameMangler {
      -- | Create a Haskell module name
      mangleModuleName :: ModuleNameContext -> HsName NsModuleName

    , -- | Create a Haskell type class name
      mangleTypeClassName :: TypeClassContext -> HsName NsTypeClass

    , -- | Create a Haskell type constructor name
      mangleTypeConstrName :: TypeConstrContext -> HsName NsTypeConstr

    , -- | Create a Haskell type variable name
      mangleTypeVarName :: TypeVarContext -> HsName NsTypeVar

      -- | Create a Haskell constructor name
    , mangleConstrName :: ConstrContext -> HsName NsConstr

      -- | Create a Haskell variable name
    , mangleVarName :: VarContext -> HsName NsVar
    }

{-------------------------------------------------------------------------------
  Options: DSL
-------------------------------------------------------------------------------}

-- | Translate a 'CName' to an 'HsName'
--
-- Namespace @ns@ determines the case of the first letter of the name, which is
-- converted automatically.
--
-- The translation function must return a 'Text' that only contains the
-- following (valid) characters:
--
-- * Unicode letters (uppercase, lowercase, and titlecase)
-- * Unicode numbers
-- * Underscore (@_@)
-- * Single quote (@'@)
--
-- Two translation functions are provided in this module: 'maintainCName' and
-- 'camelCaseCName'.
--
-- The join function joins any prefixes, the transformed name, and any suffixes.
-- Three join functions are provided in this module: 'joinWithConcat',
-- 'joinWithSnakeCase', and 'joinWithCamelCase'.
--
-- The reserved name function may be used to change names that would cause a
-- compilation error.  Two reserved name functions are provided in this module:
-- 'handleReservedNone' and 'handleReservedNames'.
translateName :: forall ns. MkHsName ns
  => (CName -> Text)          -- ^ translate a 'CName' to a generic Haskell name
  -> ([Text] -> Text)         -- ^ join parts of a name
  -> [Text]                   -- ^ prefixes
  -> [Text]                   -- ^ suffixes
  -> (HsName ns -> HsName ns) -- ^ handle reserved names
  -> CName
  -> HsName ns
translateName transCName joinParts prefixes suffixes handleReserved cname =
    handleReserved . mkHsName @ns . joinParts $
      prefixes ++ transCName cname : suffixes

-- | Translate a C name to a Haskell name, making it as close to the C name as
-- possible
--
-- The invalid character function must return a 'String' that only contains
-- valid characters.  Two invalid character functions are provided in this
-- module: 'dropInvalidChar' and 'escapeInvalidChar'.
--
-- Note that a single quote (@'@) is not valid in C names, and it is handled
-- specially.  Any single quotes in the input are treated as invalid.
maintainCName ::
     (Char -> String) -- ^ invalid character function
  -> CName
  -> Text
maintainCName f = T.pack . aux . T.unpack . getCName
  where
    aux :: String -> String
    aux (c:cs)
      | isValidChar c = c : aux cs
      | otherwise     = f c ++ aux cs
    aux []            = ""

-- | Translate a C name to a Haskell name, converting from @snake_case@ to
-- @camelCase@
--
-- Letters after underscores are changed to uppercase.  All underscores are
-- removed, aside from a single trailing underscore if one exists.
--
-- The invalid character function must return a 'String' that only contains
-- valid characters.  Two invalid character functions are provided in this
-- module: 'dropInvalidChar' and 'escapeInvalidChar'.
--
-- Note that a single quote (@'@) is not valid in C names, and it is handled
-- specially.  Any single quotes in the input are treated as invalid.
camelCaseCName :: (Char -> String) -> CName -> Text
camelCaseCName f = T.pack . aux False . T.unpack . getCName
  where
    aux :: Bool -> String -> String
    aux isUp (c:cs)
      | c == '_'      = aux True cs
      | isValidChar c = (if isUp then Char.toUpper c else c) : aux False cs
      | otherwise     = f c ++ aux isUp cs
    aux isUp []       = if isUp then "_" else "" -- preserve trailing underscore

-- | Drop invalid characters
dropInvalidChar :: Char -> String
dropInvalidChar = const ""

-- | Escape invalid characters
--
-- An invalid character transformed to a single quote (@'@) followed by the
-- Unicode code point (four lowercase hex digits).
escapeInvalidChar :: Char -> String
escapeInvalidChar c =
    let hex = showHex (Char.ord c) ""
    in  '\'' : replicate (max 0 (4 - length hex)) '0' ++ hex

-- | Valid character predicate
--
-- Since 'escapeInvalidChar' uses the single quote (@'@) as an escape
-- character, it is treated specially.  It should never occur in a 'CName',
-- and this predicate does not consider it valid.
isValidChar :: Char -> Bool
isValidChar c = Char.isAlphaNum c || c == '_'

-- | Join parts of a name by concatenating them
joinWithConcat :: [Text] -> Text
joinWithConcat = T.concat

-- | Join parts of a name with underscores (@_@)
joinWithSnakeCase :: [Text] -> Text
joinWithSnakeCase = T.intercalate "_"

-- | Join parts of a name in @camelCase@ style
--
-- The first character of all parts but the first is changed to uppercase (if it
-- is a letter), and the results are concatenated.
--
-- Since this function may change the case of letters, it can cause name
-- collisions when different C names only differ by case of the first letter.
joinWithCamelCase :: [Text] -> Text
joinWithCamelCase = \case
    (t:ts) -> T.concat $ t : map upperFirstChar ts
    []     -> T.empty
  where
    upperFirstChar :: Text -> Text
    upperFirstChar t = case T.uncons t of
      Just (c, t') -> T.cons (Char.toUpper c) t'
      Nothing      -> t

-- | Do not handle reserved names
handleReservedNone :: HsName ns -> HsName ns
handleReservedNone = id

-- | If a name is in a set of reserved names, transform it
--
-- The transformation function must return a valid Haskell name.  One
-- transformation function is provided in this module: 'appendSingleQuote'.
handleReservedNames :: (Text -> Text) -> Set Text -> HsName ns -> HsName ns
handleReservedNames f reserved name@(HsName t)
    | t `Set.member` reserved = HsName $ f t
    | otherwise               = name

-- | Append a single quote (@'@) to a name
appendSingleQuote :: Text -> Text
appendSingleQuote = (<> "'")

-- | Prepend the parent module name, joining using a @.@, if one is provided in
-- the context
handleModuleNameParent ::
     ModuleNameContext
  -> HsName NsModuleName
  -> HsName NsModuleName
handleModuleNameParent ModuleNameContext{..} name =
    case ctxModuleNameParentName of
      Just parentName -> HsName $ getHsName parentName <> "." <> getHsName name
      Nothing         -> name

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
ghcExtensionKeywords :: [Text]
ghcExtensionKeywords =
    [ "anyclass"
    , "by"
    , "capi"
    , "cases"
    , "ccall"
    , "dynamic"
    , "export"
    , "family"
    , "forall"
    , "group"
    , "interruptible"
    , "javascript"
    , "label"
    , "mdo"
    , "pattern"
    , "prim"
    , "proc"
    , "rec"
    , "role"
    , "safe"
    , "static"
    , "stdcall"
    , "stock"
    , "unsafe"
    , "using"
    , "via"
    ]

-- | Names in the type namespace that @hs-bindgen@ may use unqualified
--
-- * (None)
hsBindgenReservedTypeNames :: [Text]
hsBindgenReservedTypeNames =
    [
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

{-------------------------------------------------------------------------------
  Default Name Manglers
-------------------------------------------------------------------------------}

-- | Default name mangler
--
-- This default attempts to provide a balance between safety and taste.
--
-- * Module names are transformed to @PascalCase@, dropping invalid characters.
-- * Type class names are transformed to @PascalCase@, escaping invalid
--   characters.
-- * Type constructors are prefixed with @C@, escaping invalid characters.
-- * Type variables have invalid characters escaped, and single quotes are
--   appended to reserved names.
-- * Constructors are prefixed with @Mk@, escaping invalid characters.
-- * Record fields are prefixed with the type name if the data type has a single
--   constructor or the constructor name otherwise, joined using an underscore,
--   escaping invalid characters.
-- * Enumeration fields are prefixed with @un@, escaping invalid characters.
-- * Other variables have invalid characters escaped, and single quotes are
--   appended to reserved names.
defaultNameMangler :: NameMangler
defaultNameMangler = NameMangler{..}
  where
    reservedTypeNames, reservedVarNames :: Set Text
    reservedTypeNames = Set.fromList $
      hsBindgenReservedTypeNames ++ sanityReservedTypeNames
    reservedVarNames = Set.fromList $
         haskellKeywords
      ++ ghcExtensionKeywords
      ++ hsBindgenReservedVarNames
      ++ sanityReservedVarNames

    mangleModuleName :: ModuleNameContext -> HsName NsModuleName
    mangleModuleName ctx@ModuleNameContext{..} = handleModuleNameParent ctx $
      translateName
        (camelCaseCName dropInvalidChar)
        joinWithConcat -- not used (no prefixes/suffixes)
        []
        []
        handleReservedNone
        ctxModuleNameCName

    mangleTypeClassName :: TypeClassContext -> HsName NsTypeClass
    mangleTypeClassName TypeClassContext{..} =
      translateName
        (camelCaseCName escapeInvalidChar)
        joinWithConcat -- not used (no prefixes/suffixes)
        []
        []
        handleReservedNone
        ctxTypeClassCName

    mangleTypeConstrName :: TypeConstrContext -> HsName NsTypeConstr
    mangleTypeConstrName = \case
      TypeConstrContext{..} ->
        translateName
          (camelCaseCName escapeInvalidChar)
          joinWithCamelCase
          ["C"]
          []
          (handleReservedNames appendSingleQuote reservedTypeNames)
          ctxTypeConstrCName
      AnonNamedFieldTypeConstrContext{..} ->
        translateName
          (camelCaseCName escapeInvalidChar)
          joinWithCamelCase
          [ getHsName $
              mangleTypeConstrName ctxAnonNamedFieldTypeConstrAncestorCtx
          ]
          []
          handleReservedNone
          ctxAnonNamedFieldTypeConstrFieldName

    mangleTypeVarName :: TypeVarContext -> HsName NsTypeVar
    mangleTypeVarName TypeVarContext{..} =
      translateName
        (maintainCName escapeInvalidChar)
        joinWithSnakeCase -- not used (no prefixes/suffixes)
        []
        []
        (handleReservedNames appendSingleQuote reservedVarNames)
        ctxTypeVarCName

    mangleConstrName :: ConstrContext -> HsName NsConstr
    mangleConstrName ConstrContext{..} =
      HsName $ "Mk" <> getHsName (mangleTypeConstrName ctxConstrTypeCtx)

    mangleVarName :: VarContext -> HsName NsVar
    mangleVarName = \case
      VarContext{..} ->
        translateName
          (maintainCName escapeInvalidChar)
          joinWithSnakeCase -- not used (no prefixes/suffixes)
          []
          []
          (handleReservedNames appendSingleQuote reservedVarNames)
          ctxVarCName
      EnumVarContext{..} ->
        HsName $ "un" <> getHsName (mangleTypeConstrName ctxEnumVarTypeCtx)
      FieldVarContext{..} ->
        translateName
          (maintainCName escapeInvalidChar)
          joinWithSnakeCase
          [ if ctxFieldVarSingleConstr
              then getHsName $ mangleTypeConstrName ctxFieldVarTypeCtx
              else
                getHsName $ mangleConstrName (ConstrContext ctxFieldVarTypeCtx)
          ]
          []
          handleReservedNone
          ctxFieldVarCName

-- | Haskell-style name mangler
--
-- This default provides Haskell-style names with a higher risk of name
-- collision.
--
-- * Module names are transformed to @PascalCase@, dropping invalid characters.
-- * Type class names are transformed to @PascalCase@, dropping invalid
--   characters.
-- * Type constructors are transformed to @PascalCase@ and prefixed with @C@,
--   escaping invalid characters.
-- * Type variables have invalid characters dropped, and single quotes are
--   appended to reserved names.
-- * Constructors are are transformed to @PascalCase@ and prefixed with @Mk@,
--   escaping invalid characters.
-- * Record fields are prefixed with the type name if the data type has a single
--   constructor or the constructor name otherwise, joined using @camelCase@,
--   dropping invalid characters.
-- * Enumeration fields are prefixed with @un@, dropping invalid characters.
-- * Other variables have invalid characters dropped, and single quotes are
--   appended to reserved names.
haskellNameMangler :: NameMangler
haskellNameMangler = NameMangler{..}
  where
    reservedTypeNames, reservedVarNames :: Set Text
    reservedTypeNames = Set.fromList $
      hsBindgenReservedTypeNames ++ sanityReservedTypeNames
    reservedVarNames = Set.fromList $
         haskellKeywords
      ++ ghcExtensionKeywords
      ++ hsBindgenReservedVarNames
      ++ sanityReservedVarNames

    mangleModuleName :: ModuleNameContext -> HsName NsModuleName
    mangleModuleName ctx@ModuleNameContext{..} = handleModuleNameParent ctx $
      translateName
        (camelCaseCName dropInvalidChar)
        joinWithCamelCase -- not used (no prefixes/suffixes)
        []
        []
        handleReservedNone
        ctxModuleNameCName

    mangleTypeClassName :: TypeClassContext -> HsName NsTypeClass
    mangleTypeClassName TypeClassContext{..} =
      translateName
        (camelCaseCName dropInvalidChar)
        joinWithCamelCase -- not used (no prefixes/suffixes)
        []
        []
        handleReservedNone
        ctxTypeClassCName

    mangleTypeConstrName :: TypeConstrContext -> HsName NsTypeConstr
    mangleTypeConstrName = \case
      TypeConstrContext{..} ->
        translateName
          (camelCaseCName dropInvalidChar)
          joinWithCamelCase
          ["C"]
          []
          (handleReservedNames appendSingleQuote reservedTypeNames)
          ctxTypeConstrCName
      AnonNamedFieldTypeConstrContext{..} ->
        translateName
          (camelCaseCName dropInvalidChar)
          joinWithCamelCase
          [ getHsName $
              mangleTypeConstrName ctxAnonNamedFieldTypeConstrAncestorCtx
          ]
          []
          handleReservedNone
          ctxAnonNamedFieldTypeConstrFieldName

    mangleTypeVarName :: TypeVarContext -> HsName NsTypeVar
    mangleTypeVarName TypeVarContext{..} =
      translateName
        (maintainCName dropInvalidChar)
        joinWithSnakeCase -- not used (no prefixes/suffixes)
        []
        []
        (handleReservedNames appendSingleQuote reservedVarNames)
        ctxTypeVarCName

    mangleConstrName :: ConstrContext -> HsName NsConstr
    mangleConstrName ConstrContext{..} =
      HsName $ "Mk" <> getHsName (mangleTypeConstrName ctxConstrTypeCtx)

    mangleVarName :: VarContext -> HsName NsVar
    mangleVarName = \case
      VarContext{..} ->
        translateName
          (camelCaseCName dropInvalidChar)
          joinWithCamelCase -- not used (no prefixes/suffixes)
          []
          []
          (handleReservedNames appendSingleQuote reservedVarNames)
          ctxVarCName
      EnumVarContext{..} ->
        HsName $ "un" <> getHsName (mangleTypeConstrName ctxEnumVarTypeCtx)
      FieldVarContext{..} ->
        translateName
          (camelCaseCName dropInvalidChar)
          joinWithCamelCase
          [ if ctxFieldVarSingleConstr
              then getHsName $ mangleTypeConstrName ctxFieldVarTypeCtx
              else
                getHsName $ mangleConstrName (ConstrContext ctxFieldVarTypeCtx)
          ]
          []
          handleReservedNone
          ctxFieldVarCName
