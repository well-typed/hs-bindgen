{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Hs.AST.Name (
    -- * Definition
    Namespace(..)
  , HsName(..)
  , MkHsName(..)
    -- * Contexts
  , NsVarContext(..)
  , NsConstrContext(..)
  , NsTypeVarContext(..)
  , NsTypeConstrContext(..)
  , NsTypeClassContext(..)
  , NsModuleNameContext(..)
    -- * Options
  , NameManglingOptions(..)
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
  , varReservedNames
  , typeVarReservedNames
  , handleModuleNameParent
    -- ** Defaults
  , defaultNameManglingOptions
  , haskellNameManglingOptions
    -- * Conversion
  , toHsName
  ) where

import Data.Char qualified as Char
import Data.Kind
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Numeric

import HsBindgen.C.AST (CName(..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Namespace
--
-- See section 1.4, "Namespaces" of the Haskell report
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-130001.4>
data Namespace =
    NsVar
  | NsConstr
  | NsTypeVar
  | NsTypeConstr
  | NsTypeClass
  | NsModuleName

-- | Haskell name in namespace @ns@
newtype HsName (ns :: Namespace) = HsName { getHsName :: Text }
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)

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

instance MkHsName NsVar where
  mkHsName = mkHsName' Char.toLower

instance MkHsName NsConstr where
  mkHsName = mkHsName' Char.toUpper

instance MkHsName NsTypeVar where
  mkHsName = mkHsName' Char.toLower

instance MkHsName NsTypeConstr where
  mkHsName = mkHsName' Char.toUpper

instance MkHsName NsTypeClass where
  mkHsName = mkHsName' Char.toUpper

instance MkHsName NsModuleName where
  mkHsName = mkHsName' Char.toUpper

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

-- | Local context for translating Haskell names in the 'NsVar' namespace
data NsVarContext =
    -- | No context provided
    EmptyNsVarContext
    -- | Context for @enum@ fields (accessors)
  | EnumContext {
      ctxEnumTypeName :: HsName NsTypeConstr
    }
  | -- | Context for record fields (accessors)
    FieldContext {
      -- | Name of the record type
      ctxFieldTypeName :: HsName NsTypeConstr
      -- | Name of the constructor
    , ctxFieldConstrName :: HsName NsConstr
      -- | Record type has a single constructor?
    , ctxFieldConstrSingle :: Bool
    }
  deriving stock (Eq, Show)

-- | Local context for translating Haskell names in the 'NsConstr' namespace
newtype NsConstrContext = NsConstrContext {
      -- | Name of the type that the constructor is for
      ctxConstrTypeName :: HsName NsTypeConstr
    }
  deriving stock (Eq, Show)

-- | Local context for translating Haskell names in the 'NsTypeVar' namespace
newtype NsTypeVarContext = NsTypeVarContext {
      -- | Name of the type that the type variable is for
      ctxTypeVarTypeName :: HsName NsTypeConstr
    }
  deriving stock (Eq, Show)

-- | Local context for translating Haskell names in the 'NsTypeConstr' namespace
data NsTypeConstrContext =
    -- | No context provided
    EmptyNsTypeConstrContext
  deriving stock (Eq, Show)

-- | Local context for translating Haskell names in the 'NsTypeClass' namespace
data NsTypeClassContext =
    -- | No context provided
    EmptyNsTypeClassContext
  deriving stock (Eq, Show)

-- | Local context for translating Haskell names in the 'NsModuleName' namespace
newtype NsModuleNameContext = NsModuleNameContext {
      -- | Name of the parent module
      ctxModuleParentName :: Maybe (HsName NsModuleName)
    }
  deriving stock (Eq, Show)

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

-- | Name mangling options
data NameManglingOptions = NameManglingOptions {
      -- | Create a Haskell module name
      nameManglingModule ::
           NsModuleNameContext
        -> CName
        -> HsName NsModuleName

      -- | Create a Haskell type class name
    , nameManglingTypeClass ::
           NsTypeClassContext
        -> CName
        -> HsName NsTypeClass

      -- | Create a Haskell type constructor name
    , nameManglingTypeConstr ::
           NsTypeConstrContext
        -> CName
        -> HsName NsTypeConstr

      -- | Create a Haskell type variable name
    , nameManglingTypeVar ::
           NsTypeVarContext
        -> CName
        -> HsName NsTypeVar

      -- | Create a Haskell constructor name
    , nameManglingConstr ::
           NsConstrContext
        -> CName
        -> HsName NsConstr

      -- | Create a Haskell variable name
    , nameManglingVar ::
           NsVarContext
        -> CName
        -> HsName NsVar
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

-- | If a name is in a list of reserved names, transform it
--
-- The transformation function must return a valid Haskell name.  One
-- transformation function is provided in this module: 'appendSingleQuote'.
--
-- For example, 'varReservedNames' and 'typeVarReservedNames' may be used to
-- avoid using reserved names for variables and type variables, respectively.
handleReservedNames :: (Text -> Text) -> [Text] -> HsName ns -> HsName ns
handleReservedNames f reserved name@(HsName t)
    | t `elem` reserved = HsName $ f t
    | otherwise         = name

-- | Append a single quote (@'@) to a name
appendSingleQuote :: Text -> Text
appendSingleQuote = (<> "'")

-- | Reserved names in the Haskell variable namespace
--
-- Reference:
--
-- * [Keywords](https://wiki.haskell.org/Keywords)
-- * [Stolen Syntax](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/stolen_syntax.html)
varReservedNames :: [Text]
varReservedNames =
    [ "as"
    , "case"
    , "class"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "else"
    , "family"
    , "forall"
    , "foreign"
    , "hiding"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "mdo"
    , "module"
    , "newtype"
    , "of"
    , "pattern"
    , "proc"
    , "qualified"
    , "rec"
    , "static"
    , "then"
    , "type"
    , "where"
    ]

-- | Reserved names in the Haskell type variable namespace
--
-- Reference:
--
-- * [`role`](https://gitlab.haskell.org/ghc/ghc/-/issues/18941)
typeVarReservedNames :: [Text]
typeVarReservedNames = "role" : varReservedNames

-- | Prepend the parent module name, joining using a @.@, if one is provided in
-- the context
handleModuleNameParent ::
     NsModuleNameContext
  -> HsName NsModuleName
  -> HsName NsModuleName
handleModuleNameParent NsModuleNameContext{..} name =
    case ctxModuleParentName of
      Just parentName -> HsName $ getHsName parentName <> "." <> getHsName name
      Nothing         -> name

{-------------------------------------------------------------------------------
  Options: Defaults
-------------------------------------------------------------------------------}

-- | Default name mangling options
--
-- These options attempt to provide a balance between safety and taste.
--
-- * Module names are transformed to @PascalCase@, dropping invalid characters.
-- * Type class names are transformed to @PascalCase@, escaping invalid
--   characters.
-- * Type constructors are prefixed with @C@, escaping invalid characters.
-- * Type variables have invalid characters escaped, and single quotes are
--   appended to reserved names.
-- * Constructors are prefixed with @MkC@, escaping invalid characters.
-- * Record fields are prefixed with the type name if the data type has a single
--   constructor or the constructor name otherwise, joined using an underscore,
--   escaping invalid characters.
-- * Enumeration fields are prefixed with @un@, joined using an underscore,
--   escaping invalid characters.
-- * Other variables have invalid characters escaped, and single quotes are
--   appended to reserved names.
defaultNameManglingOptions :: NameManglingOptions
defaultNameManglingOptions = NameManglingOptions {
    nameManglingModule = \ctx -> handleModuleNameParent ctx .
      translateName
        (camelCaseCName dropInvalidChar)
        joinWithConcat -- not used (no prefixes/suffixes)
        []
        []
        handleReservedNone
  , nameManglingTypeClass = \EmptyNsTypeClassContext ->
      translateName
        (camelCaseCName escapeInvalidChar)
        joinWithConcat -- not used (no prefixes/suffixes)
        []
        []
        handleReservedNone

  , nameManglingTypeConstr = \EmptyNsTypeConstrContext ->
      translateName
        (maintainCName escapeInvalidChar)
        joinWithConcat
        ["C"]
        []
        handleReservedNone

  , nameManglingTypeVar = \NsTypeVarContext{} ->
      translateName
        (maintainCName escapeInvalidChar)
        joinWithSnakeCase -- not used (no prefixes/suffixes)
        []
        []
        (handleReservedNames appendSingleQuote typeVarReservedNames)

  , nameManglingConstr = \NsConstrContext{} ->
      translateName
        (maintainCName escapeInvalidChar)
        joinWithConcat
        ["MkC"]
        []
        handleReservedNone

  , nameManglingVar = \case
      EmptyNsVarContext ->
        translateName
          (maintainCName escapeInvalidChar)
          joinWithSnakeCase -- not used (no prefixes/suffixes)
          []
          []
          (handleReservedNames appendSingleQuote varReservedNames)
      EnumContext{} ->
        translateName
          (maintainCName escapeInvalidChar)
          joinWithSnakeCase
          ["un"]
          []
          handleReservedNone
      FieldContext{..} ->
        translateName
          (maintainCName escapeInvalidChar)
          joinWithSnakeCase
          [ if ctxFieldConstrSingle
              then getHsName ctxFieldTypeName
              else getHsName ctxFieldConstrName
          ]
          []
          handleReservedNone
}

-- | Haskell-style name mangling options
--
-- These options provide Haskell-style names with a higher risk of name
-- collision.
--
-- * Module names are transformed to @PascalCase@, dropping invalid characters.
-- * Type class names are transformed to @PascalCase@, dropping invalid
--   characters.
-- * Type constructors are transformed to @PascalCase@ and prefixed with @C@,
--   escaping invalid characters.
-- * Type variables have invalid characters dropped, and single quotes are
--   appended to reserved names.
-- * Constructors are are transformed to @PascalCase@ and prefixed with @MkC@,
--   escaping invalid characters.
-- * Record fields are prefixed with the type name if the data type has a single
--   constructor or the constructor name otherwise, joined using @camelCase@,
--   dropping invalid characters.
-- * Enumeration fields are prefixed with @un@, joined using @camelCase@,
--   dropping invalid characters.
-- * Other variables have invalid characters dropped, and single quotes are
--   appended to reserved names.
haskellNameManglingOptions :: NameManglingOptions
haskellNameManglingOptions = NameManglingOptions {
    nameManglingModule = \ctx -> handleModuleNameParent ctx .
      translateName
        (camelCaseCName dropInvalidChar)
        joinWithCamelCase -- not used (no prefixes/suffixes)
        []
        []
        handleReservedNone

  , nameManglingTypeClass = \EmptyNsTypeClassContext ->
      translateName
        (camelCaseCName dropInvalidChar)
        joinWithCamelCase -- not used (no prefixes/suffixes)
        []
        []
        handleReservedNone

  , nameManglingTypeConstr = \EmptyNsTypeConstrContext ->
      translateName
        (camelCaseCName dropInvalidChar)
        joinWithCamelCase
        ["C"]
        []
        handleReservedNone

  , nameManglingTypeVar = \NsTypeVarContext{} ->
      translateName
        (maintainCName dropInvalidChar)
        joinWithSnakeCase -- not used (no prefixes/suffixes)
        []
        []
        (handleReservedNames appendSingleQuote typeVarReservedNames)

  , nameManglingConstr = \NsConstrContext{} ->
      translateName
        (camelCaseCName dropInvalidChar)
        joinWithCamelCase
        ["MkC"]
        []
        handleReservedNone

  , nameManglingVar = \case
      EmptyNsVarContext ->
        translateName
          (camelCaseCName dropInvalidChar)
          joinWithCamelCase -- not used (no prefixes/suffixes)
          []
          []
          (handleReservedNames appendSingleQuote varReservedNames)
      EnumContext{} ->
        translateName
          (camelCaseCName dropInvalidChar)
          joinWithCamelCase
          ["un"]
          []
          handleReservedNone
      FieldContext{..} ->
        translateName
          (camelCaseCName dropInvalidChar)
          joinWithCamelCase
          [ if ctxFieldConstrSingle
              then getHsName ctxFieldTypeName
              else getHsName ctxFieldConstrName
          ]
          []
          handleReservedNone
}

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | Name mangling
class ToHsName (ns :: Namespace) where
  type ToHsNameContext ns :: Type

  toHsName :: NameManglingOptions -> ToHsNameContext ns -> CName -> HsName ns

instance ToHsName NsVar where
  type ToHsNameContext NsVar = NsVarContext

  toHsName NameManglingOptions{nameManglingVar} = nameManglingVar

instance ToHsName NsConstr where
  type ToHsNameContext NsConstr = NsConstrContext

  toHsName NameManglingOptions{nameManglingConstr} = nameManglingConstr

instance ToHsName NsTypeVar where
  type ToHsNameContext NsTypeVar = NsTypeVarContext

  toHsName NameManglingOptions{nameManglingTypeVar} = nameManglingTypeVar

instance ToHsName NsTypeConstr where
  type ToHsNameContext NsTypeConstr = NsTypeConstrContext

  toHsName NameManglingOptions{nameManglingTypeConstr} = nameManglingTypeConstr

instance ToHsName NsTypeClass where
  type ToHsNameContext NsTypeClass = NsTypeClassContext

  toHsName NameManglingOptions{nameManglingTypeClass} = nameManglingTypeClass

instance ToHsName NsModuleName where
  type ToHsNameContext NsModuleName = NsModuleNameContext

  toHsName NameManglingOptions{nameManglingModule} = nameManglingModule
