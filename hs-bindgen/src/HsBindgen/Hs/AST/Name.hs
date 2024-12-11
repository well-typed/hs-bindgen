module HsBindgen.Hs.AST.Name (
    -- * Definition
    Namespace(..)
  , SNamespace(..)
  , namespaceOf
  , SingNamespace(..)
  , HsName(..)
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
  , translateDeclPath
  , getDeclPathParts
  , maintainCName
  , camelCaseCName
  , dropInvalidChar
  , escapeInvalidChar
  , isValidChar
  , joinWithConcat
  , joinWithSnakeCase
  , joinWithCamelCase
  , mkHsNamePrefixInvalid
  , mkHsNameDropInvalid
  , handleOverrideNone
  , handleOverrideMap
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
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String
import Data.Text qualified as T
import Numeric (showHex)

import HsBindgen.Imports
import HsBindgen.C.AST (CName(..), DeclName(..), DeclPath(..))

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
  deriving (Eq, Ord)

-- | Namespace singleton
data SNamespace :: Namespace -> Star where
  SNsModuleName :: SNamespace 'NsModuleName
  SNsTypeClass  :: SNamespace 'NsTypeClass
  SNsTypeConstr :: SNamespace 'NsTypeConstr
  SNsTypeVar    :: SNamespace 'NsTypeVar
  SNsConstr     :: SNamespace 'NsConstr
  SNsVar        :: SNamespace 'NsVar

-- | Get the namespace of a namespace singleton
namespaceOf :: SNamespace ns -> Namespace
namespaceOf = \case
    SNsModuleName -> NsModuleName
    SNsTypeClass  -> NsTypeClass
    SNsTypeConstr -> NsTypeConstr
    SNsTypeVar    -> NsTypeVar
    SNsConstr     -> NsConstr
    SNsVar        -> NsVar

-- | Namespace singleton class
class SingNamespace ns where
  singNamespace :: SNamespace ns

instance SingNamespace 'NsModuleName where
  singNamespace = SNsModuleName

instance SingNamespace 'NsTypeClass where
  singNamespace = SNsTypeClass

instance SingNamespace 'NsTypeConstr where
  singNamespace = SNsTypeConstr

instance SingNamespace 'NsTypeVar where
  singNamespace = SNsTypeVar

instance SingNamespace 'NsConstr where
  singNamespace = SNsConstr

instance SingNamespace 'NsVar where
  singNamespace = SNsVar

-- | Haskell name in namespace @ns@
newtype HsName (ns :: Namespace) = HsName { getHsName :: Text }
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)

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
  | -- | Context for structures
    StructTypeConstrContext {
      -- | Structure declaration path
      ctxStructTypeConstrDeclPath :: DeclPath
    }
  deriving stock (Eq, Show)

-- | Context for creating Haskell type variable names
data TypeVarContext = TypeVarContext {
      -- | Type that the type variable is for
      ctxTypeVarTypeCtx :: TypeConstrContext
    , -- | Type variable name source
      ctxTypeVarCName :: CName
    }
  | MacroTypeVarContext {
      -- | Type variable name source
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
-- Any prefixes and suffixes specified must result in a valid name.
--
-- The constructor function must return an 'HsName' that is valid for the
-- specified namespace.  Two constructor functions are provided in this module:
-- 'mkHsNamePrefixInvalid' and 'mkHsNameDropInvalid'.
--
-- The override function may be used to override translations of Haskell names.
-- The Haskell name must be valid for the specified namespace.  Two override
-- functions are provided in this module: 'handleOverrideNone' and
-- 'handleOverrideMap'.
--
-- The reserved name function may be used to change names that would cause
-- confusion or a compilation error.  Two reserved name functions are provided
-- in this module: 'handleReservedNone' and 'handleReservedNames'.
translateName ::
     (CName -> Text)                  -- ^ Translate a 'CName'
  -> ([Text] -> Text)                 -- ^ Join parts of a name
  -> [Text]                           -- ^ Prefixes
  -> [Text]                           -- ^ Suffixes
  -> (Text -> HsName ns)              -- ^ Construct an 'HsName'
  -> (HsName ns -> Maybe (HsName ns)) -- ^ Override translation
  -> (HsName ns -> HsName ns)         -- ^ Handle reserved names
  -> CName
  -> HsName ns
translateName
  translate
  joinParts
  prefixes
  suffixes
  mkHsName
  override
  handleReserved
  cname =
    let name = mkHsName . joinParts $ prefixes ++ translate cname : suffixes
    in  handleReserved $ fromMaybe name (override name)

-- | Translate a 'DeclPath' to an 'HsName'
--
-- The first parameter is a function that gets parts of a 'DeclPath' to include
-- in the translation.  Default 'getDeclPathParts' is provided in this module.
--
-- See 'translateName' for documentation of the other parameters.
translateDeclPath ::
     (DeclPath -> [CName])            -- ^ Get parts from a 'DeclPath'
  -> (CName -> Text)                  -- ^ Translate a 'CName'
  -> ([Text] -> Text)                 -- ^ Join parts of a name
  -> [Text]                           -- ^ Prefixes
  -> [Text]                           -- ^ Suffixes
  -> (Text -> HsName ns)              -- ^ Construct an 'HsName'
  -> (HsName ns -> Maybe (HsName ns)) -- ^ Override translation
  -> (HsName ns -> HsName ns)         -- ^ Handle reserved names
  -> DeclPath
  -> HsName ns
translateDeclPath
  getParts
  translate
  joinParts
  prefixes
  suffixes
  mkHsName
  override
  handleReserved
  declPath =
    let name = mkHsName . joinParts $
          prefixes ++ map translate (getParts declPath) ++ suffixes
    in  handleReserved $ fromMaybe name (override name)

-- | Default 'DeclPath' translation
getDeclPathParts :: DeclPath -> [CName]
getDeclPathParts = aux
  where
    aux :: DeclPath -> [CName]
    aux = \case
      DeclPathTop -> ["ANONYMOUS"] -- shouldn't happen
      DeclPathStruct declName path -> case declName of
        DeclNameNone      -> aux path
        DeclNameTag n     -> [n]
        DeclNameTypedef n -> [n]
      DeclPathField n path -> aux path ++ [n]

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
-- Leading and trailing underscores are assumed to have special meaning and
-- are preserved.  All other underscores are removed.  Letters following
-- (preserved or removed) underscores are changed to uppercase.
--
-- The invalid character function must return a 'String' that only contains
-- valid characters.  Two invalid character functions are provided in this
-- module: 'dropInvalidChar' and 'escapeInvalidChar'.
--
-- Note that a single quote (@'@) is not valid in C names, and it is handled
-- specially.  Any single quotes in the input are treated as invalid.
camelCaseCName :: (Char -> String) -> CName -> Text
camelCaseCName f = T.pack . start False . T.unpack . getCName
  where
    start :: Bool -> String -> String
    start isUp = \case
      c:cs
        | c == '_'      -> c : start True cs
        | isValidChar c -> (if isUp then Char.toUpper c else c) : aux 0 cs
        | otherwise     -> f c ++ aux 0 cs
      []                -> []

    aux :: Int -> String -> String
    aux !numUs = \case
      c:cs
        | c == '_'      -> aux (numUs + 1) cs
        | isValidChar c -> (if numUs > 0 then Char.toUpper c else c) : aux 0 cs
        | otherwise     -> f c ++ aux 0 cs
      []                -> List.replicate numUs '_'

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

-- | Construct an 'HsName', changing the case of the first character or adding a
-- prefix if the first character is invalid
--
-- >>> mkHsNamePrefixInvalid @NsTypeConstr "C" "_foo"
-- "C_foo"
mkHsNamePrefixInvalid :: forall ns.
     SingNamespace ns
  => Text  -- ^ Prefix to use when first character invalid
  -> Text
  -> HsName ns
mkHsNamePrefixInvalid prefix = HsName . case singNamespace @ns of
    SNsModuleName -> auxU
    SNsTypeClass  -> auxU
    SNsTypeConstr -> auxU
    SNsTypeVar    -> auxL
    SNsConstr     -> auxU
    SNsVar        -> auxL
  where
    auxU :: Text -> Text
    auxU t = case T.uncons t of
      Just (c, t')
        | Char.isLetter c -> T.cons (Char.toUpper c) t'
        | otherwise       -> prefix <> t
      Nothing             -> prefix

    auxL :: Text -> Text
    auxL t = case T.uncons t of
      Just (c, t') -> T.cons (Char.toLower c) t'
      Nothing      -> prefix

-- | Construct an 'HsName', changing the case of the first character after
-- dropping any invalid first characters
--
-- >>> mkHsNameDropInvalid @NsTypeConstr "_foo"
-- "Foo"
mkHsNameDropInvalid :: forall ns. SingNamespace ns => Text -> HsName ns
mkHsNameDropInvalid = HsName . case singNamespace @ns of
    SNsModuleName -> auxU
    SNsTypeClass  -> auxU
    SNsTypeConstr -> auxU
    SNsTypeVar    -> auxL
    SNsConstr     -> auxU
    SNsVar        -> auxL
  where
    auxU :: Text -> Text
    auxU t = case T.uncons (T.dropWhile (not . Char.isLetter) t) of
      Just (c, t') -> T.cons (Char.toUpper c) t'
      Nothing      -> "X"

    auxL :: Text -> Text
    auxL t = case T.uncons t of
      Just (c, t') -> T.cons (Char.toLower c) t'
      Nothing      -> "x"

-- | Do not override any translations
handleOverrideNone :: HsName ns -> Maybe (HsName ns)
handleOverrideNone = const Nothing

-- | Override translations of Haskell names using a map
handleOverrideMap :: forall ns.
     SingNamespace ns
  => Map Namespace (Map Text Text)
  -> HsName ns
  -> Maybe (HsName ns)
handleOverrideMap overrideMap name = do
    nsMap <- Map.lookup (namespaceOf (singNamespace @ns)) overrideMap
    HsName <$> Map.lookup (getHsName name) nsMap

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
-- With this name mangler, names are changed as little as possible.  In general,
-- any invalid characters are escaped, and the first character is converted to
-- uppercase/lowercase as needed for the namespace.  Collision with a reserved
-- word is resolved by appending a @'@ character.
--
-- Details:
--
-- * Module, type class, type constructor, and constructor names must start with
--   an uppercase letter.  Prefix @C@ is added if the first character is not a
--   letter.
-- * The type constructor name for an anonymous structure/union for a named
--   field is the name of the type and the name of the field joined by
--   underscore.
-- * A constructor name is always the same as the type constructor name.
-- * The accessor name for a @newtype@ wrapper created for an enumeration is
--   @un@ concatenated to the type name.
-- * The accessor name for a structure/union field is the type name and the
--   field name, joined by underscore.
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
        (maintainCName escapeInvalidChar)
        joinWithSnakeCase -- not used (no prefixes/suffixes)
        []
        []
        (mkHsNamePrefixInvalid "C")
        handleOverrideNone
        handleReservedNone
        ctxModuleNameCName

    mangleTypeClassName :: TypeClassContext -> HsName NsTypeClass
    mangleTypeClassName TypeClassContext{..} =
      translateName
        (maintainCName escapeInvalidChar)
        joinWithSnakeCase -- not used (no prefixes/suffixes)
        []
        []
        (mkHsNamePrefixInvalid "C")
        handleOverrideNone
        handleReservedNone
        ctxTypeClassCName

    mangleTypeConstrName :: TypeConstrContext -> HsName NsTypeConstr
    mangleTypeConstrName = \case
      TypeConstrContext{..} ->
        translateName
          (maintainCName escapeInvalidChar)
          joinWithSnakeCase -- not used (no prefixes/suffixes)
          []
          []
          (mkHsNamePrefixInvalid "C")
          handleOverrideNone
          (handleReservedNames appendSingleQuote reservedTypeNames)
          ctxTypeConstrCName
      StructTypeConstrContext{..} ->
        translateDeclPath
          getDeclPathParts
          (maintainCName escapeInvalidChar)
          joinWithSnakeCase
          []
          []
          (mkHsNamePrefixInvalid "C")
          handleOverrideNone
          (handleReservedNames appendSingleQuote reservedTypeNames)
          ctxStructTypeConstrDeclPath

    mangleTypeVarName :: TypeVarContext -> HsName NsTypeVar
    mangleTypeVarName ctxt =
      translateName
        (maintainCName escapeInvalidChar)
        joinWithSnakeCase -- not used (no prefixes/suffixes)
        []
        []
        mkHsNameDropInvalid
        handleOverrideNone
        (handleReservedNames appendSingleQuote reservedVarNames)
        (ctxTypeVarCName ctxt)

    mangleConstrName :: ConstrContext -> HsName NsConstr
    mangleConstrName ConstrContext{..} =
      HsName $ getHsName (mangleTypeConstrName ctxConstrTypeCtx)

    mangleVarName :: VarContext -> HsName NsVar
    mangleVarName = \case
      VarContext{..} ->
        translateName
          (maintainCName escapeInvalidChar)
          joinWithSnakeCase -- not used (no prefixes/suffixes)
          []
          []
          mkHsNameDropInvalid
          handleOverrideNone
          (handleReservedNames appendSingleQuote reservedVarNames)
          ctxVarCName
      EnumVarContext{..} ->
        HsName $ "un" <> getHsName (mangleTypeConstrName ctxEnumVarTypeCtx)
      FieldVarContext{..} ->
        translateName
          (maintainCName escapeInvalidChar)
          joinWithSnakeCase
          [ getHsName (mangleTypeConstrName ctxFieldVarTypeCtx)
          ]
          []
          mkHsNameDropInvalid
          handleOverrideNone
          handleReservedNone  -- not needed since contains underscore
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
        mkHsNameDropInvalid
        handleOverrideNone
        handleReservedNone
        ctxModuleNameCName

    mangleTypeClassName :: TypeClassContext -> HsName NsTypeClass
    mangleTypeClassName TypeClassContext{..} =
      translateName
        (camelCaseCName dropInvalidChar)
        joinWithCamelCase -- not used (no prefixes/suffixes)
        []
        []
        mkHsNameDropInvalid
        handleOverrideNone
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
          mkHsNameDropInvalid
          handleOverrideNone
          (handleReservedNames appendSingleQuote reservedTypeNames)
          ctxTypeConstrCName
      StructTypeConstrContext{..} ->
        translateDeclPath
          getDeclPathParts
          (camelCaseCName dropInvalidChar)
          joinWithCamelCase
          []
          []
          (mkHsNamePrefixInvalid "C")
          handleOverrideNone
          (handleReservedNames appendSingleQuote reservedTypeNames)
          ctxStructTypeConstrDeclPath

    mangleTypeVarName :: TypeVarContext -> HsName NsTypeVar
    mangleTypeVarName ctxt =
      translateName
        (maintainCName dropInvalidChar)
        joinWithSnakeCase -- not used (no prefixes/suffixes)
        []
        []
        mkHsNameDropInvalid
        handleOverrideNone
        (handleReservedNames appendSingleQuote reservedVarNames)
        (ctxTypeVarCName ctxt)

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
          mkHsNameDropInvalid
          handleOverrideNone
          (handleReservedNames appendSingleQuote reservedVarNames)
          ctxVarCName
      EnumVarContext{..} ->
        HsName $ "un" <> getHsName (mangleTypeConstrName ctxEnumVarTypeCtx)
      FieldVarContext{..} ->
        translateName
          (camelCaseCName dropInvalidChar)
          joinWithCamelCase
          [ getHsName (mangleTypeConstrName ctxFieldVarTypeCtx)
          ]
          []
          mkHsNameDropInvalid
          handleOverrideNone
          handleReservedNone
          ctxFieldVarCName
