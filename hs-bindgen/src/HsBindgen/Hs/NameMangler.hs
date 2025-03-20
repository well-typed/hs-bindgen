{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module HsBindgen.Hs.NameMangler (
    -- * Definition
    NameMangler -- opaque
    -- * Default Name Manglers
  , defaultNameMangler
  , haskellNameMangler
    -- * Using the name mangler
  , mangleTyconName
  , mangleDataconName
  , mangleDeconName
  , mangleFieldName
  , mangleVarName
    -- * Defining name manglers
  , translateName
  , translateDeclPath
  , getDeclPathParts
  , maintainCName
  , camelCaseCName
  , dropInvalidChar
  , escapeInvalidChar
  , isValidChar
  , handleReservedNone
  , handleReservedNames
  , appendSingleQuote
    -- ** Constructing Haskell identifiers
  , NameRuleSet(..)
  , NamespaceRuleSet
  , GenerateName
  , mkHsNamePrefixInvalid
  , mkHsNameDropInvalid
  , mkHsVarName
    -- ** Constructing a name out of multiple parts
  , JoinParts(..)
  , joinWithConcat
  , joinWithSnakeCase
  , joinWithCamelCase
    -- ** Overrides
  , Overrides(..)
  , handleOverrideNone
  , handleOverrideMap
    -- ** Reserved Names
    -- $ReservedNames
  , reservedVarNames
  , reservedTypeNames
  , haskellKeywords
  , ghcExtensionKeywords
  , hsBindgenReservedTypeNames
  , hsBindgenReservedVarNames
  , sanityReservedTypeNames
  , sanityReservedVarNames
  ) where

import Control.Exception
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text qualified as T
import Numeric (showHex)

import HsBindgen.C.AST
import HsBindgen.Errors
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.NameMangler.API
import HsBindgen.Imports

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
    mangleTypeConstrContext :: TypeConstrContext -> HsName NsTypeConstr
    mangleTypeConstrContext = \case
      TypeConstrContext{..} ->
        translateName
          (maintainCName escapeInvalidChar)
          Nothing
          (mkHsNamePrefixInvalid "C")
          handleOverrideNone
          (handleReservedNames appendSingleQuote reservedTypeNames)
          ctxTypeConstrCName
      StructTypeConstrContext{..} ->
        translateDeclPath
          (maintainCName escapeInvalidChar)
          joinWithSnakeCase
          (mkHsNamePrefixInvalid "C")
          handleOverrideNone
          (handleReservedNames appendSingleQuote reservedTypeNames)
          ctxStructTypeConstrDeclPath

    mangleConstrContext :: ConstrContext -> HsName NsConstr
    mangleConstrContext ConstrContext{..} =
      HsName $ getHsName (mangleTypeConstrContext ctxConstrTypeCtx)

    mangleVarContext :: VarContext -> HsName NsVar
    mangleVarContext = \case
      VarContext{..} ->
        translateName
          (maintainCName escapeInvalidChar)
          Nothing
          mkHsVarName
          handleOverrideNone
          (handleReservedNames appendSingleQuote reservedVarNames)
          ctxVarCName
      EnumVarContext{..} ->
        HsName $ "un" <> getHsName (mangleTypeConstrContext ctxEnumVarTypeCtx)
      FieldVarContext{..} ->
        translateName
          (maintainCName escapeInvalidChar)
          (Just $ joinWithSnakeCase{extraPrefixes =
              [getHsName (mangleTypeConstrContext ctxFieldVarTypeCtx)]
            })
          mkHsVarName
          handleOverrideNone
          handleReservedNone  -- not needed since contains underscore
          ctxFieldVarCName

-- | Haskell-style name mangler
--
-- This default provides Haskell-style names with a higher risk of name
-- collision.
--
-- * Type constructors are transformed to @PascalCase@ and prefixed with @C@,
--   escaping invalid characters.
-- * Constructors are transformed to @PascalCase@ and prefixed with @Mk@,
--   escaping invalid characters.
-- * Record fields are prefixed with the type name, joined using @camelCase@,
--   dropping invalid characters.
-- * Enumeration fields are prefixed with @un@, dropping invalid characters.
-- * Other variables have invalid characters dropped, and single quotes are
--   appended to reserved names.
haskellNameMangler :: NameMangler
haskellNameMangler = NameMangler{..}
  where
    mangleTypeConstrContext :: TypeConstrContext -> HsName NsTypeConstr
    mangleTypeConstrContext = \case
      TypeConstrContext{..} ->
        translateName
          (camelCaseCName dropInvalidChar)
          (Just $ joinWithCamelCase {extraPrefixes = ["C"]})
          mkHsNameDropInvalid
          handleOverrideNone
          (handleReservedNames appendSingleQuote reservedTypeNames)
          ctxTypeConstrCName
      StructTypeConstrContext{..} ->
        translateDeclPath
          (camelCaseCName dropInvalidChar)
          joinWithCamelCase
          (mkHsNamePrefixInvalid "C")
          handleOverrideNone
          (handleReservedNames appendSingleQuote reservedTypeNames)
          ctxStructTypeConstrDeclPath

    mangleConstrContext :: ConstrContext -> HsName NsConstr
    mangleConstrContext ConstrContext{..} =
      HsName $ "Mk" <> getHsName (mangleTypeConstrContext ctxConstrTypeCtx)

    mangleVarContext :: VarContext -> HsName NsVar
    mangleVarContext = \case
      VarContext{..} ->
        translateName
          (camelCaseCName dropInvalidChar)
          Nothing
          mkHsVarName
          handleOverrideNone
          (handleReservedNames appendSingleQuote reservedVarNames)
          ctxVarCName
      EnumVarContext{..} ->
        HsName $ "un" <> getHsName (mangleTypeConstrContext ctxEnumVarTypeCtx)
      FieldVarContext{..} ->
        translateName
          (camelCaseCName dropInvalidChar)
          (Just $ joinWithCamelCase{extraPrefixes =
               [getHsName (mangleTypeConstrContext ctxFieldVarTypeCtx)]
             })
          mkHsVarName
          handleOverrideNone
          handleReservedNone
          ctxFieldVarCName

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
-- The returned Haskell name must be valid for the specified namespace.  Two
-- override functions are provided in this module: 'handleOverrideNone' and
-- 'handleOverrideMap'.
--
-- The reserved name function may be used to change names that would cause
-- confusion or a compilation error.  Two reserved name functions are provided
-- in this module: 'handleReservedNone' and 'handleReservedNames'.
translateName ::
     SingNamespace ns
  => (CName -> Text)          -- ^ Translate a 'CName'
  -> Maybe JoinParts          -- ^ Join parts (if any)
  -> GenerateName ns          -- ^ Construct an 'HsName'
  -> Overrides                -- ^ Override translation
  -> (HsName ns -> HsName ns) -- ^ Handle reserved names
  -> CName
  -> HsName ns
translateName
  translate
  joinParts
  mkHsName
  overrides
  handleReserved
  cname =
    let input = maybeJoinPartsWith joinParts (translate cname)
        name  = mkHsName input
    in  handleReserved $ useOverride input name $
          override overrides (Just cname) name

-- | Translate a 'DeclPath' to an 'HsName'
--
-- The first parameter is a function that gets parts of a 'DeclPath' to include
-- in the translation.  Default 'getDeclPathParts' is provided in this module.
--
-- See 'translateName' for documentation of the other parameters.
translateDeclPath ::
     SingNamespace ns
  => (CName -> Text)          -- ^ Translate a 'CName'
  -> JoinParts                -- ^ Join parts of a name
  -> GenerateName ns          -- ^ Construct an 'HsName'
  -> Overrides                -- ^ Override translation
  -> (HsName ns -> HsName ns) -- ^ Handle reserved names
  -> DeclPath
  -> HsName ns
translateDeclPath
  translate
  joinParts
  mkHsName
  overrides
  handleReserved
  declPath =
    let input = joinPartsWith joinParts $ map translate (getDeclPathParts declPath)
        name  = mkHsName input
    in  handleReserved $ useOverride input name $
          override overrides (getCName' declPath) name
  where
    getCName' :: DeclPath -> Maybe CName
    getCName' = \case
      DeclPathAnon      _ctxt -> Nothing
      DeclPathName name _ctxt -> Just name

-- | Default 'DeclPath' translation
getDeclPathParts :: DeclPath -> [CName]
getDeclPathParts = \case
    DeclPathAnon ctxt    -> aux ctxt
    DeclPathName n _ctxt -> [n]
  where
    aux :: DeclPathCtxt -> [CName]
    aux DeclPathCtxtTop =
        []
    aux (DeclPathCtxtField struct field ctxt) =
        aux ctxt ++ maybeToList struct ++ [field]
    aux (DeclPathCtxtTypedef name) =
        [name]
    aux (DeclPathCtxtPtr ctxt) =
        aux ctxt ++ ["Deref"]

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

{-------------------------------------------------------------------------------
  Constructing Haskell identifiers
-------------------------------------------------------------------------------}

-- | Generate name
--
-- Name generation is allowed to fail (depending on the policy, there are
-- circumstances in which we cannot generate a name). When this happens, we
-- require a name override.
type GenerateName ns = Text -> Maybe (HsName ns)

data NameRuleSet =
    -- | Variables and type variables
    NameRuleSetVar

    -- | Constructors, type constructors, type classes and module names
  | NameRuleSetOther

type family NamespaceRuleSet (ns :: Namespace) :: NameRuleSet where
  NamespaceRuleSet NsTypeConstr = NameRuleSetOther
  NamespaceRuleSet NsConstr     = NameRuleSetOther
  NamespaceRuleSet NsVar        = NameRuleSetVar

-- | Construct an 'HsName', changing the case of the first character or adding a
-- prefix if the first character is invalid
--
-- >>> mkHsNamePrefixInvalid @NsTypeConstr "C" "_foo"
-- "C_foo"
mkHsNamePrefixInvalid :: forall ns.
     NamespaceRuleSet ns ~ NameRuleSetOther
  => Text  -- ^ Prefix to use when first character invalid
  -> GenerateName ns
mkHsNamePrefixInvalid prefix =
    mkHsOtherName $ \nonletters rest ->
     Just $ HsName $ prefix <> nonletters <> rest

-- | Construct an 'HsName', changing the case of the first character after
-- dropping any invalid first characters
--
-- Can return 'Nothing' if the C identifier started with an underscore, and then
-- contained only non-letters (e.g., @_123@).
--
-- >>> mkHsNameDropInvalid @NsTypeConstr "_foo"
-- "Foo"
mkHsNameDropInvalid :: NamespaceRuleSet ns ~ NameRuleSetOther => GenerateName ns
mkHsNameDropInvalid = mkHsOtherName $ \_nonletters rest -> do
    (c, t) <- T.uncons rest
    Just $ HsName $ T.cons (Char.toUpper c) t

-- | Construct Haskell (type or value) constructor identifier
--
-- Precondition: the name must not be empty.
--
-- The assumption is that the input is a C identifier, and so must start with
-- a letter or an underscore.
--
-- * If it is a letter, we make that letter uppercase.
-- * If it is an underscore, we strip off everything that's not a letter, and
--   then call the specified function on this prefix and the remaining suffix.
mkHsOtherName ::
     NamespaceRuleSet ns ~ NameRuleSetOther
  => (Text -> Text -> Maybe (HsName ns))
  -> GenerateName ns
mkHsOtherName f t
  | T.null nonletters
  = case T.uncons rest of
      Just (c, t') -> Just $ HsName $ T.cons (Char.toUpper c) t'
      Nothing      -> panicEmptyName
  | otherwise
  = f nonletters rest
  where
    (nonletters, rest) = T.break Char.isLetter t

-- | Construct Haskell variable identifier
--
-- The assumption is that the input is a C identifier, and so must start with
-- a letter or an underscore:
--
-- * If it is a letter, we make that letter lowercase.
-- * If it is an underscore, there is nothing to do.
mkHsVarName :: forall ns.
     NamespaceRuleSet ns ~ NameRuleSetVar
  => GenerateName ns
mkHsVarName t = Just $ HsName $
    case T.uncons t of
      Just (c, t') -> T.cons (Char.toLower c) t'
      Nothing      -> panicEmptyName

panicEmptyName :: a
panicEmptyName = panicPure "mkHsNameDropInvalid: empty name"

{-------------------------------------------------------------------------------
  Joining parts of a name
-------------------------------------------------------------------------------}

-- | Prefix and/or suffix for a name
data JoinParts = JoinParts {
      extraPrefixes :: [Text]
    , extraSuffixes :: [Text]
    , joinParts     :: [Text] -> Text
    }

mkJoinParts :: ([Text] -> Text) -> JoinParts
mkJoinParts joinParts = JoinParts {
      extraPrefixes = []
    , extraSuffixes = []
    , joinParts}

-- | Join parts of a name by concatenating them
joinWithConcat :: JoinParts
joinWithConcat = mkJoinParts T.concat

-- | Join parts of a name with underscores (@_@)
joinWithSnakeCase :: JoinParts
joinWithSnakeCase = mkJoinParts $ T.intercalate "_"

-- | Join parts of a name in @camelCase@ style
--
-- The first character of all parts but the first is changed to uppercase (if it
-- is a letter), and the results are concatenated.
--
-- Since this function may change the case of letters, it can cause name
-- collisions when different C names only differ by case of the first letter.
joinWithCamelCase :: JoinParts
joinWithCamelCase = mkJoinParts $ \case
    (t:ts) -> T.concat $ t : map upperFirstChar ts
    []     -> T.empty
  where
    upperFirstChar :: Text -> Text
    upperFirstChar t = case T.uncons t of
      Just (c, t') -> T.cons (Char.toUpper c) t'
      Nothing      -> t

joinPartsWith :: JoinParts -> [Text] -> Text
joinPartsWith JoinParts{extraPrefixes, extraSuffixes, joinParts} parts =
    joinParts $ extraPrefixes ++ parts ++ extraSuffixes

maybeJoinPartsWith :: Maybe JoinParts -> Text -> Text
maybeJoinPartsWith Nothing         = id
maybeJoinPartsWith (Just joinParts) = joinPartsWith joinParts . (:[])

{-------------------------------------------------------------------------------
  Overrides
-------------------------------------------------------------------------------}

-- | Override translations of Haskell names
--
-- Since not all generated Haskell names have corresponding C names, overriding
-- is primarily done based on the generated Haskell name.  In some cases the C
-- name can be used for disambiguation, if more than one C name is being
-- translated to the same Haskell name.
data Overrides = Overrides {
      override :: forall ns.
           SingNamespace ns
        => Maybe CName
           -- ^ C name, if available.
           --
           -- An example situation in which the C name is unavailable is
           -- anonymous structs nested inside other structs.
        -> Maybe (HsName ns)
           -- ^ Haskell name
           --
           -- This will be 'Nothing' only if we fail to construct the Haskell
           -- name altogether. For example, this can happen if a C type is
           -- called @_123@, and we are using the 'mkHsNameDropInvalid'
           -- policy; in this case, /all/ characters are invalid, and so we'd
           -- end up with nothing.
        -> Maybe (HsName ns)
    }

-- | Do not override any translations
handleOverrideNone :: Overrides
handleOverrideNone = Overrides $ \_cname _name -> Nothing

-- | Override translations of Haskell names using a map
handleOverrideMap ::
     Map Namespace (Map (Maybe Text) (Map (Maybe CName) Text))
  -> Overrides
handleOverrideMap overrideMap = Overrides aux
  where
    aux :: forall ns.
         SingNamespace ns
      => Maybe CName -> Maybe (HsName ns) -> Maybe (HsName ns)
    aux cname name = do
        nsMap <- Map.lookup (namespaceOf (singNamespace @ns)) overrideMap
        nMap  <- Map.lookup (getHsName <$> name) nsMap
        HsName <$> Map.lookup cname nMap

useOverride ::
     Text               -- ^ Input to name generation
  -> Maybe (HsName ns)  -- ^ Generated name (unless failed)
  -> Maybe (HsName ns)  -- ^ Override (if any)
  -> HsName ns
useOverride _     _           (Just name) = name
useOverride _     (Just name) _           = name
useOverride input Nothing     Nothing     = throw $ RequireOverride input

data RequireOverride = RequireOverride Text
  deriving stock (Show)

instance Exception RequireOverride where
  toException   = hsBindgenExceptionToException
  fromException = hsBindgenExceptionFromException

  displayException (RequireOverride input) = concat [
        "Require name override for "
      , show input
      ]

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
