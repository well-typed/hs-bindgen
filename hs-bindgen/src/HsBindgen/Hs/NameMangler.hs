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
  ) where

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text qualified as T
import Numeric (showHex)

import HsBindgen.C.AST
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.NameMangler.API
import HsBindgen.Hs.NameMangler.DSL qualified as DSL
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
          (DSL.mkHsNamePrefixInvalid "C")
          DSL.handleOverrideNone
          (handleReservedNames appendSingleQuote DSL.reservedTypeNames)
          ctxTypeConstrCName
      StructTypeConstrContext{..} ->
        translateDeclPath
          (maintainCName escapeInvalidChar)
          DSL.joinWithSnakeCase
          (DSL.mkHsNamePrefixInvalid "C")
          DSL.handleOverrideNone
          (handleReservedNames appendSingleQuote DSL.reservedTypeNames)
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
          DSL.mkHsVarName
          DSL.handleOverrideNone
          (handleReservedNames appendSingleQuote DSL.reservedVarNames)
          ctxVarCName
      EnumVarContext{..} ->
        HsName $ "un" <> getHsName (mangleTypeConstrContext ctxEnumVarTypeCtx)
      FieldVarContext{..} ->
        translateName
          (maintainCName escapeInvalidChar)
          (Just $ DSL.joinWithSnakeCase{DSL.extraPrefixes =
              [getHsName (mangleTypeConstrContext ctxFieldVarTypeCtx)]
            })
          DSL.mkHsVarName
          DSL.handleOverrideNone
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
          (Just $ DSL.joinWithCamelCase{DSL.extraPrefixes = ["C"]})
          DSL.mkHsNameDropInvalid
          DSL.handleOverrideNone
          (handleReservedNames appendSingleQuote DSL.reservedTypeNames)
          ctxTypeConstrCName
      StructTypeConstrContext{..} ->
        translateDeclPath
          (camelCaseCName dropInvalidChar)
          DSL.joinWithCamelCase
          (DSL.mkHsNamePrefixInvalid "C")
          DSL.handleOverrideNone
          (handleReservedNames appendSingleQuote DSL.reservedTypeNames)
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
          DSL.mkHsVarName
          DSL.handleOverrideNone
          (handleReservedNames appendSingleQuote DSL.reservedVarNames)
          ctxVarCName
      EnumVarContext{..} ->
        HsName $ "un" <> getHsName (mangleTypeConstrContext ctxEnumVarTypeCtx)
      FieldVarContext{..} ->
        translateName
          (camelCaseCName dropInvalidChar)
          (Just $ DSL.joinWithCamelCase{DSL.extraPrefixes =
               [getHsName (mangleTypeConstrContext ctxFieldVarTypeCtx)]
             })
          DSL.mkHsVarName
          DSL.handleOverrideNone
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
  -> Maybe DSL.JoinParts      -- ^ Join parts (if any)
  -> DSL.GenerateName ns      -- ^ Construct an 'HsName'
  -> DSL.Overrides            -- ^ Override translation
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
    let input = DSL.maybeJoinPartsWith joinParts (translate cname)
        name  = mkHsName input
    in  handleReserved $ DSL.useOverride input name $
          DSL.override overrides (Just cname) name

-- | Translate a 'DeclPath' to an 'HsName'
--
-- The first parameter is a function that gets parts of a 'DeclPath' to include
-- in the translation.  Default 'getDeclPathParts' is provided in this module.
--
-- See 'translateName' for documentation of the other parameters.
translateDeclPath ::
     SingNamespace ns
  => (CName -> Text)          -- ^ Translate a 'CName'
  -> DSL.JoinParts            -- ^ Join parts of a name
  -> DSL.GenerateName ns      -- ^ Construct an 'HsName'
  -> DSL.Overrides            -- ^ Override translation
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
    let input = DSL.joinPartsWith joinParts $ map translate (getDeclPathParts declPath)
        name  = mkHsName input
    in  handleReserved $ DSL.useOverride input name $
          DSL.override overrides (getCName' declPath) name
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

