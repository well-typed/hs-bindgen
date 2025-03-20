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
  , handleReservedNone
  , handleReservedNames
  , appendSingleQuote
  ) where

import Data.Maybe (maybeToList)
import Data.Set qualified as Set

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
    generateName :: DSL.GenerateName
    generateName = DSL.defaultGenerateName

    mangleTypeConstrContext :: TypeConstrContext -> HsName NsTypeConstr
    mangleTypeConstrContext = \case
      TypeConstrContext{..} ->
        translateName
          generateName
          DSL.overridesNone
          (handleReservedNames appendSingleQuote DSL.reservedTypeNames)
          [ctxTypeConstrCName]
      StructTypeConstrContext{..} ->
        translateDeclPath
          generateName
          DSL.overridesNone
          (handleReservedNames appendSingleQuote DSL.reservedTypeNames)
          ctxStructTypeConstrDeclPath

    mangleConstrContext :: ConstrContext -> HsName NsConstr
    mangleConstrContext ConstrContext{..} =
      HsName $ getHsName (mangleTypeConstrContext ctxConstrTypeCtx)

    mangleVarContext :: VarContext -> HsName NsVar
    mangleVarContext = \case
      VarContext{..} ->
        translateName
          generateName
          DSL.overridesNone
          (handleReservedNames appendSingleQuote DSL.reservedVarNames)
          [ctxVarCName]
      EnumVarContext{..} ->
        HsName $ "un" <> getHsName (mangleTypeConstrContext ctxEnumVarTypeCtx)
      FieldVarContext{..} ->
        translateName
          generateName
          DSL.overridesNone
          handleReservedNone  -- not needed since contains underscore
          [ CName . getHsName $ mangleTypeConstrContext ctxFieldVarTypeCtx
          , ctxFieldVarCName
          ]

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
    generateName :: DSL.GenerateName
    generateName = DSL.GenerateName{
          processCName  = DSL.camelCaseCName
        , onInvalidChar = DSL.dropInvalidChar
        , joinNames     = DSL.joinNamesCamelCase
        , applyRuleSet  = DSL.modifyFirstLetter DSL.dropInvalidFirst
        }

    mangleTypeConstrContext :: TypeConstrContext -> HsName NsTypeConstr
    mangleTypeConstrContext = \case
      TypeConstrContext{..} ->
        translateName
          generateName
          DSL.overridesNone
          (handleReservedNames appendSingleQuote DSL.reservedTypeNames)
          [ "C"
          , ctxTypeConstrCName
          ]
      StructTypeConstrContext{..} ->
        translateDeclPath
          generateName
          DSL.overridesNone
          (handleReservedNames appendSingleQuote DSL.reservedTypeNames)
          ctxStructTypeConstrDeclPath

    mangleConstrContext :: ConstrContext -> HsName NsConstr
    mangleConstrContext ConstrContext{..} =
      HsName $ "Mk" <> getHsName (mangleTypeConstrContext ctxConstrTypeCtx)

    mangleVarContext :: VarContext -> HsName NsVar
    mangleVarContext = \case
      VarContext{..} ->
        translateName
          generateName
          DSL.overridesNone
          (handleReservedNames appendSingleQuote DSL.reservedVarNames)
          [ctxVarCName]
      EnumVarContext{..} ->
        HsName $ "un" <> getHsName (mangleTypeConstrContext ctxEnumVarTypeCtx)
      FieldVarContext{..} ->
        translateName
          generateName
          DSL.overridesNone
          handleReservedNone
          [ CName . getHsName $ mangleTypeConstrContext ctxFieldVarTypeCtx
          , ctxFieldVarCName
          ]

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
-- override functions are provided in this module: 'overridesNone' and
-- 'handleOverrideMap'.
--
-- The reserved name function may be used to change names that would cause
-- confusion or a compilation error.  Two reserved name functions are provided
-- in this module: 'handleReservedNone' and 'handleReservedNames'.
translateName ::
     SingNamespace ns
  => DSL.GenerateName         -- ^ Construct an 'HsName'
  -> DSL.Overrides            -- ^ Override translation
  -> (HsName ns -> HsName ns) -- ^ Handle reserved names
  -> [CName] -> HsName ns
translateName
  mkHsName
  overrides
  handleReserved
  cnames =
    let name = DSL.generateName mkHsName cnames
    in  handleReserved $ DSL.useOverride cnames name $
          DSL.override overrides cnames name

-- | Translate a 'DeclPath' to an 'HsName'
--
-- The first parameter is a function that gets parts of a 'DeclPath' to include
-- in the translation.  Default 'getDeclPathParts' is provided in this module.
--
-- See 'translateName' for documentation of the other parameters.
translateDeclPath ::
     SingNamespace ns
  => DSL.GenerateName         -- ^ Construct an 'HsName'
  -> DSL.Overrides            -- ^ Override translation
  -> (HsName ns -> HsName ns) -- ^ Handle reserved names
  -> DeclPath
  -> HsName ns
translateDeclPath
  mkHsName
  overrides
  handleReserved
  declPath =
    let cnames = getDeclPathParts declPath
        name   = DSL.generateName mkHsName cnames
    in  handleReserved $ DSL.useOverride cnames name $
          DSL.override overrides cnames name

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

