{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Building blocks for constructing name manglers
--
-- This does not depend on any other @NameMangler.*@ module.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Hs.NameMangler.DSL qualified as DSL
module HsBindgen.Hs.NameMangler.DSL (
    -- * Reserved Names
    -- $ReservedNames
    reservedVarNames
  , reservedTypeNames
  , haskellKeywords
  , ghcExtensionKeywords
  , hsBindgenReservedTypeNames
  , hsBindgenReservedVarNames
  , sanityReservedTypeNames
  , sanityReservedVarNames
    -- * Joining parts of a name
  , JoinParts(..)
  , joinWithSnakeCase
  , joinWithCamelCase
  , joinWithConcat
  , joinPartsWith
  , maybeJoinPartsWith
    -- * Constructing Haskell identifiers
  , NameRuleSet(..)
  , NamespaceRuleSet
  , GenerateName(..)
  , generateName
  , dropInvalidChar
  , escapeInvalidChar
  , mkHsNamePrefixInvalid
  , mkHsNameDropInvalid
  , mkHsVarName
    -- * Overrides
  , Overrides(..)
  , handleOverrideNone
  , handleOverrideMap
  , useOverride
  ) where

import Control.Exception
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric (showHex)

import HsBindgen.C.AST
import HsBindgen.Errors
import HsBindgen.Hs.AST.Name

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
    , joinParts
    }

-- | Join parts of a name by concatenating them
joinWithConcat :: JoinParts
joinWithConcat = mkJoinParts Text.concat

-- | Join parts of a name with underscores (@_@)
joinWithSnakeCase :: JoinParts
joinWithSnakeCase = mkJoinParts $ Text.intercalate "_"

-- | Join parts of a name in @camelCase@ style
--
-- The first character of all parts but the first is changed to uppercase (if it
-- is a letter), and the results are concatenated.
--
-- Since this function may change the case of letters, it can cause name
-- collisions when different C names only differ by case of the first letter.
joinWithCamelCase :: JoinParts
joinWithCamelCase = mkJoinParts $ \case
    (t:ts) -> Text.concat $ t : map upperFirstChar ts
    []     -> Text.empty
  where
    upperFirstChar :: Text -> Text
    upperFirstChar t = case Text.uncons t of
      Just (c, t') -> Text.cons (Char.toUpper c) t'
      Nothing      -> t

joinPartsWith :: JoinParts -> [Text] -> Text
joinPartsWith JoinParts{extraPrefixes, extraSuffixes, joinParts} parts =
    joinParts $ extraPrefixes ++ parts ++ extraSuffixes

maybeJoinPartsWith :: Maybe JoinParts -> Text -> Text
maybeJoinPartsWith Nothing         = id
maybeJoinPartsWith (Just joinParts) = joinPartsWith joinParts . (:[])

{-------------------------------------------------------------------------------
  Constructing Haskell identifiers
-------------------------------------------------------------------------------}

-- | Generate Haskell name from C name
data GenerateName ns = GenerateName {
      -- | Process invalid characters
      --
      -- Called on characters that are invalid anywhere in a Haskell identifier.
      -- Defaults to 'escapeInvalidChar'; see also 'dropInvalidChar'.
      onInvalid :: Char -> String

      -- | Generate the final name, after other processing is complete
      --
      -- Name generation is allowed to fail (depending on the policy, there are
      -- circumstances in which we cannot generate a name). When this happens,
      -- we require a name override.
    , generateFinalName :: Text -> Maybe (HsName ns)
    }

generateName :: GenerateName ns -> Text -> Maybe (HsName ns)
generateName GenerateName{onInvalid, generateFinalName} =
      generateFinalName
    . processInvalid
  where
    processInvalid :: Text -> Text
    processInvalid = Text.pack . go . Text.unpack
      where
        go :: String -> String
        go (c:cs)
          | isValidChar c = c : go cs
          | otherwise     = onInvalid c ++ go cs
        go []            = ""

        isValidChar :: Char -> Bool
        isValidChar c = Char.isAlphaNum c || c == '_'

mkGenerateName ::
     (Text -> Maybe (HsName ns))
     -- ^ Produce the final name
  -> GenerateName ns
mkGenerateName generateFinalName = GenerateName {
      onInvalid = escapeInvalidChar
    , generateFinalName
    }

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
    (c, t) <- Text.uncons rest
    Just $ HsName $ Text.cons (Char.toUpper c) t

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
mkHsOtherName f = mkGenerateName $ \t ->
    let (nonletters, rest) = Text.break Char.isLetter t in
    if Text.null nonletters then
      case Text.uncons rest of
        Just (c, t') -> Just $ HsName $ Text.cons (Char.toUpper c) t'
        Nothing      -> panicEmptyName
    else
      f nonletters rest

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
mkHsVarName = mkGenerateName $ \t -> Just $ HsName $
    case Text.uncons t of
      Just (c, t') -> Text.cons (Char.toLower c) t'
      Nothing      -> panicEmptyName

panicEmptyName :: a
panicEmptyName = panicPure "mkHsNameDropInvalid: empty name"

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



