{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Hs.AST.Name (
    -- * Definition
    HsName(..)
  , Namespace(..)
    -- * Options
  , NameManglingOptions(..)
  , NameManglingStrategy(..)
  , FieldContextPrefix(..)
  , InvalidCharAction(..)
  , defaultNameManglingOptions
  , safestNameManglingOptions
    -- * Contexts
  , NsVarContext(..)
  , NsConstrContext(..)
  , NsTypeVarContext(..)
  , NsTypeConstrContext(..)
  , NsTypeClassContext(..)
  , NsModuleNameContext(..)
    -- * Conversion
  , toHsName
  ) where

import Data.Char qualified as Char
import Data.Kind
import Data.Maybe
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

newtype HsName (ns :: Namespace) = HsName { getHsName :: Text }
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

-- | Name mangling options
data NameManglingOptions = NameManglingOptions {
      -- | Name mangling strategy for names in the 'NsVar' namespace
      nameManglingStrategyVar :: NameManglingStrategy

      -- | Name mangling strategy for names in the 'NsConstr' namespace
    , nameManglingStrategyConstr :: NameManglingStrategy

      -- | Name mangling strategy for names in the 'NsTypeVar' namespace
    , nameManglingStrategyTypeVar :: NameManglingStrategy

      -- | Name mangling strategy for names in the 'NsTypeConstr' namespace
    , nameManglingStrategyTypeConstr :: NameManglingStrategy

      -- | Name mangling strategy for names in the 'NsTypeClass' namespace
    , nameManglingStrategyTypeClass :: NameManglingStrategy

      -- | Name mangling strategy for names in the 'NsModuleName' namespace
    , nameManglingStrategyModuleName :: NameManglingStrategy

      -- | Invalid character action
    , nameManglingInvalidCharAction :: InvalidCharAction

      -- | Prefix for types
    , nameManglingTypePrefix :: Maybe Text

      -- | Prefix for constructors
    , nameManglingConstrPrefix :: Maybe Text

      -- | Prefix for record fields
      --
      -- This prefix is put /before/ automatic prefixes.
    , nameManglingFieldPrefix :: Maybe Text

      -- | Context prefix for record fields
    , nameManglingFieldContextPrefix :: FieldContextPrefix
    }
  deriving stock (Show)

-- | Name mangling strategy
data NameManglingStrategy =
    -- | Transform C names to names following Haskell conventions
    HaskellNameStrategy
  | -- | Minimize transformation of C names, use @camelCase@ to join parts
    MaintainCamelCaseNameStrategy
  | -- | Minimize transformation of C names, concatenating parts
    MaintainConcatNameStrategy
  | -- | Minimize transformation of C names, use @snake_case@ to join parts
    MaintainSnakeCaseNameStrategy
  deriving stock (Eq, Show)

-- | Record field context prefix options
data FieldContextPrefix =
    -- | Do not prefix record fields with context information
    FieldContextPrefixNone
  | -- | Prefix record fields with the record type name
    FieldContextPrefixType
  | -- | Prefix record fields with the constructor name
    FieldContextPrefixConstr
  | -- | Prefix record fields with the record type name when the record has a
    -- single constructor, or prefix with the constructor name otherwise
    FieldContextPrefixAuto
  deriving stock (Eq, Show)

-- | Invalid character action
data InvalidCharAction =
    -- | Drop invalid characters
    DropInvalidChars
  | -- | Escape invalid characters
    --
    -- An invalid character is escaped as @'@ followed by the Unicode code
    -- point (four-digit lowercase hex).
    EscapeInvalidChars
  deriving stock (Eq, Show)

-- | Default name mangling options
defaultNameManglingOptions :: NameManglingOptions
defaultNameManglingOptions = NameManglingOptions {
      nameManglingStrategyVar        = HaskellNameStrategy
    , nameManglingStrategyConstr     = HaskellNameStrategy
    , nameManglingStrategyTypeVar    = HaskellNameStrategy
    , nameManglingStrategyTypeConstr = HaskellNameStrategy
    , nameManglingStrategyTypeClass  = HaskellNameStrategy
    , nameManglingStrategyModuleName = HaskellNameStrategy
    , nameManglingInvalidCharAction  = DropInvalidChars
    , nameManglingTypePrefix         = Nothing
    , nameManglingConstrPrefix       = Just "Mk"
    , nameManglingFieldPrefix        = Nothing
    , nameManglingFieldContextPrefix = FieldContextPrefixAuto
    }

-- | Name mangling options the provide the lowest chance of collision
safestNameManglingOptions :: NameManglingOptions
safestNameManglingOptions = NameManglingOptions {
      nameManglingStrategyVar        = MaintainSnakeCaseNameStrategy
    , nameManglingStrategyConstr     = MaintainConcatNameStrategy
    , nameManglingStrategyTypeVar    = MaintainSnakeCaseNameStrategy
    , nameManglingStrategyTypeConstr = MaintainConcatNameStrategy
    , nameManglingStrategyTypeClass  = HaskellNameStrategy
    , nameManglingStrategyModuleName = HaskellNameStrategy
    , nameManglingInvalidCharAction  = EscapeInvalidChars
    , nameManglingTypePrefix         = Just "C"
    , nameManglingConstrPrefix       = Just "Mk"
    , nameManglingFieldPrefix        = Nothing
    , nameManglingFieldContextPrefix = FieldContextPrefixAuto
    }

{-------------------------------------------------------------------------------
  Contexts
-------------------------------------------------------------------------------}

-- | Local context for determining Haskell names in the 'NsVar' namespace
data NsVarContext =
    -- | No context provided
    EmptyNsVarContext
  | -- | Context for record fields (accessors)
    FieldContext {
      -- | Name of the record type
      ctxFieldTypeName :: HsName NsTypeConstr
      -- | Name of the constructor
    , ctxFieldConstrName :: HsName NsConstr
      -- | Record type has a single constructor?
    , ctxFieldConstrSingle :: Bool
    }
  deriving (Eq, Show)

-- | Local context for determining Haskell names in the 'NsConstr' namespace
newtype NsConstrContext = NsConstrContext {
      -- | Name of the type that the constructor is for
      ctxConstrTypeName :: HsName NsTypeConstr
    }
  deriving (Eq, Show)

-- | Local context for determining Haskell names in the 'NsTypeVar' namespace
newtype NsTypeVarContext = NsTypeVarContext {
      -- | Name of the type that the type variable is for
      ctxTypeVarTypeName :: HsName NsTypeConstr
    }
  deriving (Eq, Show)

-- | Local context for determining Haskell names in the 'NsTypeConstr' namespace
data NsTypeConstrContext =
    -- | No context provided
    EmptyNsTypeConstrContext
  deriving (Eq, Show)

-- | Local context for determining Haskell names in the 'NsTypeClass' namespace
data NsTypeClassContext =
    -- | No context provided
    EmptyNsTypeClassContext
  deriving (Eq, Show)

-- | Local context for determining Haskell names in the 'NsModuleName' namespace
newtype NsModuleNameContext = NsModuleNameContext {
      -- | Name of the parent module
      ctxModuleParentName :: Maybe (HsName NsModuleName)
    }
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | Name mangling
class ToHsName (ns :: Namespace) where
  type ToHsNameContext ns :: Type

  toHsName :: NameManglingOptions -> ToHsNameContext ns -> CName -> HsName ns

instance ToHsName NsVar where
  type ToHsNameContext NsVar = NsVarContext

  toHsName NameManglingOptions{..} =
      toHsName'
        nameManglingStrategyVar
        nameManglingInvalidCharAction
        Char.toLower
    . \case
        EmptyNsVarContext -> []
        FieldContext{..} -> catMaybes
          [ nameManglingFieldPrefix
          , case nameManglingFieldContextPrefix of
              FieldContextPrefixNone   -> Nothing
              FieldContextPrefixType   -> Just $ getHsName ctxFieldTypeName
              FieldContextPrefixConstr -> Just $ getHsName ctxFieldConstrName
              FieldContextPrefixAuto
                | ctxFieldConstrSingle -> Just $ getHsName ctxFieldTypeName
                | otherwise            -> Just $ getHsName ctxFieldConstrName
          ]

instance ToHsName NsConstr where
  type ToHsNameContext NsConstr = NsConstrContext

  toHsName NameManglingOptions{..} _ctx =
      toHsName'
        nameManglingStrategyConstr
        nameManglingInvalidCharAction
        Char.toUpper
    $ catMaybes
        [ nameManglingConstrPrefix
        , nameManglingTypePrefix
        ]

instance ToHsName NsTypeVar where
  type ToHsNameContext NsTypeVar = NsTypeVarContext

  toHsName NameManglingOptions{..} _ctx =
    toHsName'
      nameManglingStrategyTypeVar
      nameManglingInvalidCharAction
      Char.toLower
      []

instance ToHsName NsTypeConstr where
  type ToHsNameContext NsTypeConstr = NsTypeConstrContext

  toHsName NameManglingOptions{..} _ctx =
      toHsName'
        nameManglingStrategyTypeConstr
        nameManglingInvalidCharAction
        Char.toUpper
    $ maybeToList nameManglingTypePrefix

instance ToHsName NsTypeClass where
  type ToHsNameContext NsTypeClass = NsTypeClassContext

  toHsName NameManglingOptions{..} _ctx =
    toHsName'
      nameManglingStrategyTypeClass
      nameManglingInvalidCharAction
      Char.toUpper
      []

instance ToHsName NsModuleName where
  type ToHsNameContext NsModuleName = NsModuleNameContext

  toHsName NameManglingOptions{..} NsModuleNameContext{..} cName =
    let name =
          toHsName'
            nameManglingStrategyModuleName
            nameManglingInvalidCharAction
            Char.toUpper
            []
            cName
    in  case ctxModuleParentName of
          Just parentName ->
            HsName $ getHsName parentName <> "." <> getHsName name
          Nothing -> name

toHsName' ::
     NameManglingStrategy
  -> InvalidCharAction
  -> (Char -> Char)  -- ^ case conversion for first character
  -> [Text]          -- ^ name prefixes
  -> CName
  -> HsName ns
toHsName' strategy invCharAct f prefixes =
      HsName
    . handleKeywords
    . fixNameFirstChar f
    . joinNamePrefixes strategy prefixes
    . transformName strategy invCharAct
    . getCName

transformName :: NameManglingStrategy -> InvalidCharAction -> Text -> Text
transformName strategy invCharAct = case strategy of
    HaskellNameStrategy -> T.pack . goHaskell False . T.unpack
    _otherwise          -> T.pack . goMaintain      . T.unpack
  where
    isValidChar:: Char -> Bool
    isValidChar c =
      -- C names do not contain @'@, which we use as a control character when
      -- escaping invalid characters, so escape a @'@ if we unexpectingly run
      -- across one
      Char.isAlphaNum c || c == '_'

    isEscape :: Bool
    isEscape = invCharAct == EscapeInvalidChars

    goHaskell :: Bool -> String -> String
    goHaskell isUp (c:cs)
      | c == '_' = goHaskell True cs
      | isValidChar c =
          (if isUp then Char.toUpper c else c) : goHaskell False cs
      | isEscape = escapeChar c ++ goHaskell False cs
      | otherwise = goHaskell isUp cs
    goHaskell _isUp [] = []

    goMaintain :: String -> String
    goMaintain (c:cs)
      | isValidChar c = c : goMaintain cs
      | isEscape      = escapeChar c ++ goMaintain cs
      | otherwise     = goMaintain cs
    goMaintain []     = []

    escapeChar :: Char -> String
    escapeChar c =
      let hex = showHex (Char.ord c) ""
      in  '\'' : replicate (max 0 (4 - length hex)) '0' ++ hex

joinNamePrefixes :: NameManglingStrategy -> [Text] -> Text -> Text
joinNamePrefixes _strategy [] t = t
joinNamePrefixes strategy prefixes t =
    let joinParts = case strategy of
          HaskellNameStrategy           -> T.concat . map upperFirstChar
          MaintainCamelCaseNameStrategy -> T.concat . map upperFirstChar
          MaintainConcatNameStrategy    -> T.concat
          MaintainSnakeCaseNameStrategy -> T.intercalate "_"
    in  joinParts $ prefixes ++ [t]

upperFirstChar :: Text -> Text
upperFirstChar t = case T.uncons t of
    Just (c, t')
      | Char.isLetter c -> T.cons (Char.toUpper c) t'
    _otherwise          -> t

fixNameFirstChar :: (Char -> Char) -> Text -> Text
fixNameFirstChar f t = case T.uncons t of
    Just (c, t')
      | Char.isLetter c -> T.cons (f c) t'
      | otherwise       -> fixNameFirstChar f t'
    Nothing             -> t

-- <https://wiki.haskell.org/Keywords>
handleKeywords :: Text -> Text
handleKeywords t
    | t `elem` keywords = t <> "'"
    | otherwise         = t
  where
    keywords :: [Text]
    keywords =
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
      , "proc"
      , "qualified"
      , "rec"
      , "then"
      , "type"
      , "where"
      ]
