{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Hs.AST.Name (
    -- * Definition
    HsName(..)
  , Namespace(..)
    -- * Options
  , NameManglingOptions(..)
  , NameManglingStrategy(..)
  , defaultNameManglingOptions
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
      -- | Name mangling strategy
      nameManglingStrategy     :: NameManglingStrategy

      -- | Prefix for types
    , nameManglingTypePrefix   :: Maybe Text

      -- | Prefix for constructors
    , nameManglingConstrPrefix :: Maybe Text

      -- | Prefix fields with constructor name?
    , nameManglingPrefixFields :: Bool

      -- | Prefix for fields
      --
      -- This prefix is put /before/ the constructor name when
      -- 'nameManglingPrefixFields' is set.
    , nameManglingFieldPrefix  :: Maybe Text
    }
  deriving stock (Show)

-- | Name mangling strategy
data NameManglingStrategy =
    -- | Transform C names to names following Haskell conventions
    HaskellNameStrategy
  | -- | Minimize transformation of C names, use @camelCase@ to join parts
    MaintainCamelCaseNameStrategy
  | -- | Minimize transformation of C names, use @snake_case@ to join parts
    MaintainSnakeCaseNameStrategy
  deriving stock (Eq, Show)

-- | Default name mangling options
--
-- * Strategy: 'HaskellNameStrategy'
-- * Type prefix: @Just "C"@
-- * Constructor prefix: @Just "Mk"@
-- * Prefix fields with constructor name: @True@
-- * Field prefix: @Nothing@
defaultNameManglingOptions :: NameManglingOptions
defaultNameManglingOptions = NameManglingOptions {
      nameManglingStrategy     = HaskellNameStrategy
    , nameManglingTypePrefix   = Just "C"
    , nameManglingConstrPrefix = Just "Mk"
    , nameManglingPrefixFields = True
    , nameManglingFieldPrefix  = Nothing
    }

{-------------------------------------------------------------------------------
  Contexts
-------------------------------------------------------------------------------}

-- | Local context for determining Haskell names in the 'NsVar' namespace
data NsVarContext =
    -- | No context provided
    EmptyNsVarContext
  | -- | Context for record fields (accessors)
    RecordFieldContext {
      -- | Name of the constructor that the field is for
      ctxRecordFieldConstrName :: HsName NsConstr
    }
  deriving (Eq, Show)

-- | Local context for determining Haskell names in the 'NsConstr' namespace
newtype NsConstrContext = NsConstrContext {
      -- | Name of type type that the constructor is for
      ctxConstrTypeName :: HsName NsTypeConstr
    }
  deriving (Eq, Show)

-- | Local context for determining Haskell names in the 'NsTypeVar' namespace
data NsTypeVarContext =
    -- | No context provided
    EmptyNsTypeVarContext
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
    toHsName' nameManglingStrategy Char.toLower . \case
      EmptyNsVarContext -> []
      RecordFieldContext{..} -> catMaybes
        [ nameManglingFieldPrefix
        , if nameManglingPrefixFields
            then Just $ getHsName ctxRecordFieldConstrName
            else Nothing
        ]

instance ToHsName NsConstr where
  type ToHsNameContext NsConstr = NsConstrContext

  toHsName NameManglingOptions{..} _ctx =
    toHsName' HaskellNameStrategy Char.toUpper $ catMaybes
      [ nameManglingConstrPrefix
      , nameManglingTypePrefix
      ]

instance ToHsName NsTypeVar where
  type ToHsNameContext NsTypeVar = NsTypeVarContext

  toHsName NameManglingOptions{..} _ctx =
    toHsName' nameManglingStrategy Char.toLower []

instance ToHsName NsTypeConstr where
  type ToHsNameContext NsTypeConstr = NsTypeConstrContext

  toHsName NameManglingOptions{..} _ctx =
    toHsName' HaskellNameStrategy Char.toUpper $
      maybeToList nameManglingTypePrefix

instance ToHsName NsTypeClass where
  type ToHsNameContext NsTypeClass = NsTypeClassContext

  toHsName _opts _ctx = toHsName' HaskellNameStrategy Char.toUpper []

instance ToHsName NsModuleName where
  type ToHsNameContext NsModuleName = NsModuleNameContext

  toHsName _opts NsModuleNameContext{..} cName =
    let moduleName = toHsName' HaskellNameStrategy Char.toUpper [] cName
    in  case ctxModuleParentName of
          Just parentName ->
            HsName $ getHsName parentName <> "." <> getHsName moduleName
          Nothing -> moduleName

toHsName' ::
     NameManglingStrategy
  -> (Char -> Char)  -- ^ case conversion for first character
  -> [Text]          -- ^ name prefixes
  -> CName
  -> HsName ns
toHsName' strategy f prefixes =
      HsName
    . handleKeywords
    . fixNameFirstChar f
    . joinNamePrefixes strategy prefixes
    . transformName strategy
    . getCName

transformName :: NameManglingStrategy -> Text -> Text
transformName strategy = case strategy of
    HaskellNameStrategy -> T.pack . xform False . T.unpack
    _otherwise -> T.filter isValidChar
  where
    xform :: Bool -> String -> String
    xform isNext (c:cs)
      | c == '_'      = xform True cs
      | isValidChar c = (if isNext then Char.toUpper c else c) : xform False cs
      | otherwise     = xform False cs
    xform _isNext []  = []

    isValidChar:: Char -> Bool
    isValidChar c = Char.isAlphaNum c || c `elem` ['\'', '_']

joinNamePrefixes :: NameManglingStrategy -> [Text] -> Text -> Text
joinNamePrefixes _strategy [] t = t
joinNamePrefixes strategy prefixes t = case strategy of
    MaintainSnakeCaseNameStrategy -> T.intercalate "_" (prefixes ++ [t])
    _otherwise -> mconcat $ map upperFirstChar (prefixes ++ [t])

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
