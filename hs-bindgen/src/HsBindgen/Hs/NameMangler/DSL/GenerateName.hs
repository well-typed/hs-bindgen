-- | Basic policy for generating C names from Haskell names
module HsBindgen.Hs.NameMangler.DSL.GenerateName (
    GenerateName(..)
  , defaultGenerateName
    -- * Process C names
  , camelCaseCName
    -- * Dealing with invalid characters
  , dropInvalidChar
  , escapeInvalidChar
    -- * Joining names
  , joinNamesSnakeCase
  , joinNamesCamelCase
    -- * Name rule sets
  , NameRuleSet(..)
  , NamespaceRuleSet
  , SNameRuleSet(..)
  , singNameRuleSet
    -- * Applying name rule sets
  , modifyFirstLetter
  , prefixInvalidFirst
  , dropInvalidFirst
    -- * Applying 'GenerateName'
  , generateName
  ) where

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Proxy
import Data.Text qualified as Text
import Numeric (showHex)

import HsBindgen.C.AST
import HsBindgen.Errors
import HsBindgen.Hs.AST.Name
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Generate Haskell name from C name
--
-- Name generation is allowed to fail (depending on the policy, there are
-- circumstances in which we cannot generate a name). When this happens, we
-- require a name override.
data GenerateName = GenerateName {
      -- | Process C names prior to further name generation
      processCName :: CName -> CName

      -- | Process invalid characters
      --
      -- Called on characters that are invalid anywhere in a Haskell identifier.
    , onInvalidChar :: Char -> String

      -- | Combine multiple C names
    , joinNames :: [Text] -> Text

      -- | Make the name conform to the name rule set
    , applyRuleSet :: forall ns. SingNamespace ns => Text -> Maybe (HsName ns)
    }

defaultGenerateName :: GenerateName
defaultGenerateName = GenerateName {
      processCName  = id
    , onInvalidChar = escapeInvalidChar
    , joinNames     = joinNamesSnakeCase
    , applyRuleSet  = modifyFirstLetter (prefixInvalidFirst "C")
    }

{-------------------------------------------------------------------------------
  Process C names
-------------------------------------------------------------------------------}

-- | Converting from @snake_case@ to @camelCase@
--
-- Leading and trailing underscores are assumed to have special meaning and
-- are preserved.  All other underscores are removed.  Letters following
-- (preserved or removed) underscores are changed to uppercase.
camelCaseCName :: CName -> CName
camelCaseCName =
    CName . Text.pack . start False . Text.unpack . getCName
  where
    start :: Bool -> String -> String
    start isUp = \case
      c:cs
        | c == '_'  -> c : start True cs
        | otherwise -> (if isUp then Char.toUpper c else c) : aux 0 cs
      []            -> []

    aux :: Int -> String -> String
    aux !numUs = \case
      c:cs
        | c == '_'  -> aux (numUs + 1) cs
        | otherwise -> (if numUs > 0 then Char.toUpper c else c) : aux 0 cs
      []            -> List.replicate numUs '_'

{-------------------------------------------------------------------------------
  Dealing with invalid characters
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Joining names
-------------------------------------------------------------------------------}

-- | Join parts of a name with underscores (@_@)
joinNamesSnakeCase :: [Text] -> Text
joinNamesSnakeCase = Text.intercalate "_"

-- | Join parts of a name in @camelCase@ style
--
-- The first character of all parts but the first is changed to uppercase (if it
-- is a letter), and the results are concatenated.
--
-- Since this function may change the case of letters, it can cause name
-- collisions when different C names only differ by case of the first letter.
joinNamesCamelCase :: [Text] -> Text
joinNamesCamelCase = \case
    (t:ts) -> Text.concat $ t : map upperFirstChar ts
    []     -> Text.empty
  where
    upperFirstChar :: Text -> Text
    upperFirstChar t = case Text.uncons t of
      Just (c, t') -> Text.cons (Char.toUpper c) t'
      Nothing      -> t

{-------------------------------------------------------------------------------
  Name rule sets
-------------------------------------------------------------------------------}

data NameRuleSet =
    -- | Variables and type variables
    NameRuleSetVar

    -- | Constructors, type constructors, type classes and module names
  | NameRuleSetOther

type family NamespaceRuleSet (ns :: Namespace) :: NameRuleSet where
  NamespaceRuleSet NsTypeConstr = NameRuleSetOther
  NamespaceRuleSet NsConstr     = NameRuleSetOther
  NamespaceRuleSet NsVar        = NameRuleSetVar

data SNameRuleSet :: NameRuleSet -> Star where
  SNameRuleSetVar   :: SNameRuleSet NameRuleSetVar
  SNameRuleSetOther :: SNameRuleSet NameRuleSetOther

singNameRuleSet :: forall ns.
     SingNamespace ns
  => Proxy ns -> SNameRuleSet (NamespaceRuleSet ns)
singNameRuleSet _ =
    case singNamespace @ns of
      SNsTypeConstr -> SNameRuleSetOther
      SNsConstr     -> SNameRuleSetOther
      SNsVar        -> SNameRuleSetVar

{-------------------------------------------------------------------------------
  Applying name rule sets
-------------------------------------------------------------------------------}

-- | Make first letter conform the name rule set
--
-- Assumes invalid characters are processed.
--
-- Haskell identifiers have different rules for the first character and the rest
-- of the identifier. Since we started with a valid C identifier, and we have
-- processed all characters that are "invalid everywhere" (see 'onInvalidChar'),
-- the identifier must start with a letter or an underscore:
--
-- * If we are generating a 'NameRuleSetVar' name, the underscore is fine
--   as-is; any letters can be made uppercase.
-- * If we are generating a 'NameRuleSetOther', and the identifier starts
--   with an underscore, we strip off everything that's not a letter and
--   call @onInvalidFirst@ on this prefix and the remaining suffix.
modifyFirstLetter :: forall ns.
     SingNamespace ns
  => (Text -> Text -> Maybe Text)
     -- ^ @onInvalidFirst@ (see 'prefixInvalidFirst' and 'dropInvalidFirst')
  -> Text -> Maybe (HsName ns)
modifyFirstLetter onInvalidFirst = \t -> fmap HsName $
    case singNameRuleSet (Proxy @ns) of
      SNameRuleSetVar   -> Just $ changeCase Char.toLower t
      SNameRuleSetOther ->
        let (nonletters, rest) = Text.break Char.isLetter t in
        if Text.null nonletters
          then Just $ changeCase Char.toUpper rest
          else onInvalidFirst nonletters rest
  where
    changeCase :: (Char -> Char) -> Text -> Text
    changeCase f t =
        case Text.uncons t of
          Just (c, t') -> Text.cons (f c) t'
          Nothing      -> panicEmptyName

prefixInvalidFirst :: Text -> Text -> Text -> Maybe Text
prefixInvalidFirst prefix nonletters rest =
    Just $ prefix <> nonletters <> rest

dropInvalidFirst :: Text -> Text -> Maybe Text
dropInvalidFirst _nonletters rest = do
    (c, t) <- Text.uncons rest
    Just $ Text.cons (Char.toUpper c) t

{-------------------------------------------------------------------------------
  Applying 'GenerateName'
-------------------------------------------------------------------------------}

generateName ::
     SingNamespace ns
  => GenerateName -> [CName] -> Maybe (HsName ns)
generateName GenerateName{
                processCName
              , onInvalidChar
              , joinNames
              , applyRuleSet
              } =
      applyRuleSet
    . joinNames
    . map (processInvalidChars . getCName . processCName)
  where
    processInvalidChars :: Text -> Text
    processInvalidChars = Text.pack . go . Text.unpack
      where
        go :: String -> String
        go (c:cs)
          | isValidChar c = c : go cs
          | otherwise     = onInvalidChar c ++ go cs
        go []            = ""

        isValidChar :: Char -> Bool
        isValidChar c = Char.isAlphaNum c || c == '_'

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

panicEmptyName :: a
panicEmptyName = panicPure "mkHsNameDropInvalid: empty name"

