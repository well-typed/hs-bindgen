module HsBindgen.Hs.NameMangler.DSL.FixCandidate (
    -- * Definition
    FixCandidate(..)
  , fixCandidate
    -- * Standard instances
  , fixCandidateDefault
  , fixCandidateHaskell
    -- * Constructing new instances
    -- ** Dealing with invalid characters
  , dropInvalidChar
  , escapeInvalidChar
    -- ** Name rule sets
  , NameRuleSet(..)
  , NamespaceRuleSet
  , SNameRuleSet(..)
  , singNameRuleSet
  , modifyFirstLetter
  , prefixInvalidFirst
  , dropInvalidFirst
    -- ** Reserved names
  , appendSingleQuote
  ) where

import Control.Monad
import Data.Char qualified as Char
import Data.Proxy
import Data.Set qualified as Set
import Data.Text qualified as Text
import Numeric (showHex)

import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.NameMangler.DSL.ReservedNames (allReservedNames)
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Fix candidate name to conform to Haskell's naming rules
--
-- Name generation is allowed to fail (depending on the policy, there are
-- circumstances in which we cannot generate a name). When this happens, we
-- require a name override.
data FixCandidate m = FixCandidate {
      -- | Process invalid characters
      --
      -- Called on characters that are invalid anywhere in a Haskell identifier.
      --
      -- See 'dropInvalidChar' and 'escapeInvalidChar'.
      onInvalidChar :: Char -> m String

      -- | Make the name conform to the name rule set
      --
      --See 'modifyFirstLetter'
    , applyRuleSet :: forall ns. SingNamespace ns => Text -> m (HsName ns)

      -- | Reserved names
    , reservedNames :: Set Text

      -- | How to modify reserved names
      --
      -- The transformation function must return a valid Haskell name.
      --
      -- See 'appendSingleQuote'.
    , onReservedName :: Text -> Text
    }

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

fixCandidateDefault :: FixCandidate Maybe
fixCandidateDefault = FixCandidate {
      onInvalidChar  = return . escapeInvalidChar
    , applyRuleSet   = modifyFirstLetter (prefixInvalidFirst "C")
    , reservedNames  = allReservedNames
    , onReservedName = appendSingleQuote
    }

fixCandidateHaskell :: FixCandidate Maybe
fixCandidateHaskell = fixCandidateDefault {
      onInvalidChar = return . dropInvalidChar
    , applyRuleSet  = modifyFirstLetter dropInvalidFirst
    }

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

fixCandidate :: forall ns m.
     (Monad m, SingNamespace ns)
  => FixCandidate m -> Text -> m (HsName ns)
fixCandidate FixCandidate{
                onInvalidChar
              , applyRuleSet
              , reservedNames
              , onReservedName
              } =
        handleReservedNames
    <=< applyRuleSet
    <=< processInvalidChars
  where
    processInvalidChars :: Text -> m Text
    processInvalidChars = fmap Text.pack . go [] . Text.unpack
      where
        go :: [String] -> String -> m String
        go acc (c:cs)
          | isValidChar c = go ([c] : acc) cs
          | otherwise     = onInvalidChar c >>= \c' -> go (c':acc) cs
        go acc []         = return $ concat (reverse acc)

        -- NOTE: @isAlphaNum@ is @True@ for non-ASCII characters too (e.g. '你')
        -- NOTE: C names cannot include single quotes, but they can arise when
        -- we define one class of name (say, a data constructor) from the name
        -- that we generate for another (say, the data type).
        isValidChar :: Char -> Bool
        isValidChar c = Char.isAlphaNum c || c == '_' || c == '\''

    handleReservedNames :: HsName ns -> m (HsName ns)
    handleReservedNames name@(HsName t) = return $
        if t `Set.member` reservedNames
          then HsName $ onReservedName t
          else name

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
      SNameRuleSetVar   -> changeCase Char.toLower t
      SNameRuleSetOther ->
        let (nonletters, rest) = Text.break Char.isLetter t in
        if Text.null nonletters
          then changeCase Char.toUpper rest
          else onInvalidFirst nonletters rest
  where
    changeCase :: (Char -> Char) -> Text -> Maybe Text
    changeCase f t = do
        (c, t') <- Text.uncons t
        return $ Text.cons (f c) t'

prefixInvalidFirst :: Text -> Text -> Text -> Maybe Text
prefixInvalidFirst prefix nonletters rest =
    Just $ prefix <> nonletters <> rest

dropInvalidFirst :: Text -> Text -> Maybe Text
dropInvalidFirst _nonletters rest = do
    (c, t) <- Text.uncons rest
    Just $ Text.cons (Char.toUpper c) t

{-------------------------------------------------------------------------------
  Reserved names
-------------------------------------------------------------------------------}

-- | Append a single quote (@'@) to a name
appendSingleQuote :: Text -> Text
appendSingleQuote = (<> "'")
