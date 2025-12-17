module HsBindgen.Config.FixCandidate (
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

import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Config.FixCandidate.ReservedNames (allReservedNames)
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

import Numeric (showHex)

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
    , applyRuleSet :: forall ns. Hs.SingNamespace ns => Text -> m (Hs.Name ns)

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
    , applyRuleSet   = modifyFirstLetter (Just . prefixInvalidFirst "C" "c")
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
     (Monad m, Hs.SingNamespace ns)
  => FixCandidate m -> Text -> m (Hs.Name ns)
fixCandidate FixCandidate{
                onInvalidChar
              , applyRuleSet
              , reservedNames
              , onReservedName
              } =
        pure . handleReservedNames
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

    handleReservedNames :: Hs.Name ns -> Hs.Name ns
    handleReservedNames = \case
      Hs.ExposedName name | name `Set.member` reservedNames ->
        Hs.ExposedName $ onReservedName name
      otherName -> otherName

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

type family NamespaceRuleSet (ns :: Hs.Namespace) :: NameRuleSet where
  NamespaceRuleSet Hs.NsTypeConstr = NameRuleSetOther
  NamespaceRuleSet Hs.NsConstr     = NameRuleSetOther
  NamespaceRuleSet Hs.NsVar        = NameRuleSetVar

data SNameRuleSet :: NameRuleSet -> Star where
  SNameRuleSetVar   :: SNameRuleSet NameRuleSetVar
  SNameRuleSetOther :: SNameRuleSet NameRuleSetOther

singNameRuleSet :: forall ns.
     Hs.SingNamespace ns
  => Proxy ns -> SNameRuleSet (NamespaceRuleSet ns)
singNameRuleSet _ =
    case Hs.singNamespace @ns of
      Hs.SNsTypeConstr -> SNameRuleSetOther
      Hs.SNsConstr     -> SNameRuleSetOther
      Hs.SNsVar        -> SNameRuleSetVar

{-------------------------------------------------------------------------------
  Applying name rule sets
-------------------------------------------------------------------------------}

data CannotApplyRuleset ns = CannotApplyRuleset{
      -- | The ruleset we failed to apply
      cannotApplyRuleset :: SNameRuleSet (NamespaceRuleSet ns)

      -- | The prefix we cannot handle
      --
      -- This contains characters for which 'toUpper' to 'toLower' is
      -- insufficient to enforce the ruleset.
    , unhandledPrefix :: Text

      -- | The remainder of the name
      --
      -- The rest of the name, after the unhandled prefix, if any; we single out
      -- the first character, both as it appears in the original name as well as
      -- (potentially) modified to adhere to the ruleset (or unmodified if the
      -- remainder happens to already adhere to the rule).
    , usableSuffix :: Maybe (Char, Char, Text)
    }

-- | Make first letter conform the name rule set
--
-- Haskell identifiers have different rules for the first character and the rest
-- of the identifier. In particular, names in 'NameRuleSetOther' must start with
-- an uppercase letter, whereas names in 'NameRuleSetVar' must start with a
-- not-uppercase letter. This is not the same as a lowercase letter!
--
-- > isAlphaNum '事' == True
-- > isUpper    '事' == False
-- > isLower    '事' == False
--
-- To ensure that this rule is satisfied, we
--
-- 1. First check if the rule is already satisfied; if so, nothing to do.
--
-- 2. If the rule is not satisfied, we attempt to change the first character
--    to uppercase or lowercase, as appropriate. If this succeeds, we are done.
--
-- 3. Step (2) may fail; for example;
--
--    > toUpper '_' == '_'
--    > toUpper '事' == '事'
--
--    When this happens, we call the supplied function with the unusable prefix
--    and the remainder of the identifier.
modifyFirstLetter :: forall ns.
     Hs.SingNamespace ns
  => (CannotApplyRuleset ns -> Maybe (Hs.Name ns))
     -- ^ @onInvalidFirst@ (see 'prefixInvalidFirst' and 'dropInvalidFirst')
  -> Text -> Maybe (Hs.Name ns)
modifyFirstLetter onInvalidFirst =
    aux (singNameRuleSet (Proxy @ns))
  where
    aux :: SNameRuleSet (NamespaceRuleSet ns) -> Text -> Maybe (Hs.Name ns)
    aux ruleset = \t -> do
        (firstChar, rest) <- Text.uncons t
        if | matchesRule firstChar ->
               Just . Hs.ExposedName $ Text.cons firstChar rest
           | matchesRule (adjustForRule firstChar) ->
               Just . Hs.ExposedName $ Text.cons (adjustForRule firstChar) rest
           | otherwise -> do
               let unhandledPrefix, afterUnhandled :: Text
                   (unhandledPrefix, afterUnhandled) = Text.span unusable t

                   usableSuffix :: Maybe (Char, Char, Text)
                   usableSuffix =
                       (\(c, cs) -> if matchesRule c
                                      then (c, c, cs)
                                      else (c, adjustForRule c, cs)
                       ) <$>
                          Text.uncons afterUnhandled

                   cannotApply :: CannotApplyRuleset ns
                   cannotApply = CannotApplyRuleset{
                         cannotApplyRuleset = ruleset
                       , unhandledPrefix
                       , usableSuffix
                       }
               onInvalidFirst cannotApply
      where
        matchesRule   :: Char -> Bool
        adjustForRule :: Char -> Char
        (matchesRule, adjustForRule) =
            case ruleset of
              SNameRuleSetVar   -> (not . Char.isUpper , Char.toLower)
              SNameRuleSetOther -> (      Char.isUpper , Char.toUpper)

        unusable :: Char -> Bool
        unusable c =
               not (matchesRule                 $ c)
            && not (matchesRule . adjustForRule $ c)

prefixInvalidFirst ::
     Text  -- ^ Prefix for 'SNameRuleSetOther'
  -> Text  -- ^ Prefix for 'SNameRuleSetVar' (rarely needed)
  -> CannotApplyRuleset ns -> Hs.Name ns
prefixInvalidFirst prefixOther prefixVar cannotApply =
     let prefix :: Text
         prefix = case cannotApplyRuleset of
               SNameRuleSetOther -> prefixOther
               SNameRuleSetVar   -> prefixVar
     in Hs.ExposedName $ prefix <> unhandledPrefix <> aux usableSuffix
  where
    CannotApplyRuleset{
        cannotApplyRuleset
      , unhandledPrefix
      , usableSuffix} = cannotApply

    aux :: Maybe (Char, Char, Text) -> Text
    aux Nothing           = ""
    aux (Just (_, c, cs)) = Text.cons c cs

dropInvalidFirst :: CannotApplyRuleset ns -> Maybe (Hs.Name ns)
dropInvalidFirst CannotApplyRuleset{usableSuffix} =
    aux <$> usableSuffix
  where
    aux :: (Char, Char, Text) -> Hs.Name ns
    aux (_, c, cs) = Hs.ExposedName $ Text.cons c cs

{-------------------------------------------------------------------------------
  Reserved names
-------------------------------------------------------------------------------}

-- | Append a single quote (@'@) to a name
appendSingleQuote :: Text -> Text
appendSingleQuote = (<> "'")
