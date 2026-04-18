module HsBindgen.Config.MangleCandidate (
    -- * Definition
    MangleCandidate(..)
  , mangleCandidate
    -- * Parse
  , ParseCandidateError(..)
  , parseCandidate
    -- * Standard instances
  , mangleCandidateDefault
  , mangleCandidateHaskell
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
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Config.MangleCandidate.ReservedNames (allReservedNames)
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

import Numeric (showHex)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Mangle candidate name to conform to Haskell's naming rules
--
-- Name generation is allowed to fail (depending on the policy, there are
-- circumstances in which we cannot generate a name). When this happens, we
-- require a name override.
data MangleCandidate m = MangleCandidate {
      -- | Process invalid characters
      --
      -- Called on characters that are invalid anywhere in a Haskell identifier.
      --
      -- See 'dropInvalidChar' and 'escapeInvalidChar'.
      onInvalidChar :: Char -> m String

      -- | Make the name conform to the name rule set
      --
      --See 'modifyFirstLetter'
    , apply :: ApplyRuleset m

      -- | Reserved names
    , reservedNames :: Set Text

      -- | How to modify reserved names
      --
      -- The transformation function must return a valid Haskell name.
      --
      -- See 'appendSingleQuote'.
    , onReservedName :: Text -> Text
    }

newtype ApplyRuleset m = ApplyRuleset(
      forall ns. Hs.SingNamespace ns => Text -> m (Hs.Name ns)
    )

applyRuleset ::
     Hs.SingNamespace ns
  => ApplyRuleset m -> Text -> m (Hs.Name ns)
applyRuleset (ApplyRuleset f) = f

deriving stock instance Generic (MangleCandidate m)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

mangleCandidateDefault :: MangleCandidate Maybe
mangleCandidateDefault = MangleCandidate {
      onInvalidChar  = return . escapeInvalidChar
    , apply          = ApplyRuleset $
                         modifyFirstLetter (Just . prefixInvalidFirst "C" "c")
    , reservedNames  = allReservedNames
    , onReservedName = appendSingleQuote
    }

mangleCandidateHaskell :: MangleCandidate Maybe
mangleCandidateHaskell = mangleCandidateDefault {
      onInvalidChar = return . dropInvalidChar
    , apply         = ApplyRuleset $ modifyFirstLetter dropInvalidFirst
    }

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

-- | Translate C name to Haskell name, observing Haskell naming conventions
--
-- If the C name is already a valid Haskell name for its namespace, it must be
-- preserved, provided it is not a reserved name (idempotent modulo reserved
-- names).
mangleCandidate :: forall ns m.
     (Monad m, Hs.SingNamespace ns)
  => MangleCandidate m -> Text -> m (Hs.Name ns)
mangleCandidate mc =
        pure . handleReservedNames
    <=< applyRuleset mc.apply
    <=< processInvalidChars
  where
    processInvalidChars :: Text -> m Text
    processInvalidChars = fmap Text.pack . go [] . Text.unpack
      where
        go :: [String] -> String -> m String
        go acc (c:cs)
          | isValidChar c = go ([c] : acc) cs
          | otherwise     = mc.onInvalidChar c >>= \c' -> go (c':acc) cs
        go acc []         = return $ concat (reverse acc)

        -- NOTE: @isAlphaNum@ is @True@ for non-ASCII characters too (e.g. '你')
        -- NOTE: C names cannot include single quotes, but they can arise when
        -- we define one class of name (say, a data constructor) from the name
        -- that we generate for another (say, the data type).
        isValidChar :: Char -> Bool
        isValidChar c = Char.isAlphaNum c || c == '_' || c == '\''

    handleReservedNames :: Hs.Name ns -> Hs.Name ns
    handleReservedNames name
      | name.text `Set.member` mc.reservedNames
      = Hs.UnsafeName $ mc.onReservedName name.text
      | otherwise
      = name

{-------------------------------------------------------------------------------
  Parse
-------------------------------------------------------------------------------}

data ParseCandidateError =
  ParseNameMismatch Text Hs.SomeName
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace ParseCandidateError where
  prettyForTrace = \case
    ParseNameMismatch candidate name -> PP.hcat [
        "Mismatch of candidate ("
      , PP.text candidate
      , ") and name ("
      , PP.text name.text
      , "; namespace "
      , prettyForTrace name.ns
      , ")"
      ]

-- | Parse a candidate and check if it adheres to the naming rules.
parseCandidate :: forall ns m.
     (Monad m, Hs.SingNamespace ns)
  => MangleCandidate m
  -> Text
  -> m (Either ParseCandidateError (Hs.Name ns))
parseCandidate mc candidate = do
    -- 'mangleCandidate' is idempotent, so applying the transformation again
    -- must yield the same identifier.
    name <- mangleCandidate @ns mc candidate
    pure $
      if candidate == name.text; then
        Right name
      else
        Left $ ParseNameMismatch candidate (Hs.demoteNs name)

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
      ruleset :: SNameRuleSet (NamespaceRuleSet ns)

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
               Just . Hs.UnsafeName $
                 Text.cons firstChar rest
           | matchesRule (adjustForRule firstChar) ->
               Just . Hs.UnsafeName $
                 Text.cons (adjustForRule firstChar) rest
           | otherwise -> do
               let unhandled, afterUnhandled :: Text
                   (unhandled, afterUnhandled) = Text.span unusable t

                   usable :: Maybe (Char, Char, Text)
                   usable =
                       (\(c, cs) -> if matchesRule c
                                      then (c, c, cs)
                                      else (c, adjustForRule c, cs)
                       ) <$>
                          Text.uncons afterUnhandled

                   cannotApply :: CannotApplyRuleset ns
                   cannotApply = CannotApplyRuleset{
                         ruleset         = ruleset
                       , unhandledPrefix = unhandled
                       , usableSuffix    = usable
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
         prefix = case cannotApply.ruleset of
               SNameRuleSetOther -> prefixOther
               SNameRuleSetVar   -> prefixVar
     in Hs.UnsafeName $ mconcat [
            prefix
          , cannotApply.unhandledPrefix
          , aux cannotApply.usableSuffix
          ]
  where
    aux :: Maybe (Char, Char, Text) -> Text
    aux Nothing           = ""
    aux (Just (_, c, cs)) = Text.cons c cs

dropInvalidFirst :: CannotApplyRuleset ns -> Maybe (Hs.Name ns)
dropInvalidFirst cannotApply =
    aux <$> cannotApply.usableSuffix
  where
    aux :: (Char, Char, Text) -> Hs.Name ns
    aux (_, c, cs) = Hs.UnsafeName $ Text.cons c cs

{-------------------------------------------------------------------------------
  Reserved names
-------------------------------------------------------------------------------}

-- | Append a single quote (@'@) to a name
appendSingleQuote :: Text -> Text
appendSingleQuote = (<> "'")
