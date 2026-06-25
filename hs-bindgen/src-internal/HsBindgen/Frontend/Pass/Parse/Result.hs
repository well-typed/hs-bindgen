-- | Results from the parser proper
module HsBindgen.Frontend.Pass.Parse.Result (
    ParseResult(..)
  , ParseClassification(..)
  , ParseSuccess(..)
  , ParseNotAttempted(..)
    -- * Convenience constructors
  , parseSucceed
  , parseSucceedWith
  , parseDoNotAttempt
    -- * Query
  , getParseResultMaybeDecl
  , getParseResultEitherDecl
  ) where

import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types

import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parse result
--
-- NOTE: This does /NOT/ depend on the @Parse@ pass specifically: we transform
-- these results in the @AssignAnonIds@ pass.
type ParseResult :: Star -> Pass -> Star
data ParseResult l p = ParseResult{
      id             :: Id p
    , loc            :: SingleLoc
    , classification :: ParseClassification l p
    }
    deriving (Generic)

deriving stock instance ( IsPass p
                        , Macro.HasTypes l
                        ) => Show (ParseResult l p)

data ParseClassification l p =
    ParseResultSuccess      (ParseSuccess l p)
  | ParseResultNotAttempted ParseNotAttempted
  | ParseResultFailure      DelayedParseMsg
  deriving stock (Generic)

deriving stock instance ( IsPass p
                        , Macro.HasTypes l
                        ) => Show (ParseClassification l p)

instance PrettyForTrace (ParseClassification l p) where
  prettyForTrace = \case
    ParseResultSuccess      x -> prettyForTrace x
    ParseResultNotAttempted x -> prettyForTrace x
    ParseResultFailure msg    -> PP.hang "Parse failure:" 2 $
      prettyForTrace msg

data ParseSuccess l p = ParseSuccess {
      decl             :: C.Decl l p
    , delayedParseMsgs :: [DelayedParseMsg]
    }
  deriving stock (Generic)

deriving stock instance ( IsPass p
                        , Macro.HasTypes l
                        ) => Show (ParseSuccess l p)

-- | Why did we not attempt to parse a declaration?
data ParseNotAttempted =
    -- | We unexpectedly did not attempt to parse a declaration because it is
    -- reported "unavailable".
    DeclarationUnavailable
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance IsPass p => PrettyForTrace (ParseResult l p) where
  prettyForTrace result =
      prettyForTrace $ C.WithLocationInfo{
          loc = idLocationInfo (Proxy @p) result.id [result.loc]
        , msg = result.classification
        }

instance PrettyForTrace (ParseSuccess l p) where
  prettyForTrace success =
      if null success.delayedParseMsgs then
        "Parse success"
      else
        PP.hang "Parse success with messages:" 2 $
          PP.vcat $
          map prettyForTrace success.delayedParseMsgs

instance PrettyForTrace ParseNotAttempted where
  prettyForTrace = PP.hang "Parse not attempted: " 2 . \case
    DeclarationUnavailable   -> "Declaration is 'unavailable' on this platform"

{-------------------------------------------------------------------------------
  Convenience constructors
-------------------------------------------------------------------------------}

-- | Assemble a parse success
parseSucceed :: C.Decl l p -> ParseResult l p
parseSucceed = parseSucceedWith []

-- | Assemble a parse success with delayed parse messages
parseSucceedWith :: [DelayedParseMsg] -> C.Decl l p -> ParseResult l p
parseSucceedWith msgs decl = ParseResult{
     id             = decl.info.id
   , loc            = decl.info.loc
   , classification = ParseResultSuccess ParseSuccess{
         decl             = decl
       , delayedParseMsgs = msgs
       }
    }

-- | Assemble a "parse not attempted" with a reason
parseDoNotAttempt :: C.DeclInfo p -> ParseNotAttempted -> ParseResult l p
parseDoNotAttempt info reason = ParseResult{
      id              = info.id
    , loc             = info.loc
    , classification = ParseResultNotAttempted reason
    }

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

getParseResultMaybeDecl :: ParseResult l p -> Maybe (C.Decl l p)
getParseResultMaybeDecl result =
    case result.classification of
      ParseResultSuccess success -> Just $ success.decl
      _other                     -> Nothing

getParseResultEitherDecl :: ParseResult l p -> Either (ParseResult l p) (C.Decl l p)
getParseResultEitherDecl result =
    maybe (Left result) Right $ getParseResultMaybeDecl result

{-------------------------------------------------------------------------------
  CoercePass instances
-------------------------------------------------------------------------------}

instance (
      CoercePassId p p'
    , CoercePass (C.Decl l) p p'
    , Ann "TranslationUnit" p ~ Ann "TranslationUnit" p'
    ) => CoercePass (ParseResult l) p p' where
  coercePass pr = ParseResult{
        id             = coercePassId (Proxy @'(p, p')) pr.id
      , loc            = pr.loc
      , classification = coercePass pr.classification
      }

instance (
      CoercePass (C.Decl l) p p'
    , Ann "TranslationUnit" p ~ Ann "TranslationUnit" p'
    ) => CoercePass (ParseClassification l) p p' where
  coercePass = \case
    ParseResultSuccess s      -> ParseResultSuccess (coercePass s)
    ParseResultNotAttempted x -> ParseResultNotAttempted x
    ParseResultFailure x      -> ParseResultFailure x

instance (
      CoercePass (C.Decl l) p p'
    , Ann "TranslationUnit" p ~ Ann "TranslationUnit" p'
    ) => CoercePass (ParseSuccess l) p p' where
  coercePass ps = ParseSuccess{
        decl = coercePass ps.decl
      , delayedParseMsgs = ps.delayedParseMsgs
      }
