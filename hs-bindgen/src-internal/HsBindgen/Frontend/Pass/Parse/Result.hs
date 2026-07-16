-- | Results from the parser proper
module HsBindgen.Frontend.Pass.Parse.Result (
    ParseResult(..)
  , ParseClassification(..)
  , ParseSuccess(..)
    -- * Convenience constructors
  , parseSucceed
  , parseSucceedWith
  , parseUnavailable
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
    -- | We unexpectedly did not parse a declaration because it is reported
    -- "unavailable".
  | ParseResultUnavailable
  | ParseResultFailure      DelayedParseMsg
  deriving stock (Generic)

deriving stock instance ( IsPass p
                        , Macro.HasTypes l
                        ) => Show (ParseClassification l p)

data ParseSuccess l p = ParseSuccess {
      decl             :: C.Decl l p
    , delayedParseMsgs :: [DelayedParseMsg]
    }
  deriving stock (Generic)

deriving stock instance (
    IsPass p
  , Macro.HasTypes l
  ) => Show (ParseSuccess l p)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance PrettyForTrace (ParseSuccess l p) where
  prettyForTrace success =
      if null success.delayedParseMsgs then
        "Parse success"
      else
        PP.hang "Parse success with messages:" 2 $
          PP.vcat $
          map prettyForTrace success.delayedParseMsgs

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

-- | Assemble a parse result for an unavailable declaration
parseUnavailable :: C.DeclInfo p -> ParseResult l p
parseUnavailable info = ParseResult{
      id              = info.id
    , loc             = info.loc
    , classification = ParseResultUnavailable
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
    ParseResultSuccess s   -> ParseResultSuccess (coercePass s)
    ParseResultUnavailable -> ParseResultUnavailable
    ParseResultFailure x   -> ParseResultFailure x

instance (
      CoercePass (C.Decl l) p p'
    , Ann "TranslationUnit" p ~ Ann "TranslationUnit" p'
    ) => CoercePass (ParseSuccess l) p p' where
  coercePass ps = ParseSuccess{
        decl = coercePass ps.decl
      , delayedParseMsgs = ps.delayedParseMsgs
      }
