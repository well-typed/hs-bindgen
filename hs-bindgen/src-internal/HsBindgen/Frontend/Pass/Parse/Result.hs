-- | Results from the parser proper
module HsBindgen.Frontend.Pass.Parse.Result (
    ParseResult(..)
  , ParseSuccess(..)
  , ParseNotAttempted(..)
  , ParseFailure(..)
    -- * Convenience constructors
  , parseSucceed
  , parseSucceedWith
  , parseUnavailable
  , parseUnavailableWith
    -- * Query
  , parseResultId
  , parseResultInfo
  , getParseResultMaybeDecl
  , getParseResultEitherDecl
  ) where

import Text.SimplePrettyPrint qualified as PP

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
-- these results in the @FillUnnamedIds@ pass.
type ParseResult :: Star -> Pass -> Star
data ParseResult l p =
    ParseResultSuccess     (ParseSuccess l p)
    -- | We unexpectedly did not parse a declaration because it is reported
    -- "unavailable".
  | ParseResultUnavailable (ParseNotAttempted p)
  | ParseResultFailure     (ParseFailure p)
  deriving stock (Generic)

deriving stock instance ( IsPass p
                        , Macro.HasTypes l
                        ) => Show (ParseResult l p)

data ParseNotAttempted p = ParseNotAttempted{
      id               :: Id p
    , info             :: C.DeclInfo
    , delayedParseMsgs :: [DelayedParseMsg]
    }
  deriving stock (Generic)

deriving stock instance IsPass p => Show (ParseNotAttempted p)

data ParseFailure p = ParseFailure{
      id   :: Id p
    , info :: C.DeclInfo
    , msg  :: DelayedParseMsg
    }
  deriving stock (Generic)

deriving stock instance IsPass p => Show (ParseFailure p)

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
parseSucceedWith msgs decl = ParseResultSuccess ParseSuccess{
      decl             = decl
    , delayedParseMsgs = msgs
    }

-- | Assemble a parse result for an unavailable declaration
parseUnavailable :: Id p -> C.DeclInfo -> ParseResult l p
parseUnavailable = parseUnavailableWith []

-- | Assemble a parse result for an unavailable declaration with delayed parse
-- messages
parseUnavailableWith ::
     [DelayedParseMsg]
  -> Id p
  -> C.DeclInfo
  -> ParseResult l p
parseUnavailableWith msgs declId info = ParseResultUnavailable ParseNotAttempted{
      id               = declId
    , info             = info
    , delayedParseMsgs = msgs
    }

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

parseResultId :: ParseResult l p -> Id p
parseResultId = \case
    ParseResultSuccess     success     -> success.decl.id
    ParseResultUnavailable notAttempted -> notAttempted.id
    ParseResultFailure     failure     -> failure.id

parseResultInfo :: ParseResult l p -> C.DeclInfo
parseResultInfo = \case
    ParseResultSuccess     success     -> success.decl.info
    ParseResultUnavailable notAttempted -> notAttempted.info
    ParseResultFailure     failure     -> failure.info

getParseResultMaybeDecl :: ParseResult l p -> Maybe (C.Decl l p)
getParseResultMaybeDecl result =
    case result of
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
  coercePass = \case
    ParseResultSuccess     s -> ParseResultSuccess     (coercePass s)
    ParseResultUnavailable n -> ParseResultUnavailable (coercePass n)
    ParseResultFailure     f -> ParseResultFailure     (coercePass f)

instance CoercePassId p p' => CoercePass ParseNotAttempted p p' where
  coercePass n = ParseNotAttempted{
        id               = coercePassId (Proxy @'(p, p')) n.id
      , info             = n.info
      , delayedParseMsgs = n.delayedParseMsgs
      }

instance CoercePassId p p' => CoercePass ParseFailure p p' where
  coercePass f = ParseFailure{
        id   = coercePassId (Proxy @'(p, p')) f.id
      , info = f.info
      , msg  = f.msg
      }

instance (
      CoercePass (C.Decl l) p p'
    , Ann "TranslationUnit" p ~ Ann "TranslationUnit" p'
    ) => CoercePass (ParseSuccess l) p p' where
  coercePass ps = ParseSuccess{
        decl = coercePass ps.decl
      , delayedParseMsgs = ps.delayedParseMsgs
      }
