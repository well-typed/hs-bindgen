{-# LANGUAGE CPP #-}

-- | Main entry point to the @language-c@ infrastructure
--
-- It should not be necessary to import any other module in @LanguageC.*@.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.LanguageC qualified as LanC
module HsBindgen.Frontend.LanguageC (
    Parser
  , Error(..)
  , reparseFunDecl
  , reparseTypedef
  , reparseField
  , parseMacroType
    -- * Scoping
  , ReparseEnv
  , initReparseEnv
  ) where

import Control.Monad
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Data.Tuple (swap)
import Language.C qualified as LanC
import Language.C.Data.Position qualified as LanC

import Clang.Enum.Simple qualified as Clang
import Clang.HighLevel.Types qualified as Clang
import Clang.LowLevel.Core qualified as Clang
import Clang.Paths qualified as Clang

import HsBindgen.Clang.CStandard
import HsBindgen.CPP.Clang qualified as CPP
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LanguageC.Error
import HsBindgen.Frontend.LanguageC.Monad
import HsBindgen.Frontend.LanguageC.PartialAST
import HsBindgen.Frontend.LanguageC.PartialAST.FromLanC
import HsBindgen.Frontend.LanguageC.PartialAST.ToBindgen
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
#if !MIN_VERSION_language_c(0,10,2)
import HsBindgen.Language.C qualified as C
#endif

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type Parser a =
     CPP.PreprocessorContext
  -> ReparseEnv
  -> [Clang.Token Clang.TokenSpelling]
  -> IO (Either Error a)

-- | Reparse function declaration
--
-- Returns the function parameters, function result, and function name.
reparseFunDecl ::
     Parser (
         ( [(Maybe CName, C.Type HandleMacros)]
         , C.Type HandleMacros
         )
       , CName
       )
reparseFunDecl = parseWith flattenFunDecl True (fmap swap . fromFunDecl)

-- | Reparse typedef
reparseTypedef :: Parser (C.Type HandleMacros)
reparseTypedef = parseWith defaultFlatten True (fmap snd . fromDecl)

-- | Reparse struct/union field
reparseField :: Parser (C.Type HandleMacros, CName)
reparseField = parseWith defaultFlatten True (fmap swap .  fromNamedDecl)

-- | Parse macro-defined type
--
-- Unlike the other parsers, this is not /re/parsing: we are parsing this macro
-- for the first time.
parseMacroType :: Parser (C.Type HandleMacros)
parseMacroType = parseWith flattenMacroTypeDef False (fromDecl >=> checkNotVoid)
  where
    -- @void@ does not make sense as a top-level type
    checkNotVoid ::
         (Maybe CName, C.Type HandleMacros)
      -> FromLanC (C.Type HandleMacros)
    checkNotVoid (_name, typ) =
        case typ of
          C.TypeVoid -> unsupported "type 'void'"
          _otherwise -> return typ

{-------------------------------------------------------------------------------
  Internal auxiliary: run the language-c parser
-------------------------------------------------------------------------------}

parseWith ::
     ([Clang.Token Clang.TokenSpelling] -> String)
     -- ^ Flatten tokens into raw string we can feed to language-c
  -> Bool
     -- ^ Perform macro expansion on the raw tokens
  -> (PartialDecl -> FromLanC a)
     -- ^ Construct our AST from the partial declaration
  -> Parser a
parseWith flatten doExpand fromPartial prepCtx env tokens = do
    rawExpanded <- getRawExpanded
    pure $ runFromLanC env $ do
      partial <- parseUsingLanC (getLocation tokens) rawExpanded
      fromPartial partial
  where
    -- TODO: debug trace for raw, flattend, and expanded tokens
    getRawExpanded :: IO String
    getRawExpanded
      | doExpand
      = CPP.preprocess prepCtx raw >>= \case
          Left _err -> pure raw -- TODO: trace a warning.
          Right raw' -> pure raw' -- TODO: is this a safe fallback?
      | otherwise
      = pure raw

    -- TODO: debug trace for raw, flattened tokens
    raw :: String
    raw = flatten tokens

parseUsingLanC ::
     Clang.MultiLoc -- ^ Approximate location of the string in the source
  -> String         -- ^ Raw string
  -> FromLanC PartialDecl
parseUsingLanC mloc raw = do
    reparseEnv <- getReparseEnv

    let predefinedTypes' :: [LanC.Ident]
        uniqNameSupply   :: [LanC.Name]
        (predefinedTypes', uniqNameSupply) = runWithNewNameSupply $ do
            mapM declarePredefined $ (Map.keys reparseEnv.types)

    case LanC.execParser
           LanC.extDeclP
           (LanC.inputStreamFromString raw)
           (multiLocToLanC mloc)
           predefinedTypes'
           uniqNameSupply of
      Left err ->
        unexpected $ show err
      Right (fromLanC, _finalNameSupply) -> do
        decl <- fromCDeclExt fromLanC
        mkPartialDecl decl

fromCDeclExt :: LanC.CExternalDeclaration a -> FromLanC (LanC.CDeclaration a)
fromCDeclExt = \case
    LanC.CDeclExt decl -> return decl
    other              -> unexpectedF other

{-------------------------------------------------------------------------------
  Auxiliary: locations
-------------------------------------------------------------------------------}

getLocation :: [Clang.Token a] -> Clang.MultiLoc
getLocation []    = panicPure "Unexpected empty list of tokens"
getLocation (t:_) = Clang.rangeStart $ Clang.tokenExtent t

multiLocToLanC :: Clang.MultiLoc -> LanC.Position
multiLocToLanC mloc =
    LanC.position
      (Clang.singleLocOffset sloc)
      (Clang.getSourcePath $ Clang.singleLocPath sloc)
      (Clang.singleLocLine   sloc)
      (Clang.singleLocColumn sloc)
      Nothing -- @posParentFile@ in @language-c@: "including file, if any"
  where
    -- NOTE: It might make sense to use 'multiLocPresumed' instead, but these
    -- don't provide file offsets (see 'Clang.PresumedLoc').
    sloc :: Clang.SingleLoc
    sloc = Clang.multiLocExpansion mloc

{-------------------------------------------------------------------------------
  Flatten: produce a raw string we can give to language-c to parse
-------------------------------------------------------------------------------}

flattenTokens :: String -> [Clang.Token Clang.TokenSpelling] -> String
flattenTokens trailer allTokens =
    CPP.prettyTokens (skipComments allTokens) (' ' : trailer)

defaultFlatten :: [Clang.Token Clang.TokenSpelling] -> String
defaultFlatten = flattenTokens ";"

flattenFunDecl :: [Clang.Token Clang.TokenSpelling] -> String
flattenFunDecl allTokens = defaultFlatten (skipFunctionBody allTokens)

flattenMacroTypeDef :: [Clang.Token Clang.TokenSpelling] -> String
flattenMacroTypeDef []
  = panicPure "Unexpected empty list of tokens"
#if MIN_VERSION_language_c(0,10,2)
-- language-c 0.10.2 lexes @bool@ as a keyword, so it cannot be used as a
-- typedef name. We substitute a placeholder identifier instead.
-- 'parseMacroType' only uses the type from the parsed declaration, not the
-- name, so the placeholder has no effect on the result.
flattenMacroTypeDef (name:def)
  | Clang.getTokenSpelling (Clang.tokenSpelling name) == "bool"
  = "typedef " ++ flattenTokens "_hsbg_bool ;" def
#endif
flattenMacroTypeDef (name:def)
  = "typedef " ++ flattenTokens (CPP.prettyToken name " ;") def

skipComments :: [Clang.Token Clang.TokenSpelling] -> [Clang.Token Clang.TokenSpelling]
skipComments = mapMaybe $ \t -> case Clang.fromSimpleEnum $ Clang.tokenKind t of
      Right Clang.CXToken_Comment -> Nothing
      _otherwise -> Just t

skipFunctionBody :: [Clang.Token Clang.TokenSpelling] -> [Clang.Token Clang.TokenSpelling]
skipFunctionBody toks = go toks
  where
    go :: [Clang.Token Clang.TokenSpelling] -> [Clang.Token Clang.TokenSpelling]
    go []     = []
    go (t:ts) = case Clang.fromSimpleEnum $ Clang.tokenCursorKind t of
        Right Clang.CXCursor_CompoundStmt -> []
        _otherwise -> t : go ts

{-------------------------------------------------------------------------------
  Construct type environment
-------------------------------------------------------------------------------}

-- | Initial 'ReparseTypeEnv'
--
-- This is not quite empty: it contains some "built in" types.
initReparseEnv :: ClangCStandard -> ReparseEnv
initReparseEnv standard = ReparseEnv {
      types = Map.fromList (bespokeTypes standard)
    }

-- | \"Primitive\" we expect the reparser to recognize
--
-- The language-c parser does not support these explicitly.
bespokeTypes :: ClangCStandard -> [(CName, C.Type HandleMacros)]
bespokeTypes = \case
#if !MIN_VERSION_language_c(0,10,2)
    -- Make sure that we really only replace keywords lacking definitions.
    --
    -- If we add entries for types to `bespokeTypes` which are not keywords
    -- (i.e., are not part of the standard), we will pretend to know what these
    -- types are, but the actual type must come from a header, and we actually
    -- do not know what that definition is.
    ClangCStandard C23 _gnu -> [("bool", C.TypePrim C.PrimBool)]
#endif
    _otherwise -> []

{-------------------------------------------------------------------------------
  Auxiliary language-c: working with the unique name supply
-------------------------------------------------------------------------------}

newtype WithNameSupply a = WrapWithNameSupply (State [LanC.Name] a)
  deriving newtype (
      Functor
    , Applicative
    , Monad
    )

runWithNameSupply :: [LanC.Name] -> WithNameSupply a -> (a, [LanC.Name])
runWithNameSupply supply (WrapWithNameSupply ma) = State.runState ma supply

runWithNewNameSupply :: WithNameSupply a -> (a, [LanC.Name])
runWithNewNameSupply = runWithNameSupply LanC.newNameSupply

getUniqueName :: WithNameSupply LanC.Name
getUniqueName = WrapWithNameSupply $ State.state aux
  where
    aux :: [LanC.Name] -> (LanC.Name, [LanC.Name])
    aux (n:ns) = (n,ns)
    aux []     = panicPure "No more unique names"

declarePredefined :: CName -> WithNameSupply LanC.Ident
declarePredefined name =
    LanC.mkIdent LanC.nopos (Text.unpack name) <$> getUniqueName

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

_testLanCParser :: [String] -> String -> Either LanC.ParseError LanC.CExtDecl
_testLanCParser predefinedTypes input = fst <$>
    LanC.execParser
      LanC.extDeclP
      (LanC.inputStreamFromString input)
      LanC.nopos
      (map LanC.builtinIdent predefinedTypes)
      LanC.newNameSupply
