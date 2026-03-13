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
import Data.Text qualified as Text
import Data.Tuple (swap)
import Language.C qualified as LanC
import Language.C.Data.Position qualified as LanC

import Clang.Enum.Simple qualified as Clang
import Clang.HighLevel.Types qualified as Clang
import Clang.LowLevel.Core qualified as Clang
import Clang.Paths qualified as Clang

import HsBindgen.Clang.CStandard
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
     ReparseEnv
  -> [Clang.Token Clang.TokenSpelling]
  -> Either Error a

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
reparseFunDecl = parseWith flattenFunDecl (fmap swap . fromFunDecl)

-- | Reparse typedef
reparseTypedef :: Parser (C.Type HandleMacros)
reparseTypedef = parseWith defaultFlatten (fmap snd . fromDecl)

-- | Reparse struct/union field
reparseField :: Parser (C.Type HandleMacros, CName)
reparseField = parseWith defaultFlatten (fmap swap .  fromNamedDecl)

-- | Parse macro-defined type
--
-- Unlike the other parsers, this is not /re/parsing: we are parsing this macro
-- for the first time.
parseMacroType :: Parser (C.Type HandleMacros)
#if MIN_VERSION_language_c(0,10,2)
-- When we parse a macro body like @_Bool@ in @#define BOOL _Bool@,
-- language-c 0.10.2 produces @CBoolType@, whose handler looks up
-- @\"bool\"@ in the reparse env. If stdbool.h was already processed,
-- that lookup finds stdbool.h's entry instead of returning @PrimBool@.
-- Removing @\"bool\"@ from the env here avoids that, matching the
-- behaviour of language-c < 0.10.2.
parseMacroType env =
    parseWith flattenMacroTypeDef (fromDecl >=> checkNotVoid)
      (Map.delete "bool" env)
#else
parseMacroType = parseWith flattenMacroTypeDef (fromDecl >=> checkNotVoid)
#endif
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
  -> (PartialDecl -> FromLanC a)
     -- ^ Construct our AST from the partial declaration
  -> Parser a
parseWith flatten fromPartial env tokens =
    runFromLanC env $ do
      partial <- parseUsingLanC (getLocation tokens) raw
      fromPartial partial
  where
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
            mapM declarePredefined $ Map.keys reparseEnv

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
    go allTokens
  where
    -- Skip over comments
    go :: [Clang.Token Clang.TokenSpelling] -> String
    go []     = trailer
    go (t:ts) =
        case Clang.fromSimpleEnum $ Clang.tokenKind t of
          Right Clang.CXToken_Comment -> go ts
          _otherwise -> prependToken t $ go ts

defaultFlatten :: [Clang.Token Clang.TokenSpelling] -> String
defaultFlatten = flattenTokens ";"

flattenFunDecl :: [Clang.Token Clang.TokenSpelling] -> String
flattenFunDecl allTokens =
    go allTokens
  where
    go :: [Clang.Token Clang.TokenSpelling] -> String
    go []     = ";"
    go (t:ts) =
        case ( Clang.fromSimpleEnum $ Clang.tokenKind       t
             , Clang.fromSimpleEnum $ Clang.tokenCursorKind t
             ) of

          -- Skip over comments
          (Right Clang.CXToken_Comment, _) -> go ts

          -- Ignore function body, if present
          (_, Right Clang.CXCursor_CompoundStmt) -> ";"

          -- Everything else we just add to the raw string
          _otherwise -> prependToken t $ go ts

flattenMacroTypeDef :: [Clang.Token Clang.TokenSpelling] -> String
flattenMacroTypeDef []         = panicPure "Unexpected empty list of tokens"
#if MIN_VERSION_language_c(0,10,2)
-- language-c 0.10.2 lexes @bool@ as a keyword, so it cannot be used as a
-- typedef name. We substitute a placeholder identifier instead.
-- 'parseMacroType' only uses the type from the parsed declaration, not the
-- name, so the placeholder has no effect on the result.
flattenMacroTypeDef (name:def)
  | Clang.getTokenSpelling (Clang.tokenSpelling name) == "bool"
  = "typedef " ++ flattenTokens "_hsbg_bool ;" def
  | otherwise
  = "typedef " ++ defaultFlatten (def ++ [name])
#else
flattenMacroTypeDef (name:def) = "typedef " ++ defaultFlatten (def ++ [name])
#endif

prependToken :: Clang.Token Clang.TokenSpelling -> String -> String
prependToken token rest = concat [
      Text.unpack (Clang.getTokenSpelling $ Clang.tokenSpelling token)
    , " "
    , rest
    ]

{-------------------------------------------------------------------------------
  Construct type environment
-------------------------------------------------------------------------------}

-- | Initial 'ReparseTypeEnv'
--
-- This is not quite empty: it contains some "built in" types.
initReparseEnv :: ClangCStandard -> ReparseEnv
initReparseEnv standard = Map.fromList (bespokeTypes standard)

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
