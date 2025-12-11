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

import Clang.Args (CStandard (..))
import Clang.Enum.Simple qualified as Clang
import Clang.HighLevel.Types qualified as Clang
import Clang.LowLevel.Core qualified as Clang
import Clang.Paths qualified as Clang

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.LanguageC.Monad
import HsBindgen.Frontend.LanguageC.PartialAST
import HsBindgen.Frontend.LanguageC.PartialAST.FromLanC
import HsBindgen.Frontend.LanguageC.PartialAST.ToBindgen
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type Parser p a =
     ReparseEnv p
  -> [Clang.Token Clang.TokenSpelling]
  -> Either Error a

-- | Reparse function declaration
--
-- Returns the function parameters, function result, and function name.
reparseFunDecl ::
     CanApply p
  => Parser p (
         ( [(Maybe CName, Type p)]
         , Type p
         )
       , CName
       )
reparseFunDecl = parseWith flattenFunDecl (fmap swap . fromFunDecl)

-- | Reparse typedef
reparseTypedef :: CanApply p => Parser p (Type p)
reparseTypedef = parseWith defaultFlatten (fmap snd . fromDecl)

-- | Reparse struct/union field
reparseField :: CanApply p => Parser p (Type p, CName)
reparseField = parseWith defaultFlatten (fmap swap .  fromNamedDecl)

-- | Parse macro-defined type
--
-- Unlike the other parsers, this is not /re/parsing: we are parsing this macro
-- for the first time.
parseMacroType :: CanApply p => Parser p (Type p)
parseMacroType = parseWith flattenMacroTypeDef (fromDecl >=> checkNotVoid)
  where
    -- @void@ does not make sense as a top-level type
    checkNotVoid :: (Maybe CName, Type p) -> FromLanC p (Type p)
    checkNotVoid (_name, typ) =
        case typ of
          TypeVoid   -> unsupported "type 'void'"
          _otherwise -> return typ

{-------------------------------------------------------------------------------
  Internal auxiliary: run the language-c parser
-------------------------------------------------------------------------------}

parseWith ::
     CanApply p
  => ([Clang.Token Clang.TokenSpelling] -> String)
     -- ^ Flatten tokens into raw string we can feed to language-c
  -> (PartialDecl p -> FromLanC p a)
     -- ^ Construct our AST from the partial declaration
  -> Parser p a
parseWith flatten fromPartial env tokens =
    runFromLanC env $ do
      partial <- parseUsingLanC (getLocation tokens) raw
      fromPartial partial
  where
    raw :: String
    raw = flatten tokens

parseUsingLanC ::
     CanApply p
  => Clang.MultiLoc -- ^ Approximate location of the string in the source
  -> String         -- ^ Raw string
  -> FromLanC p (PartialDecl p)
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

fromCDeclExt :: LanC.CExternalDeclaration a -> FromLanC p (LanC.CDeclaration a)
fromCDeclExt = \case
    LanC.CDeclExt decl -> return decl
    other              -> unexpectedF other

{-------------------------------------------------------------------------------
  Auxiliary: locations
-------------------------------------------------------------------------------}

getLocation :: [Clang.Token a] -> Clang.MultiLoc
getLocation []    = panicPure "unexpected empty list of tokens"
getLocation (t:_) = Clang.rangeStart $ Clang.tokenExtent t

multiLocToLanC :: Clang.MultiLoc -> LanC.Position
multiLocToLanC mloc =
    LanC.position
      (Clang.singleLocOffset sloc)
      (Clang.getSourcePath $ Clang.singleLocPath sloc)
      (Clang.singleLocLine   sloc)
      (Clang.singleLocColumn sloc)
      Nothing -- TODO: parent?
  where
    -- TODO: Should this use 'multiLocPresumed' instead?
    sloc :: Clang.SingleLoc
    sloc = Clang.multiLocExpansion mloc

{-------------------------------------------------------------------------------
  Flatten: produce a raw string we can give to language-c to parse
-------------------------------------------------------------------------------}

defaultFlatten :: [Clang.Token Clang.TokenSpelling] -> String
defaultFlatten allTokens =
    go allTokens
  where
    -- Skip over comments
    go :: [Clang.Token Clang.TokenSpelling] -> String
    go []     = ";"
    go (t:ts) =
        case Clang.fromSimpleEnum $ Clang.tokenKind t of
          Right Clang.CXToken_Comment -> go ts
          _otherwise -> prependToken t $ go ts

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
flattenMacroTypeDef []         = panicPure "unexpected empty list of tokens"
flattenMacroTypeDef (name:def) = "typedef " ++ defaultFlatten (def ++ [name])

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
initReparseEnv :: CStandard -> ReparseEnv p
initReparseEnv standard = Map.fromList (bespokeTypes standard)

-- | \"Primitive\" we expect the reparser to recognize
--
-- The language-c parser does not support these explicitly.
bespokeTypes :: CStandard -> [(CName, Type p)]
bespokeTypes = \case
   -- Make sure that we really only replace keywords lacking definitions.
   --
   -- If we add entries for types to `bespokeTypes` which are not keywords
   -- (i.e., are not part of the standard), we will pretend to know what these
   -- types are, but the actual type must come from a header, and we actually do
   -- not know what that defintion is.
   C23 -> [("bool"   , TypePrim C.PrimBool)]
   _otherwise -> []

{-------------------------------------------------------------------------------
  Auxiliary language-c: working with the unique name supply
-------------------------------------------------------------------------------}

newtype WithNameSupply a = WrapWithNameSupply {
      unwrapWithNameSupply :: State [LanC.Name] a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    )

runWithNameSupply :: [LanC.Name] -> WithNameSupply a -> (a, [LanC.Name])
runWithNameSupply supply = flip State.runState supply . unwrapWithNameSupply

runWithNewNameSupply :: WithNameSupply a -> (a, [LanC.Name])
runWithNewNameSupply = runWithNameSupply LanC.newNameSupply

getUniqueName :: WithNameSupply LanC.Name
getUniqueName = WrapWithNameSupply $ State.state aux
  where
    aux :: [LanC.Name] -> (LanC.Name, [LanC.Name])
    aux (n:ns) = (n,ns)
    aux []     = panicPure "impossible: no more unique names"

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
