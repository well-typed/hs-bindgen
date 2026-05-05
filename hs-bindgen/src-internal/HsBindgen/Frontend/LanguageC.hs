{-# LANGUAGE CPP #-}

-- | Main entry point to the @language-c@ infrastructure
--
-- It should not be necessary to import any other module in @LanguageC.*@ (but
-- to avoid circular module imports).
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
  , reparseGlobal
    -- * Scoping
  , ReparseEnv
  , initReparseEnv
  ) where

import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Tuple (swap)
import Language.C qualified as LanC
import Language.C.Data.Position qualified as LanC

import Clang.HighLevel.Types qualified as Clang
import Clang.Paths qualified as Clang

import HsBindgen.Clang.CStandard
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LanguageC.Error
import HsBindgen.Frontend.LanguageC.Monad
import HsBindgen.Frontend.LanguageC.PartialAST
import HsBindgen.Frontend.LanguageC.PartialAST.FromLanC
import HsBindgen.Frontend.LanguageC.PartialAST.ToBindgen
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass

#if !MIN_VERSION_language_c(0,10,2)
import HsBindgen.Language.C qualified as C
#endif

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

type Parser a =
     ReparseEnv ReparseMacroExpansions
  -> FlatTokens
  -> Either Error a

-- | Reparse function declaration
--
-- Returns the function parameters, function result, and function name.
reparseFunDecl ::
     Parser (
         ( [(Maybe CName, C.Type ReparseMacroExpansions)]
         , C.Type ReparseMacroExpansions
         )
       , CName
       )
reparseFunDecl = parseWith (fmap swap . fromFunDecl)

-- | Reparse typedef
reparseTypedef :: Parser (C.Type ReparseMacroExpansions)
reparseTypedef = parseWith (fmap snd . fromDecl)

-- | Reparse struct/union field
reparseField :: Parser (C.Type ReparseMacroExpansions, CName)
reparseField = parseWith (fmap swap .  fromNamedDecl)

-- | Reparse global variable declaration
reparseGlobal :: Parser (C.Type ReparseMacroExpansions)
reparseGlobal = parseWith (fmap snd . fromDecl)

{-------------------------------------------------------------------------------
  Internal auxiliary: run the language-c parser
-------------------------------------------------------------------------------}

parseWith ::
     (PartialDecl -> FromLanC a)
     -- ^ Construct our AST from the partial declaration
  -> Parser a
parseWith fromPartial env tokens =
    runFromLanC env $ do
      partial <- parseUsingLanC tokens.locStart tokens.flatten
      fromPartial partial

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
  Construct type environment
-------------------------------------------------------------------------------}

-- | Initial 'ReparseEnv'
--
-- This is not quite empty: it contains some "built in" types.
initReparseEnv :: ClangCStandard -> ReparseEnv p
initReparseEnv standard = Map.fromList (bespokeTypes standard)

-- | \"Primitive\" we expect the reparser to recognize
--
-- The language-c parser does not support these explicitly.
bespokeTypes :: ClangCStandard -> [(CName, C.Type p)]
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
