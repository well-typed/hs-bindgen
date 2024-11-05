{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Backend.PP.Translation (
    HsModuleOpts(..)
  , Module(..)
  , translate
  ) where

import Data.Set (Set)
import Data.Set qualified as Set

import HsBindgen.Backend.Common
import HsBindgen.Backend.Common.Translation
import HsBindgen.Backend.PP
import HsBindgen.C.AST qualified as C
import HsBindgen.Translation.LowLevel

{-------------------------------------------------------------------------------
  Generate top-level module
-------------------------------------------------------------------------------}

newtype HsModuleOpts = HsModuleOpts {
      hsModuleName :: String
    }
  deriving (Show)

data Module = Module {
      moduleName    :: String
    , moduleImports :: [QualifiedImport]
    , moduleDecls   :: [SDecl BE]
    }

translate :: HsModuleOpts -> C.Header -> Module
translate opts header =
    let moduleName = hsModuleName opts
        (moduleDecls, _) = runM $ mapM (toBE BE) (generateDeclarations header)
        moduleImports =
          Set.toAscList . mconcat $ map getDeclQualifiedImports moduleDecls
    in  Module{..}

{-------------------------------------------------------------------------------
  Auxilliary: imports
-------------------------------------------------------------------------------}

getDeclQualifiedImports :: SDecl BE -> Set QualifiedImport
getDeclQualifiedImports = \case
    DVar _name e -> getExprQualifiedImports e
    DInst Instance{..} -> mconcat $
        getGlobalQualifiedImports instanceClass
      : map (getGlobalQualifiedImports . fst) instanceDecs
      ++ map (getExprQualifiedImports . snd) instanceDecs
    DRecord Record{..} -> mconcat $
      map (getTypeQualifiedImports . snd) dataFields
    DNewtype Newtype{..} -> getTypeQualifiedImports newtypeType

getGlobalQualifiedImports :: Global -> Set QualifiedImport
getGlobalQualifiedImports =
    maybe Set.empty Set.singleton . resolvedNameQualifier . resolve BE

getExprQualifiedImports :: SExpr BE -> Set QualifiedImport
getExprQualifiedImports = \case
    EGlobal g -> getGlobalQualifiedImports g
    EVar _x -> Set.empty
    ECon _n -> Set.empty
    EInt _i -> Set.empty
    EApp f x -> getExprQualifiedImports f <> getExprQualifiedImports x
    EInfix op x y ->
      getGlobalQualifiedImports op
        <> getExprQualifiedImports x
        <> getExprQualifiedImports y
    ELam _mPat body -> getExprQualifiedImports body
    ECase x ms -> mconcat $
        getExprQualifiedImports x
      : map (\(_, _, body) -> getExprQualifiedImports body) ms
    EInj x -> getExprQualifiedImports x

getTypeQualifiedImports :: SType BE -> Set QualifiedImport
getTypeQualifiedImports = \case
    TGlobal g -> getGlobalQualifiedImports g
    TCon _n -> Set.empty
    TApp c x -> getTypeQualifiedImports c <> getTypeQualifiedImports x
