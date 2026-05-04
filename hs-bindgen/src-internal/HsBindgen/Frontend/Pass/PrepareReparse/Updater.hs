-- | Updater for 'ReparseInfo' after preprocessing
--
-- This module is intended to be imported unqualified. It is also intended to
-- only be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse"
-- module hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.Updater
--
module HsBindgen.Frontend.Pass.PrepareReparse.Updater (
    update
  ) where

import Prelude hiding (lex, print)

import Control.Monad.Reader (MonadReader, Reader, ReaderT (ReaderT), runReader)
import Data.Kind (Type)

import Clang.HighLevel.Types qualified as Clang

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo (ReparseNeeded, ReparseNotNeeded),
                                             Tokens)
import HsBindgen.Frontend.Pass.PrepareReparse.Flatten (flattenDefault,
                                                       flattenFunction)
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (FlatTokens (..),
                                                      PrepareReparse)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (CheckedMacro,
                                                       TypecheckMacros)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

update ::
     C.TranslationUnit TypecheckMacros
  -> C.TranslationUnit PrepareReparse
update unit = runM env $ updateIt () unit
  where
    env = Env

{-------------------------------------------------------------------------------
  Update: class
-------------------------------------------------------------------------------}

class Update a where
  type Ctx a :: Type
  type Ctx a = C.DeclInfo TypecheckMacros
  updateIt :: Ctx a -> a TypecheckMacros -> M (a PrepareReparse)

{-------------------------------------------------------------------------------
  Update: monad
-------------------------------------------------------------------------------}

runM :: Env -> M a -> a
runM m (M k) = runReader k m

newtype M a = M (Reader Env a)
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadReader Env M

data Env = Env

{-------------------------------------------------------------------------------
  Update: instances
-------------------------------------------------------------------------------}

instance Update C.TranslationUnit where
  type Ctx C.TranslationUnit = ()
  updateIt _ unit = do
      decls' <- mapM (updateIt ()) unit.decls
      pure C.TranslationUnit {
          decls = decls'
        , includeGraph = unit.includeGraph
        , ann = unit.ann
        }

instance Update C.Decl where
  type Ctx C.Decl = ()
  updateIt _ decl = do
      kind' <- updateIt decl.info decl.kind
      pure C.Decl {
          info = coercePass decl.info
        , kind = kind'
        , ann = decl.ann
        }

instance Update C.DeclKind where
  updateIt info declKind = case declKind of
      C.DeclStruct struct      -> C.DeclStruct           <$> recurse struct
      C.DeclUnion union        -> C.DeclUnion            <$> recurse union
      C.DeclTypedef typedef    -> C.DeclTypedef          <$> recurse typedef
      C.DeclEnum enum          -> C.DeclEnum             <$> recurse enum
      C.DeclAnonEnumConstant c -> C.DeclAnonEnumConstant <$> recurse c
      C.DeclOpaque             -> pure C.DeclOpaque
      C.DeclMacro macro        -> C.DeclMacro            <$> recurse macro
      C.DeclFunction function  -> C.DeclFunction         <$> recurse function
      C.DeclGlobal global      -> C.DeclGlobal           <$> recurse global
    where
      recurse :: forall a.
           (Update a, Ctx a ~ C.DeclInfo TypecheckMacros)
        => a TypecheckMacros
        -> M (a PrepareReparse)
      recurse = updateIt info

instance Update C.Struct where
  updateIt info struct = do
      fields' <- mapM (updateIt info) struct.fields
      flam' <- mapM (updateIt info) struct.flam
      pure C.Struct {
          sizeof = struct.sizeof
        , alignment = struct.alignment
        , fields = fields'
        , flam = flam'
        , ann = struct.ann
        }

instance Update C.StructField where
  updateIt _info field = do
      ann' <- updateReparseInfo False field.ann
      pure C.StructField {
          info = coercePass field.info
        , typ = coercePass field.typ
        , offset = field.offset
        , width = field.width
        , ann = ann'
        }

instance Update C.Union where
  updateIt info union = do
      fields' <- mapM (updateIt info) union.fields
      pure C.Union {
          sizeof = union.sizeof
        , alignment = union.alignment
        , fields = fields'
        , ann = union.ann
        }

instance Update C.UnionField where
  updateIt _info field = do
      ann' <- updateReparseInfo False field.ann
      pure C.UnionField {
          info = coercePass field.info
        , typ = coercePass field.typ
        , ann = ann'
        }

instance Update C.Typedef where
  updateIt _info typedef = do
      ann' <- updateReparseInfo False typedef.ann
      pure C.Typedef {
          typ = coercePass typedef.typ
        , ann = ann'
        }

instance Update C.Enum where
  updateIt _ enum = pure $ coercePass enum

instance Update C.AnonEnumConstant where
  updateIt _ constant = pure $ coercePass constant

instance Update CheckedMacro where
  updateIt _info macro = pure $ coercePass macro

instance Update C.Function where
  updateIt _info function = do
      ann' <- updateReparseInfo True function.ann
      pure C.Function {
          args = map coercePass function.args
        , res = coercePass function.res
        , attrs = function.attrs
        , ann = ann'
        }

instance Update C.Global where
  updateIt _info global = do
      ann' <- updateReparseInfo False global.ann
      pure C.Global {
          typ = coercePass global.typ
        , ann = ann'
        }

updateReparseInfo ::
     Bool
  -> ReparseInfo Tokens
  -> M (ReparseInfo FlatTokens)
updateReparseInfo isFunction reparseInfo = do
    case reparseInfo of
      ReparseNotNeeded -> pure ReparseNotNeeded
      ReparseNeeded tokens usedMacros -> do
        let flatten | isFunction = flattenFunction tokens
                    | otherwise  = flattenDefault tokens
            flatTokens = FlatTokens {
                flatten = flatten
              , locStart = getLocation tokens
              }
        pure $ ReparseNeeded flatTokens
                -- TODO: does this have to be updated, or is it fine if we don't?
                usedMacros

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

getLocation :: [Clang.Token a] -> Clang.MultiLoc
getLocation []    = panicPure "Unexpected empty list of tokens"
getLocation (t:_) = Clang.rangeStart $ Clang.tokenExtent t

