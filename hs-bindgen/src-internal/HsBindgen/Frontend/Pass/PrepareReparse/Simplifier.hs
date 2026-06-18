-- | Simplifiers for 'PreHeader'
--
-- This module is intended to be imported unqualified. It is also intended to only
-- be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse" module
-- hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.Simplifier
--
module HsBindgen.Frontend.Pass.PrepareReparse.Simplifier (
    simplify
  , Simplify
    -- * Tags
  , fieldTag
  , functionTag
  , typedefTag
  , variableTag
  ) where

import Prelude hiding (print)

import Data.Either
import Data.Kind

import Clang.HighLevel.Types

import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.PrepareReparse.AST
import HsBindgen.Frontend.Pass.PrepareReparse.Flatten
import HsBindgen.Frontend.Pass.PrepareReparse.Printer.Util qualified as P
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.Type

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

simplify :: Simplify a => Ctx a -> a TypecheckMacros -> Simple a
simplify = simplifyIt

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

class Simplify a where
  type Ctx a :: Type
  type Ctx a = C.DeclInfo TypecheckMacros
  type Simple a :: Type
  type Simple a = [Either Undef Target]
  simplifyIt :: Ctx a -> a TypecheckMacros -> Simple a

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Simplify (C.TranslationUnit l) where
  type Ctx    (C.TranslationUnit l) = ()
  type Simple (C.TranslationUnit l) = (Include -> PreHeader)
  simplifyIt _ (unit) = \include -> PreHeader {
        include = include
      , undefs  = undefs
      , targets = targets
      }
    where
      (undefs, targets) =
        partitionEithers $ concatMap recurse unit.decls
      recurse = simplifyIt ()

instance Simplify (C.Decl l) where
  type Ctx (C.Decl l) = ()
  simplifyIt _ decl = case decl.kind of
      C.DeclStruct struct      -> recurse struct
      C.DeclUnion union        -> recurse union
      C.DeclTypedef typedef    -> recurse typedef
      C.DeclEnum enum          -> recurse enum
      C.DeclAnonEnumConstant c -> recurse c
      C.DeclOpaque{}           -> nothing
      C.DeclMacro macro        -> recurse $ Flip macro
      C.DeclFunction function  -> recurse function
      C.DeclGlobal global      -> recurse global
    where
      recurse :: forall a.
           (Simplify a, Ctx a ~ C.DeclInfo TypecheckMacros)
        => a TypecheckMacros
        -> Simple a
      recurse = simplifyIt decl.info

instance Simplify C.Struct where
  simplifyIt info struct =
      concatMap (simplifyIt info) struct.fields ++
      concatMap (simplifyIt info) struct.flam

instance Simplify C.StructField where
  simplifyIt info field = case field.ann of
      ReparseNotNeeded -> nothing
      ReparseNeeded tokens _macroInvs -> singleTarget $
        Target (fieldTag info field.info) (defaultDecl tokens)

instance Simplify C.Union where
  simplifyIt info union = concatMap (simplifyIt info) union.fields

instance Simplify C.UnionField where
  simplifyIt info field = case field.ann of
      ReparseNotNeeded -> nothing
      ReparseNeeded tokens _macroInvs -> singleTarget $
        Target (fieldTag info field.info) (defaultDecl tokens)

instance Simplify C.Typedef where
  simplifyIt info typedef = case typedef.ann of
      ReparseNotNeeded -> nothing
      ReparseNeeded tokens _macroInvs -> singleTarget $
        Target (typedefTag info) (defaultDecl tokens)

instance Simplify C.Enum where
  simplifyIt _ _ = nothing

instance Simplify C.AnonEnumConstant where
  simplifyIt _ _ = nothing

instance Simplify (Flip TypecheckedMacro l) where
  simplifyIt info (Flip m) = case m of
    MacroType{}  -> singleUndef $ Undef (MacroName (P.name info ""))
    MacroValue{} -> nothing

instance Simplify C.Function where
  simplifyIt info function = case function.ann of
      ReparseNotNeeded -> nothing
      ReparseNeeded tokens _macroInvs -> singleTarget $
        Target (functionTag info) (functionDecl tokens)

instance Simplify C.Global where
  simplifyIt info global = case global.ann of
      ReparseNotNeeded -> nothing
      ReparseNeeded tokens _macroInvs -> singleTarget $
        Target (variableTag info) (defaultDecl tokens)

{-------------------------------------------------------------------------------
  Lift into Simple
-------------------------------------------------------------------------------}

nothing :: [Either Undef Target]
nothing = []

singleTarget :: Target -> [Either Undef Target]
singleTarget t = [Right t]

singleUndef :: Undef -> [Either Undef Target]
singleUndef u = [Left u]

{-------------------------------------------------------------------------------
  Decls
-------------------------------------------------------------------------------}

defaultDecl :: [Token TokenSpelling] -> Decl
defaultDecl tokens = Decl $ flattenDefault tokens

functionDecl :: [Token TokenSpelling] -> Decl
functionDecl tokens = Decl $ flattenFunction tokens

{-------------------------------------------------------------------------------
  Tags
-------------------------------------------------------------------------------}

fieldTag :: C.DeclInfo TypecheckMacros -> C.FieldInfo TypecheckMacros -> Tag
fieldTag info fieldInfo = Tag Field (TagName name)
  where
    name = P.name info  . P.dot . P.fieldName fieldInfo $ ""

functionTag :: C.DeclInfo TypecheckMacros -> Tag
functionTag info = Tag Function (TagName (P.name info ""))

typedefTag :: C.DeclInfo TypecheckMacros -> Tag
typedefTag info = Tag Typedef (TagName (P.name info ""))

variableTag :: C.DeclInfo TypecheckMacros -> Tag
variableTag info = Tag Variable (TagName (P.name info ""))
