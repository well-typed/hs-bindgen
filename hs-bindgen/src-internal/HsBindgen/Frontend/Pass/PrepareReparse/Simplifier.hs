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

import HsBindgen.Frontend.Pass.ApplyMacroFallback.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.PrepareReparse.AST
import HsBindgen.Frontend.Pass.PrepareReparse.Flatten
import HsBindgen.Frontend.Pass.PrepareReparse.Printer.Util qualified as P
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.Flip

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

simplify :: Simplify a => Ctx a -> a ApplyMacroFallback -> Simple a
simplify = simplifyIt

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

class Simplify a where
  type Ctx a :: Type
  type Ctx a = C.DeclId
  type Simple a :: Type
  type Simple a = [Either Undef Target]
  simplifyIt :: Ctx a -> a ApplyMacroFallback -> Simple a

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
      C.DeclStruct struct          -> recurse struct
      C.DeclUnion union            -> recurse union
      C.DeclTypedef typedef        -> recurse typedef
      C.DeclEnum enum              -> recurse enum
      C.DeclUntaggedEnumConstant c -> recurse c
      C.DeclOpaque{}               -> nothing
      C.DeclMacro macro            -> recurse $ Flip macro
      C.DeclFunction function      -> recurse function
      C.DeclGlobal global          -> recurse global
    where
      recurse :: forall a.
           (Simplify a, Ctx a ~ C.DeclId)
        => a ApplyMacroFallback
        -> Simple a
      recurse = simplifyIt decl.id

instance Simplify C.Struct where
  simplifyIt declId struct =
      concatMap (simplifyIt declId) struct.fields ++
      foldMap (simplifyIt declId) (C.flamStructField struct.flam)

instance Simplify C.Union where
  simplifyIt declId union = concatMap (simplifyIt declId) union.fields

instance Simplify C.Field where
  simplifyIt declId = C.elimField (simplifyIt declId) (simplifyIt declId)

instance Simplify C.RegularField where
  simplifyIt declId field = case field.ann of
        ReparseNotNeeded -> nothing
        ReparseNeeded tokens _macroInvs -> singleTarget $
          Target (fieldTag declId field.info) (defaultDecl tokens)

instance Simplify C.ImplicitField where
  simplifyIt declId field = concatMap (simplifyIt declId) field.indirect

instance Simplify C.IndirectField where
  simplifyIt declId field = case field.ann of
        ReparseNotNeeded -> nothing
        ReparseNeeded tokens _macroInvs -> singleTarget $
          Target (fieldTag declId field.info) (defaultDecl tokens)

instance Simplify C.Typedef where
  simplifyIt declId typedef = case typedef.ann of
      ReparseNotNeeded -> nothing
      ReparseNeeded tokens _macroInvs -> singleTarget $
        Target (typedefTag declId) (defaultDecl tokens)

instance Simplify C.Enum where
  simplifyIt _ _ = nothing

instance Simplify C.UntaggedEnumConstant where
  simplifyIt _ _ = nothing

instance Simplify (Flip TypecheckedMacro l) where
  simplifyIt declId (Flip m) = case m of
    MacroType{}  -> singleUndef $ Undef (MacroName (P.name declId ""))
    MacroValue{} -> nothing
    MacroRaw{}   -> nothing

instance Simplify C.Function where
  simplifyIt declId function = case function.ann of
      ReparseNotNeeded -> nothing
      ReparseNeeded tokens _macroInvs -> singleTarget $
        Target (functionTag declId) (functionDecl tokens)

instance Simplify C.Global where
  simplifyIt declId global = case global.ann of
      ReparseNotNeeded -> nothing
      ReparseNeeded tokens _macroInvs -> singleTarget $
        Target (variableTag declId) (defaultDecl tokens)

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

defaultDecl :: [Token SourcePath TokenSpelling] -> Decl
defaultDecl tokens = Decl $ flattenDefault tokens

functionDecl :: [Token SourcePath TokenSpelling] -> Decl
functionDecl tokens = Decl $ flattenFunction tokens

{-------------------------------------------------------------------------------
  Tags
-------------------------------------------------------------------------------}

fieldTag :: C.DeclId -> C.FieldInfo ApplyMacroFallback -> Tag
fieldTag declId fieldInfo = Tag Field (TagName name)
  where
    name = P.name declId  . P.dot . P.fieldName fieldInfo $ ""

functionTag :: C.DeclId -> Tag
functionTag declId = Tag Function (TagName (P.name declId ""))

typedefTag :: C.DeclId -> Tag
typedefTag declId = Tag Typedef (TagName (P.name declId ""))

variableTag :: C.DeclId -> Tag
variableTag declId = Tag Variable (TagName (P.name declId ""))
