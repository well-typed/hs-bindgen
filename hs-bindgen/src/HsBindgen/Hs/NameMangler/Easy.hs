-- | Easy interface for namemangler.
module HsBindgen.Hs.NameMangler.Easy (
    -- * Name mangler
    NM.NameMangler,
    NM.defaultNameMangler,
    -- * Name mangling functions
    mangleTyconName,
    mangleDataconName,
    mangleDeconName,
    mangleFieldName,
    mangleVarName,
) where

import HsBindgen.Hs.NameMangler qualified as NM
import HsBindgen.C.AST.Type (DeclPath)
import HsBindgen.Hs.AST.Name

-- | Type context might be whole declaration path (e.g. nested anonymous structs)
class ToTypeConstrContext ctx where
    toCtx :: ctx -> NM.TypeConstrContext

instance ToTypeConstrContext DeclPath where
    toCtx = NM.StructTypeConstrContext

instance ToTypeConstrContext NM.CName where
    toCtx = NM.TypeConstrContext

mangleTyconName :: ToTypeConstrContext ctx => NM.NameMangler -> ctx -> HsName NsTypeConstr
mangleTyconName nm declPath = NM.mangleTypeConstrContext nm (toCtx declPath)

mangleDataconName :: ToTypeConstrContext ctx => NM.NameMangler -> ctx -> HsName NsConstr
mangleDataconName nm declPath = NM.mangleConstrContext nm ctx
  where
    ctx = NM.ConstrContext $ toCtx declPath

mangleFieldName :: NM.NameMangler -> DeclPath -> NM.CName -> HsName NsVar
mangleFieldName nm declPath fname = NM.mangleVarContext nm ctx
  where
    ctx = NM.FieldVarContext (toCtx declPath) fname

-- | Create destructor name, @name Tycon = Datacon { decon :: ... }@
mangleDeconName :: ToTypeConstrContext ctx => NM.NameMangler -> ctx -> HsName NsVar
mangleDeconName nm declPath = NM.mangleVarContext nm ctx
  where
    ctx = NM.EnumVarContext $ toCtx declPath

mangleVarName :: NM.NameMangler -> NM.CName -> HsName NsVar
mangleVarName nm varName = NM.mangleVarContext nm ctx
  where
    ctx = NM.VarContext varName
