module HsBindgen.Hs.NameMangler.API (
    NameMangler(..)
    -- * Contexts
  , TypeConstrContext(..)
  , ConstrContext(..)
  , VarContext(..)
    -- * Using the name mangler
  , mangleTyconName
  , mangleDataconName
  , mangleDeconName
  , mangleFieldName
  , mangleVarName
  ) where

import HsBindgen.Hs.AST.Name
import HsBindgen.C.AST

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Name mangler functions
data NameMangler = NameMangler {
      -- | Create a Haskell type constructor name
      mangleTypeConstrContext :: TypeConstrContext -> HsName NsTypeConstr

      -- | Create a Haskell constructor name
    , mangleConstrContext :: ConstrContext -> HsName NsConstr

      -- | Create a Haskell variable name
    , mangleVarContext :: VarContext -> HsName NsVar
    }

{-------------------------------------------------------------------------------
  Contexts
-------------------------------------------------------------------------------}

-- | Context for creating Haskell type constructor names
data TypeConstrContext =
    -- | Context for general cases
    TypeConstrContext {
      -- | C name for the type
      ctxTypeConstrCName :: CName
    }
  | -- | Context for structures
    StructTypeConstrContext {
      -- | Structure declaration path
      ctxStructTypeConstrDeclPath :: DeclPath
    }
  deriving stock (Eq, Show)

-- | Context for creating Haskell constructor names
newtype ConstrContext = ConstrContext {
      -- | Type that the constructor is for
      ctxConstrTypeCtx :: TypeConstrContext
    }
  deriving stock (Eq, Show)

-- | Context for creating Haskell variable names
data VarContext =
    -- | Context for general cases
    VarContext {
      -- | C variable name
      ctxVarCName :: CName
    }
  | -- | Context for enumeration fields
    EnumVarContext {
      -- | Enumeration type context
      ctxEnumVarTypeCtx :: TypeConstrContext
    }
  | -- | Context for record fields
    FieldVarContext {
      -- | Record type context
      ctxFieldVarTypeCtx :: TypeConstrContext
    , -- | C field name
      ctxFieldVarCName :: CName
    }
  deriving stock (Eq, Show)


{-------------------------------------------------------------------------------
  Simplified API
-------------------------------------------------------------------------------}

-- | Type context might be whole declaration path (e.g. nested anonymous structs)
class ToTypeConstrContext ctx where
    toCtx :: ctx -> TypeConstrContext

instance ToTypeConstrContext DeclPath where
    toCtx = StructTypeConstrContext

instance ToTypeConstrContext CName where
    toCtx = TypeConstrContext

mangleTyconName :: ToTypeConstrContext ctx => NameMangler -> ctx -> HsName NsTypeConstr
mangleTyconName nm declPath = mangleTypeConstrContext nm (toCtx declPath)

mangleDataconName :: ToTypeConstrContext ctx => NameMangler -> ctx -> HsName NsConstr
mangleDataconName nm declPath = mangleConstrContext nm ctx
  where
    ctx = ConstrContext $ toCtx declPath

mangleFieldName :: NameMangler -> DeclPath -> CName -> HsName NsVar
mangleFieldName nm declPath fname = mangleVarContext nm ctx
  where
    ctx = FieldVarContext (StructTypeConstrContext declPath) fname

-- | Create destructor name, @name Tycon = Datacon { decon :: ... }@
mangleDeconName :: ToTypeConstrContext ctx => NameMangler -> ctx -> HsName NsVar
mangleDeconName nm declPath = mangleVarContext nm ctx
  where
    ctx = EnumVarContext $ toCtx declPath

mangleVarName :: NameMangler -> CName -> HsName NsVar
mangleVarName nm varName = mangleVarContext nm ctx
  where
    ctx = VarContext varName
