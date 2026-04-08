module HsBindgen.Frontend.Pass.HandleMacros.IsPass (
    HandleMacros
  , HandleMacrosReparseMsg(..)
    -- * Checked macros
  , CheckedMacro(..)
  , CheckedMacroType(..)
  , CheckedMacroExpr(..)
  , MacroEmbedPass
  , CExpr.DSL.XApp(..)
  , CExpr.DSL.XVar(..)
  , macroEmbedPass
  ) where

import C.Expr.Syntax qualified as CExpr.DSL
import C.Expr.Typecheck.Type qualified as CExpr.DSL

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.Error
import HsBindgen.Imports
import HsBindgen.Util.Tracer (WithCallStack)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type HandleMacros :: Pass
data HandleMacros a

-- We do not need the @ReparseInfo@ anymore, so we drop it from the annotations.
type family AnnHandleMacros (ix :: Symbol) :: Star where
  AnnHandleMacros "TranslationUnit" = DeclMeta
  AnnHandleMacros _                 = NoAnn

instance IsPass HandleMacros where
  type MacroBody  HandleMacros = CheckedMacro HandleMacros
  type ExtBinding HandleMacros = Void
  type Ann ix     HandleMacros = AnnHandleMacros ix
  type Msg        HandleMacros = WithCallStack HandleMacrosReparseMsg
  type MacroId    HandleMacros = Id HandleMacros
  macroIdId _ = id

{-------------------------------------------------------------------------------
  Checked macros
-------------------------------------------------------------------------------}

data CheckedMacro p =
    MacroType (CheckedMacroType p)
  | MacroExpr (CheckedMacroExpr p)

data CheckedMacroType p = CheckedMacroType{
      typ :: C.Type p
    , ann :: Ann "CheckedMacroType" p -- for the name mangler
    }

-- | Checked expression (function) macro
data CheckedMacroExpr p = CheckedMacroExpr{
      args :: [CExpr.DSL.Name]
    , body :: CExpr.DSL.MExpr (MacroEmbedPass p)
    , typ  :: CExpr.DSL.Quant (CExpr.DSL.Type CExpr.DSL.Ty)
    }
  deriving stock (Generic)

deriving stock instance IsPass p => Show (CheckedMacro     p)
deriving stock instance IsPass p => Show (CheckedMacroType p)
deriving stock instance IsPass p => Show (CheckedMacroExpr p)

deriving stock instance IsPass p => Eq (CheckedMacro     p)
deriving stock instance IsPass p => Eq (CheckedMacroType p)
deriving stock instance IsPass p => Eq (CheckedMacroExpr p)

{-------------------------------------------------------------------------------
  Bridge between hs-bindgen passes and c-expr passes
-------------------------------------------------------------------------------}

-- | Embed hs-bindgen pass into c-expr pass
type MacroEmbedPass :: Pass -> CExpr.DSL.Pass
data MacroEmbedPass p a

-- | Annotations for function application are not relevant for hs-bindgen
data instance CExpr.DSL.XApp (MacroEmbedPass p) = MacroXApp
  deriving stock (Eq, Show, Generic)

-- | We use annotations on variables to integrate macros with the name mangler
--
-- NOTE: This is not used for local variables (macro arguments), but when
-- macros reference other macros.
data instance CExpr.DSL.XVar (MacroEmbedPass p) = MacroXVar (Id p)
  deriving stock (Generic)

deriving instance IsPass p => Eq   (CExpr.DSL.XVar (MacroEmbedPass p))
deriving instance IsPass p => Show (CExpr.DSL.XVar (MacroEmbedPass p))

macroEmbedPass ::
     CExpr.DSL.MExpr CExpr.DSL.Ps
  -> CExpr.DSL.MExpr (MacroEmbedPass HandleMacros)
macroEmbedPass =
    CExpr.DSL.mapMExpr
      (\CExpr.DSL.NoXApp -> MacroXApp)
      (\CExpr.DSL.NoXVar (CExpr.DSL.Name name) -> MacroXVar $ mkDeclId name)
  where
    mkDeclId :: Text -> DeclId
    mkDeclId macroName = DeclId{
          name = CDeclName{
              text = macroName
            , kind = CNameKindMacro
            }
        , isAnon = False
        }

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance (
      CoercePass CheckedMacroType p p'
    , CoercePass CheckedMacroExpr p p'
    ) => CoercePass CheckedMacro p p' where
  coercePass (MacroType typ)  = MacroType (coercePass typ)
  coercePass (MacroExpr expr) = MacroExpr (coercePass expr)

instance (
      CoercePass C.Type p p'
    , Ann "CheckedMacroType" p ~ Ann "CheckedMacroType" p'
    ) => CoercePass CheckedMacroType p p' where
  coercePass macroType = CheckedMacroType{
        typ = coercePass macroType.typ
      , ann = macroType.ann
      }

instance CoercePassId p p' => CoercePass CheckedMacroExpr p p' where
  coercePass macroExpr = CheckedMacroExpr{
        args = macroExpr.args
      , body = auxBody macroExpr.body
      , typ  = macroExpr.typ
      }
    where
      auxBody ::
           CExpr.DSL.MExpr (MacroEmbedPass p )
        -> CExpr.DSL.MExpr (MacroEmbedPass p')
      auxBody =
          CExpr.DSL.mapMExpr
            (\MacroXApp -> MacroXApp)
            (\(MacroXVar name) _origName -> MacroXVar $
                coercePassId (Proxy @'(p, p')) name
            )

instance CoercePassId ConstructTranslationUnit HandleMacros
instance CoercePassMacroId ConstructTranslationUnit HandleMacros where
  coercePassMacroId _ = absurd

instance CoercePassAnn "TypeFunArg" ConstructTranslationUnit HandleMacros
