{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.HsModule.Pretty.Type (
    prettyType
  ) where

import DeBruijn (Add (..), EmptyCtx, Env (..), lookupEnv)
import Text.SimplePrettyPrint (CtxDoc, Pretty (..), (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.Global
import HsBindgen.Backend.HsModule.Names
import HsBindgen.Backend.HsModule.Pretty.Common
import HsBindgen.Backend.SHs.AST.Type

{-------------------------------------------------------------------------------
  Type pretty-printing
-------------------------------------------------------------------------------}

instance ctx ~ EmptyCtx => Pretty (SType ctx) where
  prettyPrec = prettyType EmptyEnv

prettyType :: Env ctx CtxDoc -> Int -> SType ctx -> CtxDoc
prettyType env prec ty = case ty of
      TGlobal g -> pretty $ resolveGlobal g
      TClass cls -> pretty $ resolveGlobal $ typeClassGlobal cls
      TCon n -> pretty n
      TFree var -> pretty var
      TLit n -> PP.show n
      TStrLit s -> PP.string (show s)
      TExt i _cTypeSpec _hsTypeSpec -> pretty i
      TApps (TBoxedTup n) args ->
        let decls = prettyType env 0 <$> args
        in  prettyBoxedTuple n decls
      TApp c x -> PP.parensWhen (prec > 0) $
        prettyType env 1 c <+> prettyType env 1 x
      TFun a b -> PP.parensWhen (prec > 0) $
        prettyType env 1 a <+> "->" <+> prettyType env 0 b
      TBound x -> lookupEnv x env
      TUnit -> PP.string "()"
      -- Handled in 'prettyType'.
      TBoxedTup n -> prettyBoxedTuple n []
      -- TODO: https://github.com/well-typed/hs-bindgen/issues/1715.
      TEq -> PP.string "(~)"
      TForall hints add ctxt body ->
        case add of
          AZ -> PP.hsep (map (\ ct -> prettyType env 0 ct <+> "=> ") ctxt) >< prettyType env 0 body
          _  -> withFreshNames env add hints $ \env' params ->
            "forall" <+> PP.hsep params >< "." <+>
            PP.hsep (map (\ ct -> prettyType env' 0 ct <+> "=>") ctxt) <+> prettyType env' 0 body
