{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module HsBindgen.Backend.HsModule.Pretty.Type (
    prettyType
    -- * Precedences
  , funPrec
  , funPrec1
  , eqPrec
  , eqPrec1
  , appPrec
  , appPrec1
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
      TFun a b -> PP.parensWhen (prec > funPrec) $
        prettyType env funPrec1 a <+> "->" <+> prettyType env funPrec b
      TBound x -> lookupEnv x env
      TUnit -> PP.string "()"
      TForall hints add ctxt body ->
        case add of
          AZ -> PP.hsep (map (\ ct -> prettyType env 0 ct <+> "=> ") ctxt) >< prettyType env 0 body
          _  -> withFreshNames env add hints $ \env' params ->
            "forall" <+> PP.hsep params >< "." <+>
            PP.hsep (map (\ ct -> prettyType env' 0 ct <+> "=>") ctxt) <+> prettyType env' 0 body

      --We print tuples without sections if we can
      TApps (TBoxedTup n) args ->
        let decls = prettyType env 0 <$> args
        in  prettyBoxedTuple n decls
      TBoxedTup n -> prettyBoxedTuple n []

      -- When the type operator is fully applied, we print it with infix
      -- notation. If it is not fully applied (or is applied to too many types),
      -- we print the operator with prefix notation. Sections are not supported
      -- for type operators.
      TApps TEq [l, r] ->
        prettyType env eqPrec1 l <+> PP.string "~" <+> prettyType env eqPrec1 r
      TEq -> PP.string "(~)"

      -- This case should be last, so that TApps cases and their alternatives
      -- are matched on first
      TApp c x -> PP.parensWhen (prec > appPrec) $
        prettyType env appPrec c <+> prettyType env appPrec1 x

{-------------------------------------------------------------------------------
  Precedences
-------------------------------------------------------------------------------}

funPrec, funPrec1 :: Int
funPrec = 0
funPrec1 = funPrec + 1

eqPrec, eqPrec1 :: Int
eqPrec = 4
eqPrec1 = eqPrec + 1

appPrec, appPrec1:: Int
appPrec = 10
appPrec1 = appPrec + 1
