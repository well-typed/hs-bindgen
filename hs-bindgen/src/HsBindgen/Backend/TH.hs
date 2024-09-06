{-# LANGUAGE TemplateHaskellQuotes #-}

module HsBindgen.Backend.TH (
    BE(..)
  , M
  , runM
  ) where

import Foreign.Storable qualified
import Language.Haskell.TH (Q)
import Language.Haskell.TH qualified as TH

import HsBindgen.Backend.Common

{-------------------------------------------------------------------------------
  Backend definition
-------------------------------------------------------------------------------}

data BE = BE

instance BackendRep BE where
  type Name BE = TH.Name
  type Expr BE = TH.ExpQ
  type Decl BE = TH.DecQ

  resolve _ =  \case
      Applicative_pure     -> 'pure
      Applicative_seq      -> '(<*>)
      Monad_seq            -> '(>>)
      Storable_Storable    -> ''Foreign.Storable.Storable
      Storable_sizeOf      -> 'Foreign.Storable.sizeOf
      Storable_alignment   -> 'Foreign.Storable.alignment
      Storable_peekByteOff -> 'Foreign.Storable.peekByteOff
      Storable_pokeByteOff -> 'Foreign.Storable.pokeByteOff
      Storable_peek        -> 'Foreign.Storable.peek
      Storable_poke        -> 'Foreign.Storable.poke

  mkExpr be = \case
      EGlobal n     -> TH.varE (resolve be n)
      EVar x        -> TH.varE (getFresh x)
      ECon n        -> TH.conE (TH.mkName n)
      EInt i        -> TH.litE (TH.IntegerL $ fromIntegral i)
      EApp f x      -> TH.appE (mkExpr be f) (mkExpr be x)
      EInfix op x y -> TH.infixE
                         (Just $ mkExpr be x)
                         (TH.varE $ resolve be op)
                         (Just $ mkExpr be y)
      ELam x f      -> TH.lamE
                         [maybe TH.wildP (TH.varP . getFresh) x]
                         (mkExpr be f)
      ECase x ms    -> TH.caseE (mkExpr be x) [
                           TH.match
                             ( TH.conP (TH.mkName c) $
                                 map (TH.varP . getFresh) xs
                             )
                             (TH.normalB $ mkExpr be b)
                             []
                         | (c, xs, b) <- ms
                         ]
      EInj x        -> x

  mkDecl be = \case
      DVar x f -> simpleDecl x f
      DInst i  -> TH.instanceD
                    (return [])
                    [t| $(TH.conT $ resolve be $ instanceClass i)
                        $(TH.conT $ TH.mkName $ instanceType i)
                    |]
                    ( map (\(x, f) -> simpleDecl (resolve be x) f) $
                        instanceDecs i
                    )
    where
      simpleDecl :: TH.Name -> SExpr BE -> TH.DecQ
      simpleDecl x f = TH.valD (TH.varP x) (TH.normalB $ mkExpr be f) []

instance Backend BE where
  newtype M BE a = Gen { unwrapGen :: Q a }
    deriving newtype (
        Functor
      , Applicative
      , Monad
      , TH.Quote
      )

  fresh _ = \x k -> TH.newName x >>= k . Fresh

{-------------------------------------------------------------------------------
  Monad functionality
-------------------------------------------------------------------------------}

runM :: M BE a -> Q a
runM = unwrapGen
