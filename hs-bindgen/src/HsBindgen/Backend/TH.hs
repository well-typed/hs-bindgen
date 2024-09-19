{-# LANGUAGE TemplateHaskellQuotes #-}

module HsBindgen.Backend.TH (
    BE(..)
  , M
  , runM
  ) where

import Data.Kind (Type)
import Data.Text qualified as Text
import Foreign.Storable qualified
import Language.Haskell.TH qualified as TH

import HsBindgen.Backend.Common

{-------------------------------------------------------------------------------
  Backend definition
-------------------------------------------------------------------------------}

type BE :: (Type -> Type) -> Type
data BE q = BE

instance TH.Quote q => BackendRep (BE q) where
  type Name (BE q) = TH.Name
  type Expr (BE q) = q TH.Exp
  type Decl (BE q) = q TH.Dec

  resolve _ =  \case
      Unit_type            -> ''()
      Unit_constructor     -> '()
      Applicative_pure     -> 'pure
      Applicative_seq      -> '(<*>)
      Monad_return         -> 'return
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
      ECon n        -> TH.conE (TH.mkName $ Text.unpack n)
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
                             ( TH.conP (TH.mkName $ Text.unpack c) $
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
                        $(TH.conT $ TH.mkName $ Text.unpack $ instanceType i)
                    |]
                    ( map (\(x, f) -> simpleDecl (resolve be x) f) $
                        instanceDecs i
                    )
    where
      simpleDecl :: TH.Name -> SExpr (BE q) -> q TH.Dec
      simpleDecl x f = TH.valD (TH.varP x) (TH.normalB $ mkExpr be f) []

instance TH.Quote q => Backend (BE q) where
  newtype M (BE q) a = Gen { unwrapGen :: q a }
    deriving newtype (
        Functor
      , Applicative
      , Monad
      , TH.Quote
      )

  fresh _ = \x k -> TH.newName (Text.unpack x) >>= k . Fresh

{-------------------------------------------------------------------------------
  Monad functionality
-------------------------------------------------------------------------------}

runM :: M (BE q) a -> q a
runM = unwrapGen
