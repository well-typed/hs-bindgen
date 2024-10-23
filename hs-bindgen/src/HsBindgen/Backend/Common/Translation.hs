{-# LANGUAGE OverloadedStrings #-}

-- | Translation from the Haskell AST to the backend representation
module HsBindgen.Backend.Common.Translation (toBE) where

import Data.Foldable
import Data.Kind
import Data.Vec.Lazy (Vec(..))

import HsBindgen.Backend.Common
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Util.PHOAS

{-------------------------------------------------------------------------------
  Translate to backend-specific type
-------------------------------------------------------------------------------}

class Backend be => ToBE be (a :: PHOAS) where
  type Rep be a :: Type
  type Rep be a = Expr be

  toBE :: be -> a (Fresh be) -> M be (Rep be a)

class    (ToBE be a, Rep be a ~ Expr be) => DefToBE be a
instance (ToBE be a, Rep be a ~ Expr be) => DefToBE be a

{-------------------------------------------------------------------------------
  Variable binding
-------------------------------------------------------------------------------}

instance DefToBE be a => ToBE be (Hs.Lambda a) where
  toBE be (Hs.Lambda k) = fresh be "x" $ \x ->
      lambda be (Just x) <$> toBE be (k x)

instance (DefToBE be a, DefToBE be b) => ToBE be (Hs.Ap a b) where
  toBE be (Hs.Ap f stmts) = idiom' be <$> toBE be f <*> mapM (toBE be) stmts

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

instance Backend be => ToBE be Hs.Decl where
  type Rep be Hs.Decl = Decl be
  toBE be (Hs.DeclData d) = mkDecl be <$> toBE be d
  toBE be (Hs.DeclInstance i) = inst be <$> toBE be i

instance Backend be => ToBE be Hs.InstanceDecl where
  type Rep be Hs.InstanceDecl = Instance be
  toBE be (Hs.InstanceStorable i) = toBE be i

instance Backend be => ToBE be (Hs.WithStruct Hs.DataDecl) where
  type Rep be (Hs.WithStruct Hs.DataDecl) = SDecl be

  toBE _be (Hs.WithStruct struct Hs.MkDataDecl) = do
    return $ DData $ Data
      { dataType = Hs.structName struct
      , dataCon  = Hs.structConstr struct
      , dataFields = map (typeToBE . snd) $ toList $ Hs.structFields struct
      }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

typeToBE :: {- Backend be => -} Hs.HsType -> SType be
typeToBE (Hs.HsPrimType t) = TGlobal (PrimType t)
typeToBE _ = TGlobal Unit_type

{-------------------------------------------------------------------------------
  'Storable'
-------------------------------------------------------------------------------}

instance Backend be => ToBE be (Hs.WithStruct Hs.StorableInstance) where
  type Rep be (Hs.WithStruct Hs.StorableInstance) = Instance be

  toBE be (Hs.WithStruct struct Hs.StorableInstance{
          storableSizeOf
        , storableAlignment
        , storablePeek
        , storablePoke
        }) = do
      peek <- toBE be storablePeek
      poke <- toBE be storablePoke
      return $ Instance {
          instanceClass = Storable_Storable
        , instanceType  = Hs.structName struct
        , instanceDecs  = [
              (Storable_sizeOf    , ELam Nothing $ EInt storableSizeOf)
            , (Storable_alignment , ELam Nothing $ EInt storableAlignment)
            , (Storable_peek      , EInj peek)
            , (Storable_poke      , EInj poke)
            ]
        }

instance Backend be => ToBE be Hs.PeekByteOff where
  toBE be (Hs.PeekByteOff ptr i) = return . mkExpr be $
      appMany Storable_peekByteOff [EVar ptr, EInt i]

instance Backend be => ToBE be Hs.PokeByteOff where
  toBE be (Hs.PokeByteOff ptr i x) = return . mkExpr be $
      appMany Storable_pokeByteOff [EVar ptr, EInt i, EVar x]

{-------------------------------------------------------------------------------
  Statements
-------------------------------------------------------------------------------}

instance DefToBE be a => ToBE be (Hs.Seq a) where
  toBE be (Hs.Seq (List stmts)) = doAll be <$> mapM (toBE be) stmts

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

instance Backend be => ToBE be (Hs.IntroStruct n) where
  toBE be (Hs.IntroStruct struct) = return $
      mkExpr be $ ECon $ Hs.structConstr struct

instance DefToBE be a => ToBE be (Hs.ElimStruct n a) where
  toBE be (Hs.ElimStruct struct k) =
      fresh    be "x"        $ \x ->
      freshVec be fieldNames $ \fs -> do
        k' <- toBE be (k fs)
        return $ mkExpr be $ ELam (Just x) $ ECase (EVar x) [
            (Hs.structConstr struct, toList fs, EInj k')
          ]
    where
      fieldNames :: Vec n (HsName NsVar)
      fieldNames = fst <$> Hs.structFields struct

{-------------------------------------------------------------------------------
  Internal auxiliary: derived functionality
-------------------------------------------------------------------------------}

-- | Apply function to many arguments
appMany :: Global -> [SExpr be] -> SExpr be
appMany = foldl' EApp . EGlobal

-- | Idiom brackets
idiom :: SExpr be -> [SExpr be] -> SExpr be
idiom f = foldl' (EInfix Applicative_seq) (EApp (EGlobal Applicative_pure) f)

-- | Idiom brackets
idiom' :: forall be. Backend be => be -> Expr be -> [Expr be] -> Expr be
idiom' be f = mkExpr be . idiom (EInj f) . map EInj

-- | Construct simple lambda abstraction
lambda :: Backend be => be -> Maybe (Fresh be Bound) -> Expr be -> Expr be
lambda be x = mkExpr be . ELam x . EInj

-- | Simple instance declaration
inst :: Backend be => be -> Instance be -> Decl be
inst be i = mkDecl be $ DInst i

-- | Monad sequencing
doAll :: Backend be => be -> [Expr be] -> Expr be
doAll be [] = mkExpr be $ EGlobal Monad_return `EApp` EGlobal Unit_constructor
doAll be ss = mkExpr be $ foldr1 (EInfix Monad_seq) (map EInj ss)

freshVec ::
     Backend be
  => be
  -> Vec n (HsName NsVar)
  -> (Vec n (Fresh be Bound) -> M be a)
  -> M be a
freshVec  _ VNil       k = k VNil
freshVec be (x ::: xs) k = fresh    be x  $ \v ->
                           freshVec be xs $ \vs ->
                           k (v ::: vs)


