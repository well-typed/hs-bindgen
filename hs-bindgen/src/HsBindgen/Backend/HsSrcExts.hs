module HsBindgen.Backend.HsSrcExts (
    BE(..)
    -- * Backend monad
  , M
  , runM
  , GenState(..)
    -- * Annotations
  , Ann -- opaque
  , ann
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Language.Haskell.Exts ()
import Language.Haskell.Exts qualified as E

import HsBindgen.Backend.Common

{-------------------------------------------------------------------------------
  Backend definition
-------------------------------------------------------------------------------}

data BE = BE

instance BackendRep BE where
  type Name BE = E.QName Ann
  type Expr BE = E.Exp   Ann
  type Decl BE = E.Decl  Ann

  resolve _ = \case
      Unit_type            -> prelude "()"
      Unit_constructor     -> prelude "()"
      Applicative_pure     -> prelude "pure"
      Applicative_seq      -> prelude "<*>"
      Monad_return         -> prelude "return"
      Monad_seq            -> prelude ">>"
      Storable_Storable    -> foreignStorable "Storable"
      Storable_sizeOf      -> foreignStorable "sizeOf"
      Storable_alignment   -> foreignStorable "alignment"
      Storable_peekByteOff -> foreignStorable "peekByteOff"
      Storable_pokeByteOff -> foreignStorable "pokeByteOff"
      Storable_peek        -> foreignStorable "peek"
      Storable_poke        -> foreignStorable "poke"

  mkExpr be = \case
      EGlobal n     -> E.Var ann (resolve be n)
      EVar x        -> E.Var ann (getFresh x)
      ECon n        -> E.Con ann (unqualified n)
      EInt i        -> E.Lit ann (E.Int ann (fromIntegral i) (show i))
      EApp f x      -> E.App ann (mkExpr be f) (mkExpr be x)
      EInfix op x y -> E.InfixApp ann
                         (mkExpr be x)
                         (E.QVarOp ann $ resolve be op)
                         (mkExpr be y)
      ELam x f      -> E.Lambda ann
                         [ maybe
                             (E.PWildCard ann)
                             (E.PVar ann . unqual . getFresh)
                             x
                         ]
                         (mkExpr be f)
      ECase x ms    -> E.Case ann (mkExpr be x) [
                           E.Alt ann
                             ( E.PApp ann (unqualified c) $
                                 map (E.PVar ann . unqual . getFresh) xs
                             )
                             (E.UnGuardedRhs ann $ mkExpr be b)
                             Nothing
                         | (c, xs, b) <- ms
                         ]
      EInj x        -> x

  mkDecl be = \case
       DVar x f -> simpleDecl x f
       DInst i  -> E.InstDecl ann Nothing
                     ( E.IRule ann Nothing Nothing $
                         E.IHApp ann
                           (E.IHCon ann $ resolve be $ instanceClass i)
                           (E.TyCon ann $ unqualified $ instanceType i)
                     )
                     ( Just . map (E.InsDecl ann) $
                         map (\(x, f) -> simpleDecl (resolve be x) f) $
                           instanceDecs i
                     )
    where
      simpleDecl :: E.QName Ann -> SExpr BE -> E.Decl Ann
      simpleDecl x f =
          E.FunBind ann [
              E.Match ann (unqual x) []
                (E.UnGuardedRhs ann $ mkExpr be f)
                Nothing
            ]

instance Backend BE where
  newtype M BE a = Gen { unwrapGen :: ReaderT Int (State GenState) a }
    deriving newtype (
        Functor
      , Applicative
      , Monad
      , MonadState GenState
      )

  fresh _ = \x k -> withFreshName x $ k . Fresh . E.UnQual ann . E.Ident ann

{-------------------------------------------------------------------------------
  Generation state
-------------------------------------------------------------------------------}

data GenState = GenState

initGenState :: GenState
initGenState = GenState

{-------------------------------------------------------------------------------
  Monad functionality
-------------------------------------------------------------------------------}

runM :: M BE a -> (a, GenState)
runM = flip runState initGenState . flip runReaderT 0 . unwrapGen

withFreshName :: String -> (String -> M BE a) -> M BE a
withFreshName x k = Gen $ do
    i <- ask
    local succ $ unwrapGen (k (x ++ show i))

{-------------------------------------------------------------------------------
  Syntax tree annotation

  Not sure if we'll need these. For now this is just a placeholder.
-------------------------------------------------------------------------------}

-- | Syntax tree annotation
data Ann = Ann
  deriving stock (Show)

-- | Default annotation
ann :: Ann
ann = Ann

{-------------------------------------------------------------------------------
  Name resolution
-------------------------------------------------------------------------------}

unqualified :: String -> E.QName Ann
unqualified = E.UnQual ann . E.Ident ann

prelude :: String -> E.QName Ann
prelude = unqualified

foreignStorable :: String -> E.QName Ann
foreignStorable = E.Qual ann (E.ModuleName ann "Foreign.Storable") . E.Ident ann

{-------------------------------------------------------------------------------
  Internal auxiliary: @haskell-src-exts@
-------------------------------------------------------------------------------}

unqual :: E.QName Ann -> E.Name Ann
unqual (E.UnQual _ n) = n
unqual n                 = error $ "unqual: unexpected " ++ show n
