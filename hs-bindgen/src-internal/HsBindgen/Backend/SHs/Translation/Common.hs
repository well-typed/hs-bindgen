module HsBindgen.Backend.SHs.Translation.Common (
    translateElimStruct
  , toNameHint
  , appMany
  , appManyExpr
  , structCon
  , idiom
  , lambda
  , doAll
  ) where

import Data.Text qualified as T

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint (NameHint (..))

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

translateElimStruct :: (forall ctx'. t ctx' -> SExpr ctx') -> Hs.ElimStruct t ctx -> SExpr ctx
translateElimStruct f (Hs.ElimStruct x struct add k) = ECase
    (EBound x)
    [SAlt (Hs.structConstr struct) add hints (f k)]
  where
    hints = fmap (toNameHint . (.name)) $ Hs.structFields struct

toNameHint :: Hs.Name 'Hs.NsVar -> NameHint
toNameHint = NameHint . T.unpack . Hs.getName

{-------------------------------------------------------------------------------
-  Internal auxiliary: derived functionality
--------------------------------------------------------------------------------}

-- | Apply function to many arguments
appMany :: Global -> [SExpr ctx] -> SExpr ctx
appMany = appManyExpr . EGlobal

appManyExpr :: SExpr ctx -> [SExpr ctx] -> SExpr ctx
appManyExpr = foldl' EApp

-- | Struct constructor
structCon :: Hs.StructCon ctx -> SExpr ctx
structCon (Hs.StructCon s) = ECon (Hs.structConstr s)

-- | Idiom brackets
idiom :: (pure ctx -> SExpr ctx) -> (xs ctx -> SExpr ctx) -> Hs.Ap pure xs ctx -> SExpr ctx
idiom f g (Hs.Ap p xs) = foldl'
    (\ acc x -> EInfix Applicative_seq acc (g x))
    (EApp (EGlobal Applicative_pure) (f p))
    xs

-- | Translate lambda
lambda :: (t (S ctx) -> SExpr (S ctx)) -> Hs.Lambda t ctx -> SExpr ctx
lambda f (Hs.Lambda hint t) = ELam hint (f t)

-- | Monad sequencing
doAll :: (t ctx -> SExpr ctx) -> Hs.Seq t ctx -> SExpr ctx
doAll _ (Hs.Seq []) = EGlobal Monad_return `EApp` EGlobal (Tuple_constructor 0)
doAll f (Hs.Seq ss) = foldr1 (EInfix Monad_seq) (map f ss)
