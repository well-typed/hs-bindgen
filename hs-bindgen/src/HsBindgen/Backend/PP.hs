{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Backend.PP (
    -- * ResolvedName
    ResolvedName(..)
    -- * QualifiedImport
  , QualifiedImport(..)
  , getExprQualifiedImports
  , getDeclQualifiedImports
  , getTypeQualifiedImports
    -- * Backend definition
  , BE(..)
    -- * Backend monad
  , M
  , runM
  , GenState(..)
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Char qualified as Char
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import HsBindgen.Backend.Common
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type

{-------------------------------------------------------------------------------
  ResolvedName
-------------------------------------------------------------------------------}

data ResolvedName = ResolvedIdent String | ResolvedOperator String
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  QualifiedImport
-------------------------------------------------------------------------------}

data QualifiedImport = QualifiedImport String
  deriving (Eq, Ord, Show)

resolve' :: Global -> (Maybe QualifiedImport, ResolvedName)
resolve' = \case
    Unit_type            -> qualify qiPrelude "()"
    Unit_constructor     -> qualify qiPrelude "()"
    Applicative_pure     -> qualify qiPrelude "pure"
    Applicative_seq      -> qualify qiPrelude "<*>"
    Monad_return         -> qualify qiPrelude "return"
    Monad_seq            -> qualify qiPrelude ">>"
    Storable_Storable    -> qualify qiForeign "Storable"
    Storable_sizeOf      -> qualify qiForeign "sizeOf"
    Storable_alignment   -> qualify qiForeign "alignment"
    Storable_peekByteOff -> qualify qiForeign "peekByteOff"
    Storable_pokeByteOff -> qualify qiForeign "pokeByteOff"
    Storable_peek        -> qualify qiForeign "peek"
    Storable_poke        -> qualify qiForeign "poke"
    Foreign_Ptr          -> qualify qiForeign "Ptr"
    PrimType hsPrimType  -> case hsPrimType of
      HsPrimVoid    -> qualify qiDataVoid "Void"
      HsPrimCChar   -> qualify qiForeignC "CChar"
      HsPrimCSChar  -> qualify qiForeignC "CSChar"
      HsPrimCUChar  -> qualify qiForeignC "CUChar"
      HsPrimCInt    -> qualify qiForeignC "CInt"
      HsPrimCUInt   -> qualify qiForeignC "CUInt"
      HsPrimCShort  -> qualify qiForeignC "CShort"
      HsPrimCUShort -> qualify qiForeignC "CUShort"
      HsPrimCLong   -> qualify qiForeignC "CLong"
      HsPrimCULong  -> qualify qiForeignC "CULong"
      HsPrimCLLong  -> qualify qiForeignC "CLLong"
      HsPrimCULLong -> qualify qiForeignC "CULLong"
      HsPrimCBool   -> qualify qiForeignC "CBool"
      HsPrimCFloat  -> qualify qiForeignC "CFloat"
      HsPrimCDouble -> qualify qiForeignC "CDouble"
  where
    qualify ::
         QualifiedImport
      -> String
      -> (Maybe QualifiedImport, ResolvedName)
    qualify qi@(QualifiedImport q) s
      | all isIdentChar s = (Just qi, ResolvedIdent    (q ++ '.' : s))
      | otherwise         = (Just qi, ResolvedOperator (q ++ '.' : s))

    isIdentChar :: Char -> Bool
    isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''

    qiDataVoid :: QualifiedImport
    qiDataVoid = QualifiedImport "Data.Void"

    qiForeignC :: QualifiedImport
    qiForeignC = QualifiedImport "Foreign.C"

    qiForeign :: QualifiedImport
    qiForeign = QualifiedImport "Foreign"

    qiPrelude :: QualifiedImport
    qiPrelude = QualifiedImport "Prelude"

getGlobalQualifiedImports :: Global -> Set QualifiedImport
getGlobalQualifiedImports = maybe Set.empty Set.singleton . fst . resolve'

getExprQualifiedImports :: SExpr BE -> Set QualifiedImport
getExprQualifiedImports = \case
    EGlobal g -> getGlobalQualifiedImports g
    EVar _x -> Set.empty
    ECon _n -> Set.empty
    EInt _i -> Set.empty
    EApp f x -> getExprQualifiedImports f <> getExprQualifiedImports x
    EInfix op x y ->
      getGlobalQualifiedImports op
        <> getExprQualifiedImports x
        <> getExprQualifiedImports y
    ELam _mPat body -> getExprQualifiedImports body
    ECase x ms -> mconcat $
        getExprQualifiedImports x
      : map (\(_, _, body) -> getExprQualifiedImports body) ms
    EInj x -> getExprQualifiedImports x

getDeclQualifiedImports :: SDecl BE -> Set QualifiedImport
getDeclQualifiedImports = \case
    DVar _name e -> getExprQualifiedImports e
    DInst Instance{..} -> mconcat $
        getGlobalQualifiedImports instanceClass
      : map (getGlobalQualifiedImports . fst) instanceDecs
      ++ map (getExprQualifiedImports . snd) instanceDecs
    DRecord Record{..} -> mconcat $
      map (getTypeQualifiedImports . snd) dataFields
    DNewtype Newtype{..} -> getTypeQualifiedImports newtypeType

getTypeQualifiedImports :: SType BE -> Set QualifiedImport
getTypeQualifiedImports = \case
    TGlobal g -> getGlobalQualifiedImports g
    TCon _n -> Set.empty
    TApp c x -> getTypeQualifiedImports c <> getTypeQualifiedImports x

{-------------------------------------------------------------------------------
  Backend definition
-------------------------------------------------------------------------------}

data BE = BE

instance BackendRep BE where
  type Name BE = ResolvedName
  type Expr BE = SExpr BE
  type Decl BE = SDecl BE
  type Ty   BE = SType BE

  resolve BE = snd . resolve'
  mkExpr  BE = id
  mkDecl  BE = id
  mkType  BE = id

instance Backend BE where
  newtype M BE a = Gen { unwrapGen :: ReaderT Int (State GenState) a }
    deriving newtype (
        Functor
      , Applicative
      , Monad
      , MonadState GenState
      )

  fresh _ = \x k -> withFreshName x $ k . Fresh . ResolvedIdent . Text.unpack

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

withFreshName :: HsName NsVar -> (Text -> M BE a) -> M BE a
withFreshName x k = Gen $ do
    i <- ask
    local succ $ unwrapGen (k (getHsName x <> Text.pack (show i)))
