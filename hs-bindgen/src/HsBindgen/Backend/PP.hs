{-# LANGUAGE OverloadedStrings #-}

module HsBindgen.Backend.PP (
    -- * Resolved Name
    ResolvedName(..)
  , BE(..)
    -- * Backend monad
  , M
  , runM
  , GenState(..)
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Char qualified as Char
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text

import HsBindgen.Backend.Common
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type

{-------------------------------------------------------------------------------
  Backend definition
-------------------------------------------------------------------------------}

data ResolvedName = ResolvedIdent String | ResolvedOperator String
  deriving (Eq, Show)

instance IsString ResolvedName where
  fromString s
      | all isIdentChar s = ResolvedIdent s
      | otherwise         = ResolvedOperator s
    where
      isIdentChar :: Char -> Bool
      isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''

data BE = BE

instance BackendRep BE where
  type Name BE = ResolvedName
  type Expr BE = SExpr BE
  type Decl BE = SDecl BE
  type Ty   BE = SType BE

  resolve BE = \case
    Unit_type            -> resolvePrelude "()"
    Unit_constructor     -> resolvePrelude "()"
    Applicative_pure     -> resolvePrelude "pure"
    Applicative_seq      -> resolvePrelude "<*>"
    Monad_return         -> resolvePrelude "return"
    Monad_seq            -> resolvePrelude ">>"
    Storable_Storable    -> resolveForeign "Storable"
    Storable_sizeOf      -> resolveForeign "sizeOf"
    Storable_alignment   -> resolveForeign "alignment"
    Storable_peekByteOff -> resolveForeign "peekByteOff"
    Storable_pokeByteOff -> resolveForeign "pokeByteOff"
    Storable_peek        -> resolveForeign "peek"
    Storable_poke        -> resolveForeign "poke"
    Foreign_Ptr          -> resolveForeign "Ptr"
    PrimType hsPrimType  -> case hsPrimType of
      HsPrimVoid    -> resolveDataVoid "Void"
      HsPrimCChar   -> resolveForeignC "CChar"
      HsPrimCSChar  -> resolveForeignC "CSChar"
      HsPrimCUChar  -> resolveForeignC "CUChar"
      HsPrimCInt    -> resolveForeignC "CInt"
      HsPrimCUInt   -> resolveForeignC "CUInt"
      HsPrimCShort  -> resolveForeignC "CShort"
      HsPrimCUShort -> resolveForeignC "CUShort"
      HsPrimCLong   -> resolveForeignC "CLong"
      HsPrimCULong  -> resolveForeignC "CULong"
      HsPrimCLLong  -> resolveForeignC "CLLong"
      HsPrimCULLong -> resolveForeignC "CULLong"
      HsPrimCBool   -> resolveForeignC "CBool"
      HsPrimCFloat  -> resolveForeignC "CFloat"
      HsPrimCDouble -> resolveForeignC "CDouble"

  mkExpr BE = id

  mkDecl BE = id

  mkType BE = id

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

{-------------------------------------------------------------------------------
  Name resolution
-------------------------------------------------------------------------------}

qualify :: String -> ResolvedName -> ResolvedName
qualify q = \case
    ResolvedIdent    s -> ResolvedIdent    $ q ++ '.' : s
    ResolvedOperator s -> ResolvedOperator $ q ++ '.' : s

resolveDataVoid :: ResolvedName -> ResolvedName
resolveDataVoid = qualify "Data.Void"

resolveForeign :: ResolvedName -> ResolvedName
resolveForeign = qualify "Foreign"

resolveForeignC :: ResolvedName -> ResolvedName
resolveForeignC = qualify "Foreign.C"

resolvePrelude :: ResolvedName -> ResolvedName
resolvePrelude = id
