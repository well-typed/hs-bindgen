module HsBindgen.Backend.PP (
    -- * QualifiedImport
    QualifiedImport(..)
    -- * ResolvedName
  , ResolvedName(..)
  , ResolvedNameType(..)
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
import Data.Text (Text)
import Data.Text qualified as Text

import HsBindgen.Backend.Common
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type

{-------------------------------------------------------------------------------
  QualifiedImport
-------------------------------------------------------------------------------}

-- | Qualified import
--
-- All qualified imports use the full module name.
newtype QualifiedImport = QualifiedImport String
  deriving (Eq, Ord, Show)

-- | @Data.Void@ qualified import
qiDataVoid :: QualifiedImport
qiDataVoid = QualifiedImport "Data.Void"

-- | @Foreign.C@ qualified import
qiForeignC :: QualifiedImport
qiForeignC = QualifiedImport "Foreign.C"

-- | @Foreign@ qualified import
qiForeign :: QualifiedImport
qiForeign = QualifiedImport "Foreign"

-- | @Prelude@ qualified import
qiPrelude :: QualifiedImport
qiPrelude = QualifiedImport "Prelude"

{-------------------------------------------------------------------------------
  ResolvedName
-------------------------------------------------------------------------------}

-- | Resolved name
data ResolvedName = ResolvedName {
      resolvedNameString :: String
    , resolvedNameType :: !ResolvedNameType
    , resolvedNameQualifier :: Maybe QualifiedImport
    }

-- | Type of resolved name
data ResolvedNameType = ResolvedNameIdentifier | ResolvedNameOperator
  deriving (Eq, Show)

-- | Construct a 'ResolvedName'
--
-- This function is used for globals, which are always qualified.
mkResolvedName :: QualifiedImport -> String -> ResolvedName
mkResolvedName qi s = ResolvedName {
      resolvedNameString = s
    , resolvedNameType =
        if all isIdentChar s
          then ResolvedNameIdentifier
          else ResolvedNameOperator
    , resolvedNameQualifier = Just qi
    }
  where
    isIdentChar :: Char -> Bool
    isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''

-- | Construct a unqualified identifier 'ResolvedName'
--
-- This function is used for fresh variables, which are unqualified identifiers.
mkIdentifier :: String -> ResolvedName
mkIdentifier s = ResolvedName {
      resolvedNameString    = s
    , resolvedNameType      = ResolvedNameIdentifier
    , resolvedNameQualifier = Nothing
    }

{-------------------------------------------------------------------------------
  Backend definition
-------------------------------------------------------------------------------}

data BE = BE

instance BackendRep BE where
  type Name BE = ResolvedName
  type Expr BE = SExpr BE
  type Decl BE = SDecl BE
  type Ty   BE = SType BE

  resolve BE = \case
    Unit_type            -> mkResolvedName qiPrelude "()"
    Unit_constructor     -> mkResolvedName qiPrelude "()"
    Applicative_pure     -> mkResolvedName qiPrelude "pure"
    Applicative_seq      -> mkResolvedName qiPrelude "<*>"
    Monad_return         -> mkResolvedName qiPrelude "return"
    Monad_seq            -> mkResolvedName qiPrelude ">>"
    Storable_Storable    -> mkResolvedName qiForeign "Storable"
    Storable_sizeOf      -> mkResolvedName qiForeign "sizeOf"
    Storable_alignment   -> mkResolvedName qiForeign "alignment"
    Storable_peekByteOff -> mkResolvedName qiForeign "peekByteOff"
    Storable_pokeByteOff -> mkResolvedName qiForeign "pokeByteOff"
    Storable_peek        -> mkResolvedName qiForeign "peek"
    Storable_poke        -> mkResolvedName qiForeign "poke"
    Foreign_Ptr          -> mkResolvedName qiForeign "Ptr"
    PrimType hsPrimType  -> case hsPrimType of
      HsPrimVoid    -> mkResolvedName qiDataVoid "Void"
      HsPrimCChar   -> mkResolvedName qiForeignC "CChar"
      HsPrimCSChar  -> mkResolvedName qiForeignC "CSChar"
      HsPrimCUChar  -> mkResolvedName qiForeignC "CUChar"
      HsPrimCInt    -> mkResolvedName qiForeignC "CInt"
      HsPrimCUInt   -> mkResolvedName qiForeignC "CUInt"
      HsPrimCShort  -> mkResolvedName qiForeignC "CShort"
      HsPrimCUShort -> mkResolvedName qiForeignC "CUShort"
      HsPrimCLong   -> mkResolvedName qiForeignC "CLong"
      HsPrimCULong  -> mkResolvedName qiForeignC "CULong"
      HsPrimCLLong  -> mkResolvedName qiForeignC "CLLong"
      HsPrimCULLong -> mkResolvedName qiForeignC "CULLong"
      HsPrimCBool   -> mkResolvedName qiForeignC "CBool"
      HsPrimCFloat  -> mkResolvedName qiForeignC "CFloat"
      HsPrimCDouble -> mkResolvedName qiForeignC "CDouble"

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

  fresh _ = \x k -> withFreshName x $ k . Fresh . mkIdentifier . Text.unpack

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
