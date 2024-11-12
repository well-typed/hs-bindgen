{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Backend.PP (
    -- * HsImport
    HsImport(..)
    -- * NameType
  , NameType(..)
    -- * ResolvedName
  , ResolvedName(..)
    -- * Resolution
  , resolveGlobal
    -- * BackendName
  , BackendName(..)
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
  HsImport
-------------------------------------------------------------------------------}

-- | An import with an optional alias
data HsImport = HsImport {
      hsImportModule :: String
    , hsImportAlias  :: Maybe String
    }
  deriving (Eq, Ord, Show)

-- | @Data.Void@ import
iDataVoid :: HsImport
iDataVoid = HsImport "Data.Void" Nothing

-- | @Foreign@ import
iForeign :: HsImport
iForeign = HsImport "Foreign" (Just "F")

-- | @Foreign.C@ import
iForeignC :: HsImport
iForeignC = HsImport "Foreign.C" (Just "FC")

-- | @Prelude@ import
iPrelude :: HsImport
iPrelude = HsImport "Prelude" (Just "P")

{-------------------------------------------------------------------------------
  NameType
-------------------------------------------------------------------------------}

-- | Name type
data NameType = IdentifierName | OperatorName
  deriving (Eq, Ord, Show)

{-------------------------------------------------------------------------------
  ResolvedName
-------------------------------------------------------------------------------}

-- | Resolved name
data ResolvedName = ResolvedName {
      resolvedNameString  :: String
    , resolvedNameImport  :: HsImport
    , resolvedNameQualify :: Bool
    , resolvedNameType    :: NameType
    }
  deriving (Eq, Ord, Show)

-- | Construct a 'ResolvedName'
mkResolvedName :: Bool -> HsImport -> String -> ResolvedName
mkResolvedName resolvedNameQualify resolvedNameImport resolvedNameString =
    let resolvedNameType
          | all isIdentChar resolvedNameString = IdentifierName
          | otherwise                          = OperatorName
    in  ResolvedName{..}
  where
    isIdentChar :: Char -> Bool
    isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''

{-------------------------------------------------------------------------------
  Resolution
-------------------------------------------------------------------------------}

-- | Resolve a 'Global'
resolveGlobal :: Global -> ResolvedName
resolveGlobal = \case
    -- When adding a new global that resolves to a non-qualified identifier, be
    -- sure to reserve the name in "HsBindgen.Hs.AST.Name".
    Unit_type            -> import_ iPrelude "()"
    Unit_constructor     -> import_ iPrelude "()"
    Applicative_pure     -> import_ iPrelude "pure"
    Applicative_seq      -> import_ iPrelude "<*>"
    Monad_return         -> import_ iPrelude "return"
    Monad_seq            -> import_ iPrelude ">>"
    Storable_Storable    -> importQ iForeign "Storable"
    Storable_sizeOf      -> importQ iForeign "sizeOf"
    Storable_alignment   -> importQ iForeign "alignment"
    Storable_peekByteOff -> importQ iForeign "peekByteOff"
    Storable_pokeByteOff -> importQ iForeign "pokeByteOff"
    Storable_peek        -> importQ iForeign "peek"
    Storable_poke        -> importQ iForeign "poke"
    Foreign_Ptr          -> importQ iForeign "Ptr"
    ConstantArray        -> importQ (HsImport "HsBindgen.ConstantArray" Nothing) "ConstantArray"
    PrimType hsPrimType  -> case hsPrimType of
      HsPrimVoid    -> import_ iDataVoid "Void"
      HsPrimCChar   -> importQ iForeignC "CChar"
      HsPrimCSChar  -> importQ iForeignC "CSChar"
      HsPrimCUChar  -> importQ iForeignC "CUChar"
      HsPrimCInt    -> importQ iForeignC "CInt"
      HsPrimCUInt   -> importQ iForeignC "CUInt"
      HsPrimCShort  -> importQ iForeignC "CShort"
      HsPrimCUShort -> importQ iForeignC "CUShort"
      HsPrimCLong   -> importQ iForeignC "CLong"
      HsPrimCULong  -> importQ iForeignC "CULong"
      HsPrimCLLong  -> importQ iForeignC "CLLong"
      HsPrimCULLong -> importQ iForeignC "CULLong"
      HsPrimCBool   -> importQ iForeignC "CBool"
      HsPrimCFloat  -> importQ iForeignC "CFloat"
      HsPrimCDouble -> importQ iForeignC "CDouble"
  where
    import_, importQ :: HsImport -> String -> ResolvedName
    import_ = mkResolvedName False
    importQ = mkResolvedName True

{-------------------------------------------------------------------------------
  BackendName
-------------------------------------------------------------------------------}

-- | Backend name representation
data BackendName =
    -- | Local (not imported) name
    LocalBackendName NameType String
  | -- | Resolved name
    ResolvedBackendName ResolvedName
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  Backend definition
-------------------------------------------------------------------------------}

data BE = BE

instance BackendRep BE where
  type Name BE = BackendName
  type Expr BE = SExpr BE
  type Decl BE = SDecl BE
  type Ty   BE = SType BE

  resolve BE = ResolvedBackendName . resolveGlobal
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

  fresh _ = \x k -> withFreshName x $
    k . Fresh . LocalBackendName IdentifierName . Text.unpack

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
