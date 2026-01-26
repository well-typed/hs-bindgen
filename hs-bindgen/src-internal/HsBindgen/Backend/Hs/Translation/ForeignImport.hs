-- | Generate Haskell foreign imports (using the 'HasBaseForeignType' class)
module HsBindgen.Backend.Hs.Translation.ForeignImport (
    FunName (..)
  , FunParam (..)
  , FunRes (..)
  , foreignImportDec
  , foreignImportWrapperDec
  , foreignImportDynamicDec
  ) where

import Data.Function
import Data.Maybe (isJust)
import Data.Set qualified as Set
import DeBruijn (Idx (IZ))
import Optics.Core
import Text.Printf (printf)

import HsBindgen.Runtime.BaseForeignType qualified as BFT

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.State (TranslationState (..))
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.UniqueSymbol (UniqueSymbol (..))
import HsBindgen.Errors (panicPure)
import HsBindgen.Language.C qualified as C

-- | Info about a function name
data FunName = FunName {
    uniqSymbol :: UniqueSymbol
  }

-- | Info about a function argument
data FunParam = FunParam {
    hsParam :: Hs.FunctionParameter
  }

-- | Info about the function result
data FunRes = FunRes {
    hsType :: HsType
  }

-- | Generate a foreign import
--
-- > foreign import ccall "foo" foo :: CInt -> IO CInt
--
foreignImportDec ::
     TranslationState
  -> C.Sizeofs
  -> FunName
  -> [FunParam]
  -> FunRes
  -> C.DeclName
  -> CallConv
  -> Origin.ForeignImport
  -> Safety
  -> [Hs.Decl]
foreignImportDec transState sizeofs name params res origName callConv origin safety
  | willForeignImportTypecheck transState sizeofs funType
  = [ Hs.DeclForeignImport foreignImportDecl
    , Hs.DeclFunction funDecl
    ]
  | otherwise
  = panicPure "foreignImportDec: generated binding would not compile"
  where
    funType :: HsType
    funType = foldr Hs.HsFun res.hsType (fmap (.hsParam.typ) params)

    foreignImportDecl :: Hs.ForeignImportDecl
    foreignImportDecl =  Hs.ForeignImportDecl{
          name       = fiName
        , result     = unsafeToBase sizeofs res.hsType
        , parameters = fiParameters
        , origName   = origName
        , callConv   = callConv
        , origin     = origin
        , comment    = fiComment
        , safety     = safety
        }
      where

    -- fiName is unique because it is created from a unique name + suffix
    fiName = Hs.InternalName (name.uniqSymbol & #unique %~ (<> "_base"))
    fiParameters = over each (\x ->
        x.hsParam
          & #comment .~ Nothing
          & #typ .~ unsafeToBase sizeofs x.hsParam.typ
          ) params

    fiComment = Just $ HsDoc.uniqueSymbol name.uniqSymbol

    funDecl :: Hs.FunctionDecl
    funDecl = Hs.FunctionDecl
        { name       = fName
        , parameters = fParameters
        , result     = res.hsType
        , body       = EGlobal HasBaseForeignType_fromBaseForeignType `EApp` EFree fiName
        , origin     = origin
        , pragmas    = []
        , comment    = fComment
        }

    -- fName is unique
    fName =  Hs.InternalName name.uniqSymbol
    fParameters = fmap (.hsParam) params
    fComment = Just $ HsDoc.uniqueSymbol name.uniqSymbol

{-------------------------------------------------------------------------------
  Dynamic wrapper
-------------------------------------------------------------------------------}

-- | Generate a so-called dynamic wrapper that turns a Haskell function into a C
-- function pointer.
--
-- > foreign import ccall "wrapper"
-- >   mkCallback :: IO () -> IO (FunPtr (IO ()))
--
-- For more information on this type of wrapper, see section "8.5.1 Standard C
-- Calls" from the "Haskell 2010 Language" report.
--
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1620008.5.1>
--
foreignImportWrapperDec ::
     TranslationState
  -> C.Sizeofs
  -> FunName
  -> Hs.HsType
  -> Origin.ForeignImport
  -> [Hs.Decl]
foreignImportWrapperDec transState sizeofs name hsType origin
  | willForeignImportTypecheck transState sizeofs hsType
  = [ Hs.DeclForeignImportWrapper foreignImportWrapperDecl
    , Hs.DeclFunction funDecl
    ]
  | otherwise
  = panicPure "foreignImportWrapperDec: generated binding would not compile"
  where
    foreignImportWrapperDecl :: Hs.ForeignImportWrapper
    foreignImportWrapperDecl =  Hs.ForeignImportWrapper {
          name    = fiName
        , funType = fiFunType
        , origin  = origin
        , comment = Nothing
        }

    -- fiName is unique because it is created from a unique name + suffix
    fiName = Hs.InternalName (name.uniqSymbol & #unique %~ (<> "_base"))
    fiFunType = unsafeToBase sizeofs hsType

    funDecl :: Hs.FunctionDecl
    funDecl = Hs.FunctionDecl
        { name       = fName
        , parameters = fParameters
        , result     = fResult
        , body       = fBody
        , origin     = origin
        , pragmas    = []
        , comment    = fComment
        }

    -- fName is unique
    fName =  Hs.InternalName name.uniqSymbol
    fParameters = [
          Hs.FunctionParameter{
              typ     = hsType
            , comment = Nothing
            }
        ]
    fResult = Hs.HsIO $ Hs.HsFunPtr hsType
    fBody =
        ELam "fun" $
        EGlobal Functor_fmap `EApp`
        EGlobal HasBaseForeignType_castFunPtrFromBaseForeignType `EApp`
        (EFree fiName `EApp`
        (EGlobal HasBaseForeignType_toBaseForeignType `EApp`
        EBound IZ
        ))
    fComment = Just $ HsDoc.uniqueSymbol name.uniqSymbol

{-------------------------------------------------------------------------------
  Dynamic import
-------------------------------------------------------------------------------}

-- | Generate a so-called dynamic import that turns a C function pointer into a
-- corresponding Haskell function.
--
-- > foreign import ccall "dynamic"
-- >   mkFun :: FunPtr (CInt -> IO ()) -> (CInt -> IO ())
--
-- For more information on this type of wrapper, see section "8.5.1 Standard C
-- Calls" from the "Haskell 2010 Language" report.
--
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1620008.5.1>
--
foreignImportDynamicDec ::
     TranslationState
  -> C.Sizeofs
  -> FunName
  -> Hs.HsType
  -> Origin.ForeignImport
  -> [Hs.Decl]
foreignImportDynamicDec transState sizeofs name hsType origin
  | willForeignImportTypecheck transState sizeofs hsType
  = [ Hs.DeclForeignImportDynamic foreignImportDynamicDecl
    , Hs.DeclFunction funDecl
    ]
  | otherwise
  = panicPure "foreignImportDynamicDec: generated binding would not compile"
  where
    foreignImportDynamicDecl :: Hs.ForeignImportDynamic
    foreignImportDynamicDecl =  Hs.ForeignImportDynamic {
          name    = fiName
        , funType = fiFunType
        , origin  = origin
        , comment = Nothing
        }

    -- fiName is unique because it is created from a unique name + suffix
    fiName = Hs.InternalName (name.uniqSymbol & #unique %~ (<> "_base"))
    fiFunType = unsafeToBase sizeofs hsType

    funDecl :: Hs.FunctionDecl
    funDecl = Hs.FunctionDecl
        { name       = fName
        , parameters = fParameters
        , result     = fResult
        , body       = fBody
        , origin     = origin
        , pragmas    = []
        , comment    = fComment
        }

    -- fName is unique
    fName =  Hs.InternalName name.uniqSymbol
    fParameters = [
          Hs.FunctionParameter{
              typ     = Hs.HsFunPtr hsType
            , comment = Nothing
            }
        ]
    fResult = hsType
    fBody =
        ELam "funPtr" $
        EGlobal HasBaseForeignType_fromBaseForeignType `EApp`
        (EFree fiName `EApp`
        (EGlobal HasBaseForeignType_castFunPtrToBaseForeignType `EApp`
        EBound IZ
        ))
    fComment = Just $ HsDoc.uniqueSymbol name.uniqSymbol

{-------------------------------------------------------------------------------
  Sanity check
-------------------------------------------------------------------------------}

willForeignImportTypecheck ::
     TranslationState
  -> C.Sizeofs
  -> HsType
  -> Bool
willForeignImportTypecheck transState sizeofs typ =
       hasInstance_HasBaseForeignType
    && isJust (toBase sizeofs typ)
  where
    hasInstance_HasBaseForeignType :: Bool
    hasInstance_HasBaseForeignType =
      Hs.HasBaseForeignType `elem`
        Hs.getInstances
          transState.instanceMap
          Nothing
          (Set.singleton Hs.HasBaseForeignType)
          [typ]

{-------------------------------------------------------------------------------
  Base foreign type
-------------------------------------------------------------------------------}

-- NOTE: we might want to change @ForeignImportDecl@, @ForeignImportWrapper@,
-- and @ForeignImportDynamic@ to store @BFT.BaseForeignType@ rather than
-- @HsType@. The upside would be that we enforce statically that a foreign
-- import declaration only uses base foreign types. The downside is that it
-- requires quite a bit of (boring) plumbing. For now, the YAGNI principle
-- applies.

unsafeToBase :: C.Sizeofs -> HsType -> HsType
unsafeToBase sizeofs ty = case toBase sizeofs ty of
    Nothing ->
      panicPure $ printf "Type does not have base foreign type: %s" (show ty)
    Just ty' ->
      ty'

toBase :: C.Sizeofs -> HsType -> Maybe HsType
toBase sizeofs ty =
      fmap fromBaseForeignType
    $ toBaseForeignType sizeofs ty

toBaseForeignType :: C.Sizeofs -> HsType -> Maybe BFT.BaseForeignType
toBaseForeignType sizeofs = go
  where
    no = Nothing
    yes = Just

    go :: HsType -> Maybe BFT.BaseForeignType
    go = \case
      HsPrimType pt -> goPrim pt
      HsTypRef _ t -> t >>= go
      HsConstArray{} -> no
      HsIncompleteArray{} -> no
      HsPtr{} -> yes $ BFT.Basic BFT.Ptr
      HsFunPtr{} -> yes $ BFT.Basic BFT.FunPtr
      HsStablePtr{} -> no
      HsConstPtr{} -> yes $ BFT.Basic BFT.Ptr
      HsIO t' -> BFT.IO <$> go t'
      HsFun s t' -> BFT.FunArrow <$> go s <*> go t'
      HsExtBinding _ _ _ t' -> go t'
      HsByteArray -> no
      HsSizedByteArray{} -> no
      HsBlock{} -> yes $ BFT.Basic BFT.Ptr
      HsComplexType{} -> no
      HsStrLit{} -> no
      HsWithFlexibleArrayMember{} -> no

    goPrim :: HsPrimType -> Maybe BFT.BaseForeignType
    goPrim pt = case pt of
        HsPrimVoid -> no
        HsPrimUnit -> yes $ BFT.Unit
        HsPrimCStringLen -> no
        HsPrimCPtrdiff -> no
        HsPrimChar -> yes $ BFT.Basic BFT.Char
        HsPrimInt -> yes $ BFT.Basic BFT.Int
        HsPrimDouble -> yes $ BFT.Basic BFT.Double
        HsPrimFloat -> yes $ BFT.Basic BFT.Float
        HsPrimBool -> yes $ BFT.Basic BFT.Bool
        HsPrimInt8 -> yes $ BFT.Basic BFT.Int8
        HsPrimInt16 -> yes $ BFT.Basic BFT.Int16
        HsPrimInt32 -> yes $ BFT.Basic BFT.Int32
        HsPrimInt64 -> yes $ BFT.Basic BFT.Int64
        HsPrimWord -> yes $ BFT.Basic BFT.Word
        HsPrimWord8 -> yes $ BFT.Basic BFT.Word8
        HsPrimWord16 -> yes $ BFT.Basic BFT.Word16
        HsPrimWord32 -> yes $ BFT.Basic BFT.Word32
        HsPrimWord64 -> yes $ BFT.Basic BFT.Word64
        HsPrimCChar -> yes $ BFT.Basic $ signedType sizeofs.char
        HsPrimCSChar -> yes $ BFT.Basic $ signedType sizeofs.schar
        HsPrimCUChar -> yes $ BFT.Basic $ unsignedType sizeofs.uchar
        HsPrimCShort -> yes $ BFT.Basic $ signedType sizeofs.short
        HsPrimCUShort -> yes $ BFT.Basic $ unsignedType sizeofs.ushort
        HsPrimCInt -> yes $ BFT.Basic $ signedType sizeofs.int
        HsPrimCUInt -> yes $ BFT.Basic $ unsignedType sizeofs.uint
        HsPrimCLong -> yes $ BFT.Basic $ signedType sizeofs.long
        HsPrimCULong -> yes $ BFT.Basic $ unsignedType sizeofs.ulong
        HsPrimCLLong -> yes $ BFT.Basic $ signedType sizeofs.longlong
        HsPrimCULLong -> yes $ BFT.Basic $ unsignedType sizeofs.ulonglong
        HsPrimCBool -> yes $ BFT.Basic $ unsignedType sizeofs.bool
        HsPrimCFloat -> yes $ BFT.Basic BFT.Float
        HsPrimCDouble -> yes $ BFT.Basic BFT.Double

signedType :: C.NumBytes -> BFT.BasicForeignType
signedType = \case
    C.One   -> BFT.Int8
    C.Two   -> BFT.Int16
    C.Four  -> BFT.Int32
    C.Eight -> BFT.Int64

unsignedType :: C.NumBytes -> BFT.BasicForeignType
unsignedType = \case
    C.One   -> BFT.Word8
    C.Two   -> BFT.Word16
    C.Four  -> BFT.Word32
    C.Eight -> BFT.Word64

fromBaseForeignType :: BFT.BaseForeignType -> HsType
fromBaseForeignType = goBase
  where
    prim :: HsPrimType -> HsType
    prim = HsPrimType

    goBase :: BFT.BaseForeignType -> HsType
    goBase t = case t of
        BFT.FunArrow s t' -> goBase s `HsFun` goBase t'
        BFT.Unit -> prim HsPrimUnit
        BFT.IO t' -> HsIO (goBase t')
        BFT.Basic t' -> goBasic t'

    goBasic :: BFT.BasicForeignType -> HsType
    goBasic t = case t of
        BFT.Char -> prim HsPrimChar
        BFT.Int -> prim HsPrimInt
        BFT.Double -> prim HsPrimDouble
        BFT.Float -> prim HsPrimFloat
        BFT.Bool -> prim HsPrimBool
        BFT.Int8 -> prim HsPrimInt8
        BFT.Int16 -> prim HsPrimInt16
        BFT.Int32 -> prim HsPrimInt32
        BFT.Int64 -> prim HsPrimInt64
        BFT.Word -> prim HsPrimWord
        BFT.Word8 -> prim HsPrimWord8
        BFT.Word16 -> prim HsPrimWord16
        BFT.Word32 -> prim HsPrimWord32
        BFT.Word64 -> prim HsPrimWord64
        BFT.Ptr -> HsPtr $ prim HsPrimVoid
        BFT.FunPtr -> HsFunPtr $ prim HsPrimVoid
        BFT.StablePtr -> HsStablePtr $ prim HsPrimVoid
