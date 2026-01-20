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
import Data.Map.Strict qualified as Map
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
import HsBindgen.Backend.Hs.Translation.Type (NewtypeMap)
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.UniqueSymbol (UniqueSymbol (..))
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec.V1
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
  -> FunName
  -> [FunParam]
  -> FunRes
  -> C.DeclName
  -> CallConv
  -> Origin.ForeignImport
  -> Safety
  -> [Hs.Decl]
foreignImportDec transState name params res origName callConv origin safety
  | willForeignImportTypecheck transState funType
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
        , result     = unsafeToBase transState.newtypeMap res.hsType
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
          & #typ .~ unsafeToBase transState.newtypeMap x.hsParam.typ
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
  -> FunName
  -> Hs.HsType
  -> Origin.ForeignImport
  -> [Hs.Decl]
foreignImportWrapperDec transState name hsType origin
  | willForeignImportTypecheck transState hsType
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
    fiFunType = unsafeToBase transState.newtypeMap hsType

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
              name    = Nothing
            , typ     = hsType
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
  -> FunName
  -> Hs.HsType
  -> Origin.ForeignImport
  -> [Hs.Decl]
foreignImportDynamicDec transState name hsType origin
  | willForeignImportTypecheck transState hsType
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
    fiFunType = unsafeToBase transState.newtypeMap hsType

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
              name    = Nothing
            , typ     = Hs.HsFunPtr hsType
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
  -> HsType
  -> Bool
willForeignImportTypecheck transState typ =
       hasInstance_HasBaseForeignType
    && isJust (toBase transState.newtypeMap typ)
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

unsafeToBase :: NewtypeMap -> HsType -> HsType
unsafeToBase ntMap ty = case toBase ntMap ty of
    Nothing ->
      panicPure $ printf "Type does not have base foreign type: %s" (show ty)
    Just ty' ->
      ty'

toBase :: NewtypeMap -> HsType -> Maybe HsType
toBase ntMap ty =
      fmap fromBaseForeignType
    $ toBaseForeignType ntMap ty

toBaseForeignType :: NewtypeMap -> HsType -> Maybe BFT.BaseForeignType
toBaseForeignType ntMap t = go t
  where
    no = Nothing
    yes = Just

    go :: HsType -> Maybe BFT.BaseForeignType
    go = \case
      HsPrimType pt -> goPrim pt
      HsTypRef name
        | Just t' <- Map.lookup name ntMap
        -> go t'
        -- If the referenced type is not in the newtype map, then it is
        -- assumed to be a struct or union
        | otherwise
        -> no
      HsConstArray{} -> no
      HsIncompleteArray{} -> no
      HsPtr{} -> yes $  BFT.Basic BFT.Ptr
      HsFunPtr{} -> yes $  BFT.Basic BFT.FunPtr
      HsStablePtr{} -> yes $  BFT.Basic BFT.StablePtr
      HsConstPtr{} -> yes $  BFT.Builtin BFT.ConstPtr
      HsIO t' -> BFT.IO <$> go t'
      HsFun s t' -> BFT.FunArrow <$> go s <*> go t'
      HsExtBinding extRef _ hsSpec ->
          case BindingSpec.V1.hsSpecFFIType hsSpec of
            Nothing ->
              panicPure $
              printf "toBaseForeignType: no ffitype found for external reference %s"
                    (show extRef)
            Just t' -> case t' of
              BindingSpec.V1.Basic t'' -> yes $ BFT.Basic t''
              BindingSpec.V1.Builtin t'' -> yes $ BFT.Builtin t''
      HsByteArray -> no
      HsSizedByteArray{} -> no
      HsBlock{} -> yes $ BFT.Basic BFT.Ptr
      HsComplexType{} -> no
      HsStrLit{} -> no

    goPrim :: HsPrimType -> Maybe BFT.BaseForeignType
    goPrim pt = case pt of
        HsPrimVoid -> no
        HsPrimUnit -> yes $  BFT.Unit
        HsPrimCStringLen -> no
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
        HsPrimIntPtr -> yes $ BFT.Builtin BFT.IntPtr
        HsPrimWordPtr -> yes $ BFT.Builtin BFT.WordPtr
        HsPrimCChar -> yes $ BFT.Builtin BFT.CChar
        HsPrimCSChar -> yes $ BFT.Builtin BFT.CSChar
        HsPrimCUChar -> yes $ BFT.Builtin BFT.CUChar
        HsPrimCShort -> yes $ BFT.Builtin BFT.CShort
        HsPrimCUShort -> yes $ BFT.Builtin BFT.CUShort
        HsPrimCInt -> yes $ BFT.Builtin BFT.CInt
        HsPrimCUInt -> yes $ BFT.Builtin BFT.CUInt
        HsPrimCLong -> yes $ BFT.Builtin BFT.CLong
        HsPrimCULong -> yes $ BFT.Builtin BFT.CULong
        HsPrimCPtrdiff -> yes $ BFT.Builtin BFT.CPtrdiff
        HsPrimCSize -> yes $ BFT.Builtin BFT.CSize
        HsPrimCWchar -> yes $ BFT.Builtin BFT.CWchar
        HsPrimCSigAtomic -> yes $ BFT.Builtin BFT.CSigAtomic
        HsPrimCLLong -> yes $ BFT.Builtin BFT.CLLong
        HsPrimCULLong -> yes $ BFT.Builtin BFT.CULLong
        HsPrimCBool -> yes $ BFT.Builtin BFT.CBool
        HsPrimCIntPtr -> yes $ BFT.Builtin BFT.CIntPtr
        HsPrimCUIntPtr -> yes $ BFT.Builtin BFT.CUIntPtr
        HsPrimCIntMax -> yes $ BFT.Builtin BFT.CIntMax
        HsPrimCUIntMax -> yes $ BFT.Builtin BFT.CUIntMax
        HsPrimCClock -> yes $ BFT.Builtin BFT.CClock
        HsPrimCTime -> yes $ BFT.Builtin BFT.CTime
        HsPrimCUSeconds -> yes $ BFT.Builtin BFT.CUSeconds
        HsPrimCSUSeconds -> yes $ BFT.Builtin BFT.CSUSeconds
        HsPrimCFloat -> yes $ BFT.Builtin BFT.CFloat
        HsPrimCDouble -> yes $ BFT.Builtin BFT.CDouble

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
        BFT.Builtin t' -> goBuiltin t'

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

    goBuiltin :: BFT.BuiltinForeignType -> HsType
    goBuiltin t = case t of
        BFT.IntPtr -> prim HsPrimIntPtr
        BFT.WordPtr -> prim HsPrimWordPtr
        BFT.ConstPtr -> HsConstPtr $ prim HsPrimVoid
        BFT.CChar -> prim HsPrimCChar
        BFT.CSChar -> prim HsPrimCSChar
        BFT.CUChar -> prim HsPrimCUChar
        BFT.CShort -> prim HsPrimCShort
        BFT.CUShort -> prim HsPrimCUShort
        BFT.CInt -> prim HsPrimCInt
        BFT.CUInt -> prim HsPrimCUInt
        BFT.CLong -> prim HsPrimCLong
        BFT.CULong -> prim HsPrimCULong
        BFT.CPtrdiff -> prim HsPrimCPtrdiff
        BFT.CSize -> prim HsPrimCSize
        BFT.CWchar -> prim HsPrimCWchar
        BFT.CSigAtomic -> prim HsPrimCSigAtomic
        BFT.CLLong -> prim HsPrimCLLong
        BFT.CULLong -> prim HsPrimCULLong
        BFT.CBool -> prim HsPrimCBool
        BFT.CIntPtr -> prim HsPrimCIntPtr
        BFT.CUIntPtr -> prim HsPrimCUIntPtr
        BFT.CIntMax -> prim HsPrimCIntMax
        BFT.CUIntMax -> prim HsPrimCUIntMax
        BFT.CClock -> prim HsPrimCClock
        BFT.CTime -> prim HsPrimCTime
        BFT.CUSeconds -> prim HsPrimCUSeconds
        BFT.CSUSeconds -> prim HsPrimCSUSeconds
        BFT.CFloat -> prim HsPrimCFloat
        BFT.CDouble -> prim HsPrimCDouble
