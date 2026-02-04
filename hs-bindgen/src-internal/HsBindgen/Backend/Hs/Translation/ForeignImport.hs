-- | Generate Haskell foreign imports (using the 'HasFFIType' class)
module HsBindgen.Backend.Hs.Translation.ForeignImport (
    FunName (..)
  , FunParam (..)
  , FunRes (..)
  , foreignImportDec
  , foreignImportWrapperDec
  , foreignImportDynamicDec
  ) where

import Data.Function
import DeBruijn (Idx (IZ))
import Optics.Core
import Text.Printf (printf)

import HsBindgen.Runtime.FFIType qualified as FFI

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
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
     C.Sizeofs
  -> FunName
  -> [FunParam]
  -> FunRes
  -> C.DeclName
  -> CallConv
  -> Origin.ForeignImport
  -> Safety
  -> [Hs.Decl]
foreignImportDec sizeofs name params res origName callConv origin safety =
    [ Hs.DeclForeignImport foreignImportDecl
    , Hs.DeclFunction funDecl
    ]
  where
    foreignImportDecl :: Hs.ForeignImportDecl
    foreignImportDecl =  Hs.ForeignImportDecl{
          name       = fiName
        , result     = unsafeToFFI sizeofs res.hsType
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
          & #typ .~ unsafeToFFI sizeofs x.hsParam.typ
          ) params

    fiComment = Just $ HsDoc.uniqueSymbol name.uniqSymbol

    funDecl :: Hs.FunctionDecl
    funDecl = Hs.FunctionDecl
        { name       = fName
        , parameters = fParameters
        , result     = res.hsType
        , body       = EGlobal HasFFIType_fromFFIType `EApp` EFree fiName
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
     C.Sizeofs
  -> FunName
  -> Hs.HsType
  -> Origin.ForeignImport
  -> [Hs.Decl]
foreignImportWrapperDec sizeofs name hsType origin =
    [ Hs.DeclForeignImportWrapper foreignImportWrapperDecl
    , Hs.DeclFunction funDecl
    ]
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
    fiFunType = unsafeToFFI sizeofs hsType

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
        EGlobal HasFFIType_castFunPtrFromFFIType `EApp`
        (EFree fiName `EApp`
        (EGlobal HasFFIType_toFFIType `EApp`
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
     C.Sizeofs
  -> FunName
  -> Hs.HsType
  -> Origin.ForeignImport
  -> [Hs.Decl]
foreignImportDynamicDec sizeofs name hsType origin =
    [ Hs.DeclForeignImportDynamic foreignImportDynamicDecl
    , Hs.DeclFunction funDecl
    ]
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
    fiFunType = unsafeToFFI sizeofs hsType

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
        EGlobal HasFFIType_fromFFIType `EApp`
        (EFree fiName `EApp`
        (EGlobal HasFFIType_castFunPtrToFFIType `EApp`
        EBound IZ
        ))
    fComment = Just $ HsDoc.uniqueSymbol name.uniqSymbol

{-------------------------------------------------------------------------------
  FFI types
-------------------------------------------------------------------------------}

-- NOTE: we might want to change @ForeignImportDecl@, @ForeignImportWrapper@,
-- and @ForeignImportDynamic@ to store @FFI.FFIType@ rather than @HsType@. The
-- upside would be that we enforce statically that a foreign import declaration
-- only uses FFI types. The downside is that it requires quite a bit of (boring)
-- plumbing. For now, the YAGNI principle applies.

unsafeToFFI :: C.Sizeofs -> HsType -> HsType
unsafeToFFI sizeofs ty = case toFFI sizeofs ty of
    Nothing ->
      panicPure $ printf "Type does not have an FFI type: %s" (show ty)
    Just ty' ->
      ty'

toFFI :: C.Sizeofs -> HsType -> Maybe HsType
toFFI sizeofs ty =
      fmap fromFFIType
    $ toFFIType sizeofs ty

-- TODO: after issue #1599 is resolved, we should reconsider whether we want to
-- use @HsType@ as an input here, or @C.Type Final@, or something else.
toFFIType :: C.Sizeofs -> HsType -> Maybe FFI.FFIType
toFFIType sizeofs = go
  where
    no = Nothing
    yes = Just

    go :: HsType -> Maybe FFI.FFIType
    go = \case
      HsPrimType pt -> goPrim pt
      HsTypRef _ t -> t >>= go
      HsConstArray{} -> no
      HsIncompleteArray{} -> no
      HsPtr{} -> yes $ FFI.Basic FFI.Ptr
      HsFunPtr{} -> yes $ FFI.Basic FFI.FunPtr
      HsStablePtr{} -> no
      HsPtrConst{} -> yes $ FFI.Basic FFI.Ptr
      HsIO t' -> FFI.IO <$> go t'
      HsFun s t' -> FFI.FunArrow <$> go s <*> go t'
      HsExtBinding _ _ _ t' -> go t'
      HsByteArray -> no
      HsSizedByteArray{} -> no
      HsBlock{} -> yes $ FFI.Basic FFI.Ptr
      HsComplexType{} -> no
      HsStrLit{} -> no
      HsWithFlam{} -> no
      HsEquivStorable{} -> no

    goPrim :: HsPrimType -> Maybe FFI.FFIType
    goPrim pt = case pt of
        HsPrimVoid -> no
        HsPrimUnit -> yes $ FFI.Unit
        HsPrimCStringLen -> no
        HsPrimCPtrdiff -> no
        HsPrimChar -> yes $ FFI.Basic FFI.Char
        HsPrimInt -> yes $ FFI.Basic FFI.Int
        HsPrimDouble -> yes $ FFI.Basic FFI.Double
        HsPrimFloat -> yes $ FFI.Basic FFI.Float
        HsPrimBool -> yes $ FFI.Basic FFI.Bool
        HsPrimInt8 -> yes $ FFI.Basic FFI.Int8
        HsPrimInt16 -> yes $ FFI.Basic FFI.Int16
        HsPrimInt32 -> yes $ FFI.Basic FFI.Int32
        HsPrimInt64 -> yes $ FFI.Basic FFI.Int64
        HsPrimWord -> yes $ FFI.Basic FFI.Word
        HsPrimWord8 -> yes $ FFI.Basic FFI.Word8
        HsPrimWord16 -> yes $ FFI.Basic FFI.Word16
        HsPrimWord32 -> yes $ FFI.Basic FFI.Word32
        HsPrimWord64 -> yes $ FFI.Basic FFI.Word64
        HsPrimCChar -> yes $ FFI.Basic $ signedType sizeofs.char
        HsPrimCSChar -> yes $ FFI.Basic $ signedType sizeofs.schar
        HsPrimCUChar -> yes $ FFI.Basic $ unsignedType sizeofs.uchar
        HsPrimCShort -> yes $ FFI.Basic $ signedType sizeofs.short
        HsPrimCUShort -> yes $ FFI.Basic $ unsignedType sizeofs.ushort
        HsPrimCInt -> yes $ FFI.Basic $ signedType sizeofs.int
        HsPrimCUInt -> yes $ FFI.Basic $ unsignedType sizeofs.uint
        HsPrimCLong -> yes $ FFI.Basic $ signedType sizeofs.long
        HsPrimCULong -> yes $ FFI.Basic $ unsignedType sizeofs.ulong
        HsPrimCLLong -> yes $ FFI.Basic $ signedType sizeofs.longlong
        HsPrimCULLong -> yes $ FFI.Basic $ unsignedType sizeofs.ulonglong
        HsPrimCBool -> yes $ FFI.Basic $ unsignedType sizeofs.bool
        HsPrimCFloat -> yes $ FFI.Basic FFI.Float
        HsPrimCDouble -> yes $ FFI.Basic FFI.Double

signedType :: C.NumBytes -> FFI.BasicFFIType
signedType = \case
    C.One   -> FFI.Int8
    C.Two   -> FFI.Int16
    C.Four  -> FFI.Int32
    C.Eight -> FFI.Int64

unsignedType :: C.NumBytes -> FFI.BasicFFIType
unsignedType = \case
    C.One   -> FFI.Word8
    C.Two   -> FFI.Word16
    C.Four  -> FFI.Word32
    C.Eight -> FFI.Word64

fromFFIType :: FFI.FFIType -> HsType
fromFFIType = goBase
  where
    prim :: HsPrimType -> HsType
    prim = HsPrimType

    goBase :: FFI.FFIType -> HsType
    goBase t = case t of
        FFI.FunArrow s t' -> goBase s `HsFun` goBase t'
        FFI.Unit -> prim HsPrimUnit
        FFI.IO t' -> HsIO (goBase t')
        FFI.Basic t' -> goBasic t'

    goBasic :: FFI.BasicFFIType -> HsType
    goBasic t = case t of
        FFI.Char -> prim HsPrimChar
        FFI.Int -> prim HsPrimInt
        FFI.Double -> prim HsPrimDouble
        FFI.Float -> prim HsPrimFloat
        FFI.Bool -> prim HsPrimBool
        FFI.Int8 -> prim HsPrimInt8
        FFI.Int16 -> prim HsPrimInt16
        FFI.Int32 -> prim HsPrimInt32
        FFI.Int64 -> prim HsPrimInt64
        FFI.Word -> prim HsPrimWord
        FFI.Word8 -> prim HsPrimWord8
        FFI.Word16 -> prim HsPrimWord16
        FFI.Word32 -> prim HsPrimWord32
        FFI.Word64 -> prim HsPrimWord64
        FFI.Ptr -> HsPtr $ prim HsPrimVoid
        FFI.FunPtr -> HsFunPtr $ prim HsPrimVoid
        FFI.StablePtr -> HsStablePtr $ prim HsPrimVoid
