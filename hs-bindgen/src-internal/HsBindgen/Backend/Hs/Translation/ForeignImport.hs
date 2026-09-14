-- | Generate Haskell foreign imports (using the 'HsBindgen.Runtime.Support.HasFFIType.HasFFIType' class)
module HsBindgen.Backend.Hs.Translation.ForeignImport (
    FunName (..)
  , FunParam (..)
  , FunRes (..)
  , foreignImportDec
  , foreignImportWrapperDec
  , foreignImportDynamicDec
  , ImportFor (..)
  ) where

import Data.Function
import DeBruijn (Idx (IZ), Size (SS, SZ))
import Optics.Core
import Text.Printf (printf)

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.AST.Expr qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Backend.SHs.Translation.MapFunction
import HsBindgen.Backend.UniqueSymbol (UniqueSymbol (..))
import HsBindgen.Errors (panicPure)
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

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
    hsType :: Hs.Type
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
  -> [Hs.Decl l]
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

    fiComment =  Just $ HsDoc.uniqueSymbol name.uniqSymbol

    funDecl :: Hs.FunctionDecl
    funDecl = Hs.FunctionDecl
        { name       = fName
        , parameters = fParameters
        , result     = res.hsType
        , body       = mapFromFFI importFor SZ (SHs.EFree fiName)
        , origin     = origin
        , pragmas    = []
        , comment    = fComment
        }

    importFor :: ImportFor
    importFor = ImportForFunction {
          args = fmap (.hsParam.typ) params
        , res = res.hsType
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
  -> Hs.Type
  -> ImportFor
  -> Origin.ForeignImport
  -> [Hs.Decl l]
foreignImportWrapperDec sizeofs name hsType importFor origin =
    [ Hs.DeclForeignImportWrapper foreignImportWrapperDecl
    , Hs.DeclFunction funDecl
    ]
  where
    foreignImportWrapperDecl :: Hs.ForeignImportWrapper
    foreignImportWrapperDecl =  Hs.ForeignImportWrapper {
          name    = fiName
        , funType = fiFunType
        , origin  = origin
        , comment = Just $ HsDoc.uniqueSymbol fiName
        }

    -- fiName is unique because it is created from a unique name + suffix
    fiName :: UniqueSymbol
    fiName = name.uniqSymbol & #unique %~ (<> "_base")
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
    fResult = Hs.IO $ Hs.FunPtr hsType
    fBody =
        ELam (NameHint "fun") $
        eBindgenGlobal Functor_fmap `EApp`
        eBindgenGlobal Foreign_castFunPtr `EApp`
        (EFree (Hs.InternalName fiName) `EApp` mapToFFI importFor (SS SZ) (EBound IZ))
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
  -> Hs.Type
  -> ImportFor
  -> Origin.ForeignImport
  -> [Hs.Decl l]
foreignImportDynamicDec sizeofs name hsType importFor origin =
    [ Hs.DeclForeignImportDynamic foreignImportDynamicDecl
    , Hs.DeclFunction funDecl
    ]
  where
    foreignImportDynamicDecl :: Hs.ForeignImportDynamic
    foreignImportDynamicDecl =  Hs.ForeignImportDynamic {
          name    = fiName
        , funType = fiFunType
        , origin  = origin
        , comment = Just $ HsDoc.uniqueSymbol fiName
        }

    -- fiName is unique because it is created from a unique name + suffix
    fiName :: UniqueSymbol
    fiName = name.uniqSymbol & #unique %~ (<> "_base")
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
              typ     = Hs.FunPtr hsType
            , comment = Nothing
            }
        ]
    fResult = hsType
    fBody =
        ELam (NameHint "funPtr") $
        (mapFromFFI importFor (SS SZ)
        (EFree (Hs.InternalName fiName) `EApp`
        (eBindgenGlobal Foreign_castFunPtr `EApp`
        EBound IZ
        )))
    fComment = Just $ HsDoc.uniqueSymbol name.uniqSymbol

{-------------------------------------------------------------------------------
  ImportFor
-------------------------------------------------------------------------------}

data ImportFor =
    ImportForFunction {
        args :: [Hs.Type]
      , res  :: Hs.Type
      }
  | ImportForNewtype {
        args   :: [Hs.Type]
      , res    :: Hs.Type
      , newtyp :: Hs.Newtype
      }

mapToFFI :: ImportFor -> Size ctx -> SHs.SExpr ctx -> SHs.SExpr ctx
mapToFFI dynFor size funExpr = case dynFor of
    ImportForFunction args res -> forFunction args res
    ImportForNewtype args res nt -> forNewtype args res nt
  where
    convArg = mkConvArg HasFFIType_fromFFIType
    convRes = mkConvRes HasFFIType_toFFIType

    forFunction args res =
        mapFunctionExpr (MapFunctionParams {
            convArg = convArg
          , convRes = convRes
          , args    = args
          , res     = res
          , funExpr = funExpr
          , size    = size
          })

    forNewtype args res nt =
        mapFunctionExpr (MapFunctionParams {
            convArg = convArg
          , convRes = convRes
          , args    = args
          , res     = res
          , funExpr = eBindgenGlobal HasField_getField `ETypeApp` fieldLit `EApp` funExpr
          , size    = size
          })
      where
        fieldLit = SHs.translateType $ Hs.StrLit $ Hs.nameToStr nt.field.name

mapFromFFI :: ImportFor -> Size ctx -> SHs.SExpr ctx -> SHs.SExpr ctx
mapFromFFI dynFor size funExpr = case dynFor of
    ImportForFunction args res -> forFunction args res
    ImportForNewtype args res nt -> forNewtype args res nt
  where
    convArg = mkConvArg HasFFIType_toFFIType
    convRes = mkConvRes HasFFIType_fromFFIType

    forFunction args res =
        mapFunctionExpr (MapFunctionParams {
            convArg = convArg
          , convRes = convRes
          , args    = args
          , res     = res
          , funExpr = funExpr
          , size    = size
          })

    forNewtype args res nt =
        ECon nt.constr `EApp`
        mapFunctionExpr (MapFunctionParams {
            convArg = convArg
          , convRes = convRes
          , args    = args
          , res     = res
          , funExpr = funExpr
          , size    = size
          })

mkConvArg :: BindgenGlobalTerm -> ConvArg
mkConvArg g =  ConvArg $ \_typ idx -> SHs.eBindgenGlobal g  `EApp` SHs.EBound idx

mkConvRes :: BindgenGlobalTerm -> ConvRes
mkConvRes g = ConvRes $ \typ e -> case typ of
    Hs.IO (Hs.PrimType Hs.PrimUnit) -> e
    Hs.PrimType Hs.PrimUnit -> e
    Hs.IO{} -> eBindgenGlobal Functor_fmap `EApp` eBindgenGlobal g `EApp` e
    _ -> eBindgenGlobal g `EApp` e

{-------------------------------------------------------------------------------
  FFI types
-------------------------------------------------------------------------------}

-- NOTE: we might want to change @ForeignImportDecl@, @ForeignImportWrapper@,
-- and @ForeignImportDynamic@ to store @FFI.FFIType@ rather than @Hs.Type@. The
-- upside would be that we enforce statically that a foreign import declaration
-- only uses FFI types. The downside is that it requires quite a bit of (boring)
-- plumbing. For now, the YAGNI principle applies.

unsafeToFFI :: C.Sizeofs -> Hs.Type -> Hs.Type
unsafeToFFI sizeofs ty = case toFFIType sizeofs ty of
    Nothing ->
      panicPure $ printf "Type does not have an FFI type: %s" (show ty)
    Just ty' ->
      ty'

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1599>
-- After issue #1599 is resolved, we should reconsider whether we want to
-- use @Hs.Type@ as an input here, or @C.Type Final@, or something else.
toFFIType :: C.Sizeofs -> Hs.Type -> Maybe Hs.Type
toFFIType sizeofs = go
  where
    no = Nothing
    yes = Just

    prim :: Hs.PrimType -> Hs.Type
    prim = Hs.PrimType

    go :: Hs.Type -> Maybe Hs.Type
    go = \case
      Hs.PrimType pt          -> Hs.PrimType <$> goPrim pt
      Hs.TypRef _ t           -> t >>= go
      Hs.ConstArray{}         -> no
      Hs.IncompleteArray{}    -> no
      Hs.PtrArrayElem {}      -> yes $ Hs.Ptr $ prim Hs.PrimVoid
      Hs.PtrConstArrayElem {} -> yes $ Hs.Ptr $ prim Hs.PrimVoid
      Hs.Ptr{}                -> yes $ Hs.Ptr $ prim Hs.PrimVoid
      Hs.FunPtr{}             -> yes $ Hs.FunPtr $ prim Hs.PrimVoid
      Hs.StablePtr{}          -> no
      Hs.PtrConst{}           -> yes $ Hs.Ptr $ prim Hs.PrimVoid
      Hs.IO t'                -> Hs.IO <$> go t'
      Hs.Fun s t'             -> Hs.Fun <$> go s <*> go t'
      Hs.ExtBinding _ _ _ t'  -> go t'
      Hs.ByteArray            -> no
      Hs.SizedByteArray{}     -> no
      Hs.Block{}              -> yes $ Hs.Ptr $ prim Hs.PrimVoid
      Hs.ComplexType{}        -> no
      Hs.StrLit{}             -> no
      Hs.WithFlam{}           -> no
      Hs.EquivStorable{}      -> no
      Hs.IsStructViaReadRaw{} -> no

    goPrim :: Hs.PrimType -> Maybe Hs.PrimType
    goPrim pt = case pt of
        Hs.PrimVoid    -> no
        Hs.PrimUnit    -> yesId
        Hs.PrimChar    -> yesId
        Hs.PrimInt     -> yesId
        Hs.PrimDouble  -> yesId
        Hs.PrimFloat   -> yesId
        Hs.PrimBool    -> yesId
        Hs.PrimInt8    -> yesId
        Hs.PrimInt16   -> yesId
        Hs.PrimInt32   -> yesId
        Hs.PrimInt64   -> yesId
        Hs.PrimWord    -> yesId
        Hs.PrimWord8   -> yesId
        Hs.PrimWord16  -> yesId
        Hs.PrimWord32  -> yesId
        Hs.PrimWord64  -> yesId
        Hs.PrimCChar   -> yes $ signedType sizeofs.char
        Hs.PrimCSChar  -> yes $ signedType sizeofs.schar
        Hs.PrimCUChar  -> yes $ unsignedType sizeofs.uchar
        Hs.PrimCShort  -> yes $ signedType sizeofs.short
        Hs.PrimCUShort -> yes $ unsignedType sizeofs.ushort
        Hs.PrimCInt    -> yes $ signedType sizeofs.int
        Hs.PrimCUInt   -> yes $ unsignedType sizeofs.uint
        Hs.PrimCLong   -> yes $ signedType sizeofs.long
        Hs.PrimCULong  -> yes $ unsignedType sizeofs.ulong
        Hs.PrimCLLong  -> yes $ signedType sizeofs.longlong
        Hs.PrimCULLong -> yes $ unsignedType sizeofs.ulonglong
        Hs.PrimCBool   -> yes $ unsignedType sizeofs.bool
        Hs.PrimCFloat  -> yes Hs.PrimFloat
        Hs.PrimCDouble -> yes Hs.PrimDouble
      where yesId = yes pt

signedType :: C.NumBytes -> Hs.PrimType
signedType = \case
    C.One   -> Hs.PrimInt8
    C.Two   -> Hs.PrimInt16
    C.Four  -> Hs.PrimInt32
    C.Eight -> Hs.PrimInt64

unsignedType :: C.NumBytes -> Hs.PrimType
unsignedType = \case
    C.One   -> Hs.PrimWord8
    C.Two   -> Hs.PrimWord16
    C.Four  -> Hs.PrimWord32
    C.Eight -> Hs.PrimWord64
