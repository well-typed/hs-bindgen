-- | Generate Haskell foreign imports (using the
-- 'HsBindgen.Runtime.Support.HasFFIType' class)
module HsBindgen.Backend.Hs.Translation.ForeignImport (
    FunName (..)
  , FunParam (..)
  , FunRes (..)
  , foreignImportDec
  , foreignImportWrapperDec
  , foreignImportDynamicDec
  , ImportFor (..)
  , toFFIType
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
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Errors (panicPure)
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

-- | Info about a function name
data FunName = FunName {
    uniqSymbol :: UniqueSymbol
  }

-- | Info about a function argument
data FunParam = FunParam {
    hsParam :: Hs.FunctionParameter Hs.Type
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
     FunName
  -> [FunParam]
  -> FunRes
  -> C.DeclName
  -> CallConv
  -> Origin.ForeignImport
  -> Safety
  -> [Hs.Decl l]
foreignImportDec name params res origName callConv origin safety =
    [ Hs.DeclForeignImport foreignImportDecl
    , Hs.DeclFunction funDecl
    ]
  where
    foreignImportDecl :: Hs.ForeignImportDecl
    foreignImportDecl =  Hs.ForeignImportDecl{
          name       = fiName
        , result     = unsafeToFFIResType res.hsType
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
          & #typ .~ unsafeToFFI x.hsParam.typ
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
     FunName
  -> Hs.Type
  -> ImportFor
  -> Origin.ForeignImport
  -> [Hs.Decl l]
foreignImportWrapperDec name hsType importFor origin =
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
    fiFunType = unsafeToFFIFunType hsType

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
     FunName
  -> Hs.Type
  -> ImportFor
  -> Origin.ForeignImport
  -> [Hs.Decl l]
foreignImportDynamicDec name hsType importFor origin =
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
    fiFunType = unsafeToFFIFunType hsType

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

unsafeToFFIFunType :: Hs.Type -> Hs.FFIFunType
unsafeToFFIFunType ty = case toFFIFunType ty of
    Nothing ->
      panicPure $ printf "Type does not have an FFI function type: %s" (show ty)
    Just ty' ->
      ty'

toFFIFunType :: Hs.Type -> Maybe Hs.FFIFunType
toFFIFunType = \case
    Hs.TypRef _ t  -> t >>= toFFIFunType
    Hs.Fun arg res -> addArg <$> toFFIType arg <*> toFFIFunType res
    t -> toFFIResType t >>= \res -> pure Hs.FFIFunType {args = [], res = res }
  where
    addArg arg t = Hs.FFIFunType {
          args = arg : t.args
        , res  = t.res
        }

unsafeToFFIResType :: Hs.Type -> Hs.FFIResType
unsafeToFFIResType ty = case toFFIResType ty of
    Nothing ->
      panicPure $ printf "Type does not have an FFI function result type: %s" (show ty)
    Just ty' ->
      ty'

toFFIResType :: Hs.Type -> Maybe Hs.FFIResType
toFFIResType = \case
    Hs.IO (Hs.PrimType Hs.PrimUnit) -> Just Hs.FFIResIOUnit
    Hs.PrimType Hs.PrimUnit         -> Just Hs.FFIResUnit
    Hs.IO t                         -> Hs.FFIResIO <$> toFFIType t
    t                               -> Hs.FFIRes <$> toFFIType t

unsafeToFFI :: Hs.Type -> Hs.FFIType
unsafeToFFI ty = case toFFIType ty of
    Nothing ->
      panicPure $ printf "Type does not have an FFI type: %s" (show ty)
    Just ty' ->
      ty'

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1599>
-- After issue #1599 is resolved, we should reconsider whether we want to
-- use @Hs.Type@ as an input here, or @C.Type Final@, or something else.
toFFIType :: Hs.Type -> Maybe Hs.FFIType
toFFIType = go
  where
    no = Nothing
    yes = Just

    go :: Hs.Type -> Maybe Hs.FFIType
    go = \case
      Hs.PrimType pt          -> goPrim pt
      Hs.TypRef _ t           -> t >>= go
      Hs.ConstArray{}         -> no
      Hs.IncompleteArray{}    -> no
      Hs.PtrArrayElem {}      -> yes Hs.FFIPtrVoid
      Hs.PtrConstArrayElem {} -> yes Hs.FFIPtrVoid
      Hs.Ptr{}                -> yes Hs.FFIPtrVoid
      Hs.FunPtr{}             -> yes Hs.FFIFunPtrVoid
      Hs.PtrConst{}           -> yes Hs.FFIPtrVoid
      Hs.IO{}                 -> no
      Hs.Fun{}                -> no
      Hs.ExtBinding _ref _cSpec hsSpec t' ->
        case BindingSpec.hsSpecFFIType hsSpec of
          -- TODO <https://github.com/well-typed/hs-bindgen/issues/1599>: Ideally we'd warn if a type does not have an FFI type
          Nothing -> go t'
          Just hsFFIType -> pure $ extFFIType hsFFIType
      Hs.ByteArray            -> no
      Hs.SizedByteArray{}     -> no
      Hs.Block{}              -> yes Hs.FFIPtrVoid
      Hs.ComplexType{}        -> no
      Hs.StrLit{}             -> no
      Hs.WithFlam{}           -> no
      Hs.EquivStorable{}      -> no
      Hs.IsStructViaReadRaw{} -> no

    goPrim :: Hs.PrimType -> Maybe Hs.FFIType
    goPrim pt = case pt of
        Hs.PrimVoid    -> no
        Hs.PrimUnit    -> no
        -- We never generate foreign imports with the Haskell-specific @Int@
        -- type. We use @CInt@ or similar instead.
        Hs.PrimInt     -> no
        -- Primitive types that correspond to C keywords
        Hs.PrimCChar   -> yes Hs.FFIPrimCChar
        Hs.PrimCSChar  -> yes Hs.FFIPrimCSChar
        Hs.PrimCUChar  -> yes Hs.FFIPrimCUChar
        Hs.PrimCShort  -> yes Hs.FFIPrimCShort
        Hs.PrimCUShort -> yes Hs.FFIPrimCUShort
        Hs.PrimCInt    -> yes Hs.FFIPrimCInt
        Hs.PrimCUInt   -> yes Hs.FFIPrimCUInt
        Hs.PrimCLong   -> yes Hs.FFIPrimCLong
        Hs.PrimCULong  -> yes Hs.FFIPrimCULong
        Hs.PrimCLLong  -> yes Hs.FFIPrimCLLong
        Hs.PrimCULLong -> yes Hs.FFIPrimCULLong
        Hs.PrimCBool   -> yes Hs.FFIPrimCBool
        Hs.PrimCFloat  -> yes Hs.FFIPrimCFloat
        Hs.PrimCDouble -> yes Hs.FFIPrimCDouble

    extFFIType :: BindingSpec.HsFFIType -> Hs.FFIType
    extFFIType hsFFIType =  Hs.FFIExternal hsFFIType.unwrap
