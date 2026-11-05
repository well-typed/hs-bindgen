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
  , unsafeToFFIResType
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
import HsBindgen.Frontend.Pass.TranslateTypes.FFIType qualified as FFIType
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
  , ffiType :: Hs.FFIType
  }

-- | Info about the function result
data FunRes = FunRes {
    hsType  :: Hs.Type
  , ffiType :: Hs.FFIResType
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
        , result     = res.ffiType
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
          & #typ .~ x.ffiType
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

-- | The function type that we are generating a @foreign import@ for
--
-- When generating a @foreign import@ declaration, we do that for either a
-- straight function type or a @newtype@ around a function type. We track this
-- information so that we can pass it to @mapToFFI@ or @mapFromFFI@.
--
-- Note: for function @newtype@s we only generate dynamic wrappers and dynamic
-- imports, not regular @foreign import@ declarations.
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

-- | Map a function expression that uses arbitrary Haskell types to a function
-- expression that uses FFI types
--
-- The mapping is type-directed, and the type of the non-FFI-type function is
-- tracked in the 'ImportFor' argument.
--
-- === Example
--
-- Let's say @fun@ is the name of the function we are converting. Then the
-- result of @mapToFFI@ may look something like:
--
-- >  \x0 -> \x1 -> \x2 ->
-- >    fmap toFFIType (fun (fromFFIType x0) (fromFFIType x1) (fromFFIType x2))
--
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


-- | Map a function expression that uses FFI types to a function expression that
-- uses arbitrary Haskell types types
--
-- The mapping is type-directed, and the type of the non-FFI-type function is
-- tracked in the 'ImportFor' argument.
--
-- === Example
--
-- Let's say @fun@ is the name of the function we are converting. Then the
-- result of @mapFromFFI@ may look something like:
--
-- >  \x0 -> \x1 -> \x2 ->
-- >    fmap fromFFIType (fun (toFFIType x0) (toFFIType x1) (toFFIType x2))
--
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

-- | Create a convertion expression that is applied to function arguments.
--
-- This is used to convert function arguments to\/from FFI types.
mkConvArg :: BindgenGlobalTerm -> ConvArg
mkConvArg g =  ConvArg $ \_typ idx ->
    SHs.eBindgenGlobal g  `EApp` SHs.EBound idx

-- | Create a convertion expression that is applied to a function result.
--
-- This is used to convert a function result to\/from an FFI type.
mkConvRes :: BindgenGlobalTerm -> ConvRes
mkConvRes g = ConvRes $ \typ e -> case typ of
    Hs.IO (Hs.PrimType Hs.PrimUnit) -> e
    Hs.PrimType Hs.PrimUnit -> e
    Hs.IO{} -> eBindgenGlobal Functor_fmap `EApp` eBindgenGlobal g `EApp` e
    _ -> eBindgenGlobal g `EApp` e

{-------------------------------------------------------------------------------
  FFI types
-------------------------------------------------------------------------------}

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1599>: compute the FFI
-- types of function types nested in use sites in the @TranslateTypes@ pass.
unsafeToFFIFunType :: Hs.Type -> Hs.FFIFunType
unsafeToFFIFunType ty = case FFIType.toFFIFunType ty of
    Nothing ->
      panicPure $ printf "Type does not have an FFI function type: %s" (show ty)
    Just ty' ->
      ty'

-- | For types synthesised in the backend
unsafeToFFIResType :: Hs.Type -> Hs.FFIResType
unsafeToFFIResType ty = case FFIType.toFFIResType ty of
    Nothing ->
      panicPure $ printf "Type does not have an FFI function result type: %s" (show ty)
    Just ty' ->
      ty'
