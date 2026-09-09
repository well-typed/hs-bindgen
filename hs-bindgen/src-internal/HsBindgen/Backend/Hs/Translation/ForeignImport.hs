-- | Generate Haskell foreign imports (using the 'HsBindgen.Runtime.Support.HasFFIType.HasFFIType' class)
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

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.UniqueSymbol (UniqueSymbol (..))
import HsBindgen.Errors (panicPure)
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.C qualified as C
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
        , body       = eBindgenGlobal HasFFIType_fromFFIType `EApp` EFree fiName
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
  -> Hs.Type
  -> Origin.ForeignImport
  -> [Hs.Decl l]
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
        eBindgenGlobal HasFFIType_castFunPtrFromFFIType `EApp`
        (EFree (Hs.InternalName fiName) `EApp`
        (eBindgenGlobal HasFFIType_toFFIType `EApp`
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
  -> Hs.Type
  -> Origin.ForeignImport
  -> [Hs.Decl l]
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
        eBindgenGlobal HasFFIType_fromFFIType `EApp`
        (EFree (Hs.InternalName fiName) `EApp`
        (eBindgenGlobal HasFFIType_castFunPtrToFFIType `EApp`
        EBound IZ
        ))
    fComment = Just $ HsDoc.uniqueSymbol name.uniqSymbol

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
