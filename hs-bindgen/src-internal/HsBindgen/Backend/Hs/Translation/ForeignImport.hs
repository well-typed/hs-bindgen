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

import HsBindgen.Runtime.Support.FFIType qualified as FFI

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
unsafeToFFI sizeofs ty = case toFFI sizeofs ty of
    Nothing ->
      panicPure $ printf "Type does not have an FFI type: %s" (show ty)
    Just ty' ->
      ty'

toFFI :: C.Sizeofs -> Hs.Type -> Maybe Hs.Type
toFFI sizeofs ty =
      fmap fromFFIType
    $ toFFIType sizeofs ty

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1599>
-- After issue #1599 is resolved, we should reconsider whether we want to
-- use @Hs.Type@ as an input here, or @C.Type Final@, or something else.
toFFIType :: C.Sizeofs -> Hs.Type -> Maybe FFI.FFIType
toFFIType sizeofs = go
  where
    no = Nothing
    yes = Just

    go :: Hs.Type -> Maybe FFI.FFIType
    go = \case
      Hs.PrimType pt          -> goPrim pt
      Hs.TypRef _ t           -> t >>= go
      Hs.ConstArray{}         -> no
      Hs.IncompleteArray{}    -> no
      Hs.PtrArrayElem {}      -> yes $ FFI.Basic FFI.Ptr
      Hs.PtrConstArrayElem {} -> yes $ FFI.Basic FFI.Ptr
      Hs.Ptr{}                -> yes $ FFI.Basic FFI.Ptr
      Hs.FunPtr{}             -> yes $ FFI.Basic FFI.FunPtr
      Hs.StablePtr{}          -> no
      Hs.PtrConst{}           -> yes $ FFI.Basic FFI.Ptr
      Hs.IO t'                -> FFI.IO <$> go t'
      Hs.Fun s t'             -> FFI.FunArrow <$> go s <*> go t'
      Hs.ExtBinding _ _ _ t'  -> go t'
      Hs.ByteArray            -> no
      Hs.SizedByteArray{}     -> no
      Hs.Block{}              -> yes $ FFI.Basic FFI.Ptr
      Hs.ComplexType{}        -> no
      Hs.StrLit{}             -> no
      Hs.WithFlam{}           -> no
      Hs.EquivStorable{}      -> no

    goPrim :: Hs.PrimType -> Maybe FFI.FFIType
    goPrim pt = case pt of
        Hs.PrimVoid    -> no
        Hs.PrimUnit    -> yes $ FFI.Unit
        Hs.PrimChar    -> yes $ FFI.Basic FFI.Char
        Hs.PrimInt     -> yes $ FFI.Basic FFI.Int
        Hs.PrimDouble  -> yes $ FFI.Basic FFI.Double
        Hs.PrimFloat   -> yes $ FFI.Basic FFI.Float
        Hs.PrimBool    -> yes $ FFI.Basic FFI.Bool
        Hs.PrimInt8    -> yes $ FFI.Basic FFI.Int8
        Hs.PrimInt16   -> yes $ FFI.Basic FFI.Int16
        Hs.PrimInt32   -> yes $ FFI.Basic FFI.Int32
        Hs.PrimInt64   -> yes $ FFI.Basic FFI.Int64
        Hs.PrimWord    -> yes $ FFI.Basic FFI.Word
        Hs.PrimWord8   -> yes $ FFI.Basic FFI.Word8
        Hs.PrimWord16  -> yes $ FFI.Basic FFI.Word16
        Hs.PrimWord32  -> yes $ FFI.Basic FFI.Word32
        Hs.PrimWord64  -> yes $ FFI.Basic FFI.Word64
        Hs.PrimCChar   -> yes $ FFI.Basic $ signedType sizeofs.char
        Hs.PrimCSChar  -> yes $ FFI.Basic $ signedType sizeofs.schar
        Hs.PrimCUChar  -> yes $ FFI.Basic $ unsignedType sizeofs.uchar
        Hs.PrimCShort  -> yes $ FFI.Basic $ signedType sizeofs.short
        Hs.PrimCUShort -> yes $ FFI.Basic $ unsignedType sizeofs.ushort
        Hs.PrimCInt    -> yes $ FFI.Basic $ signedType sizeofs.int
        Hs.PrimCUInt   -> yes $ FFI.Basic $ unsignedType sizeofs.uint
        Hs.PrimCLong   -> yes $ FFI.Basic $ signedType sizeofs.long
        Hs.PrimCULong  -> yes $ FFI.Basic $ unsignedType sizeofs.ulong
        Hs.PrimCLLong  -> yes $ FFI.Basic $ signedType sizeofs.longlong
        Hs.PrimCULLong -> yes $ FFI.Basic $ unsignedType sizeofs.ulonglong
        Hs.PrimCBool   -> yes $ FFI.Basic $ unsignedType sizeofs.bool
        Hs.PrimCFloat  -> yes $ FFI.Basic FFI.Float
        Hs.PrimCDouble -> yes $ FFI.Basic FFI.Double

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

fromFFIType :: FFI.FFIType -> Hs.Type
fromFFIType = goBase
  where
    prim :: Hs.PrimType -> Hs.Type
    prim = Hs.PrimType

    goBase :: FFI.FFIType -> Hs.Type
    goBase t = case t of
        FFI.FunArrow s t' -> goBase s `Hs.Fun` goBase t'
        FFI.Unit          -> prim Hs.PrimUnit
        FFI.IO t'         -> Hs.IO (goBase t')
        FFI.Basic t'      -> goBasic t'

    goBasic :: FFI.BasicFFIType -> Hs.Type
    goBasic t = case t of
        FFI.Char      -> prim Hs.PrimChar
        FFI.Int       -> prim Hs.PrimInt
        FFI.Double    -> prim Hs.PrimDouble
        FFI.Float     -> prim Hs.PrimFloat
        FFI.Bool      -> prim Hs.PrimBool
        FFI.Int8      -> prim Hs.PrimInt8
        FFI.Int16     -> prim Hs.PrimInt16
        FFI.Int32     -> prim Hs.PrimInt32
        FFI.Int64     -> prim Hs.PrimInt64
        FFI.Word      -> prim Hs.PrimWord
        FFI.Word8     -> prim Hs.PrimWord8
        FFI.Word16    -> prim Hs.PrimWord16
        FFI.Word32    -> prim Hs.PrimWord32
        FFI.Word64    -> prim Hs.PrimWord64
        FFI.Ptr       -> Hs.Ptr       $ prim Hs.PrimVoid
        FFI.FunPtr    -> Hs.FunPtr    $ prim Hs.PrimVoid
        FFI.StablePtr -> Hs.StablePtr $ prim Hs.PrimVoid
