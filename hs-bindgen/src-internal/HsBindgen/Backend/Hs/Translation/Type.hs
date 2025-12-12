-- | Translate types (use sites)
--
-- Intended for qualified import
--
-- > import HsBindgen.Backend.Hs.Translation.Type qualified as Type
module HsBindgen.Backend.Hs.Translation.Type (
    topLevel
  , TypeContext(..)
  , inContext
    -- * Foreign types
  , NewtypeMap
  , isBaseForeignType
  , NotForeign
  , isBasicForeignType
  , isBasicForeignTypeE
  , primIsBasicForeignTypeE
  ) where

import Control.Exception
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Generics
import GHC.Stack
import Text.Printf

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

data TypeContext =
    Top     -- ^ Anything else
  | FunArg  -- ^ Function argument
  | FunRes  -- ^ Function result
  | PtrArg  -- ^ Pointer argument
  deriving stock (Show)

topLevel :: HasCallStack => C.Type -> Hs.HsType
topLevel = inContext Top

inContext :: HasCallStack => TypeContext -> C.Type -> Hs.HsType
inContext ctx = go ctx
  where
    go :: TypeContext -> C.Type -> Hs.HsType
    go _ (C.TypeTypedef (C.TypedefRef declId _)) =
        Hs.HsTypRef (C.unsafeDeclIdHaskellName declId)
    go _ (C.TypeRef declId) =
        Hs.HsTypRef (C.unsafeDeclIdHaskellName declId)
    go c C.TypeVoid =
        Hs.HsPrimType (void c)
    go _ (C.TypePrim p) =
        Hs.HsPrimType (primType p)
    go _ (C.TypePointer t)
      -- Use a 'FunPtr' if the type is a function type. We inspect the
      -- /canonical/ type because we want to see through typedefs and type
      -- qualifiers like @const@.
      | C.isCanonicalTypeFunction t
      = Hs.HsFunPtr (go PtrArg t)
      | C.isErasedTypeConstQualified t
      = Hs.HsConstPtr (go PtrArg t)
      | otherwise
      = Hs.HsPtr (go PtrArg t)
    go _ (C.TypeConstArray n ty) =
        Hs.HsConstArray n $ go Top ty
    go _ (C.TypeIncompleteArray ty) =
        Hs.HsIncompleteArray $ go Top ty
    go _ (C.TypeFun xs y) =
        foldr (\x res -> Hs.HsFun (go FunArg x) res) (Hs.HsIO (go FunRes y)) xs
    go _ (C.TypeBlock ty) =
        HsBlock $ go Top ty
    go _ (C.TypeExtBinding ext) =
        Hs.HsExtBinding (C.extHsRef ext) (C.extCSpec ext) (C.extHsSpec ext)
    go c (C.TypeQualified C.TypeQualifierConst ty) =
        go c ty
    go _ (C.TypeComplex p) =
        Hs.HsComplexType (primType p)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

primType :: C.PrimType -> HsPrimType
primType C.PrimBool           = HsPrimCBool
primType (C.PrimIntegral i s) = integralType i s
primType (C.PrimFloating f)   = floatingType f
primType C.PrimPtrDiff        = HsPrimCPtrDiff
primType C.PrimSize           = HsPrimCSize
primType (C.PrimChar sign)    = primChar sign

primChar :: C.PrimSignChar -> HsPrimType
primChar (C.PrimSignImplicit _inferred)  = HsPrimCChar
primChar (C.PrimSignExplicit C.Signed)   = HsPrimCSChar
primChar (C.PrimSignExplicit C.Unsigned) = HsPrimCUChar

-- | Translate @void@
--
-- This only makes sense in non-top-level contexts.
-- (We take special care in macro type parsing to rule out top-level @void@.)
void :: HasCallStack => TypeContext -> HsPrimType
void FunRes = HsPrimUnit
void PtrArg = HsPrimVoid
void c      = panicPure $ "unexpected type void in context " ++ show c

integralType :: C.PrimIntType -> C.PrimSign -> HsPrimType
integralType C.PrimInt      C.Signed   = HsPrimCInt
integralType C.PrimInt      C.Unsigned = HsPrimCUInt
integralType C.PrimShort    C.Signed   = HsPrimCShort
integralType C.PrimShort    C.Unsigned = HsPrimCUShort
integralType C.PrimLong     C.Signed   = HsPrimCLong
integralType C.PrimLong     C.Unsigned = HsPrimCULong
integralType C.PrimLongLong C.Signed   = HsPrimCLLong
integralType C.PrimLongLong C.Unsigned = HsPrimCULLong

floatingType :: C.PrimFloatType -> HsPrimType
floatingType C.PrimFloat  = HsPrimCFloat
floatingType C.PrimDouble = HsPrimCDouble

{-------------------------------------------------------------------------------
  Foreign types
-------------------------------------------------------------------------------}

type NewtypeMap = Map (Hs.Name Hs.NsTypeConstr) HsType

data ToBaseForeignTypeError =
    NoBaseForeignType HsType
  | PrimNoBaseForeignType HsPrimType
  | BindingSpecNoBaseForeignType Hs.ExtRef
  deriving stock (Show, Eq, Generic)
  deriving anyclass Exception

isBaseForeignType ::
     NewtypeMap
  -> HsType
  -> Either ToBaseForeignTypeError HsBaseForeignType
isBaseForeignType ntMap = fmap HsBaseForeignType . go
  where
    go :: HsType -> Either ToBaseForeignTypeError Hs.BaseForeignType
    go t = case t of
        HsPrimType pt -> unHsBaseForeignType <$> primIsBaseForeignType pt
        HsTypRef name
          | Just t' <- Map.lookup name ntMap
          -> unHsBaseForeignType <$> isBaseForeignType ntMap t'
          | otherwise
          -> panicPure $
              printf "toBaseForeignType: can not find a type for %s in the NewtypeMap"
                (show name)
        HsConstArray{} -> throwNotABaseForeignType
        HsIncompleteArray{} -> throwNotABaseForeignType
        HsConstPtr{} -> pure $ Hs.Basic Hs.ConstPtr
        HsPtr{} -> pure $ Hs.Basic Hs.Ptr
        HsFunPtr{} -> pure$ Hs.Basic Hs.FunPtr
        HsIO t' -> Hs.IO <$> go t'
        HsFun s t' -> Hs.FunArrow <$> go s <*> go t'
        HsExtBinding ref _ mHsTypeSpec ->
            case mHsTypeSpec >>= BindingSpec.hsTypeSpecBaseForeignType of
              Nothing -> Left $ BindingSpecNoBaseForeignType ref
              Just t' -> Right $ Hs.Basic t'
        HsByteArray -> throwNotABaseForeignType
        HsSizedByteArray{} -> throwNotABaseForeignType
        HsBlock{} -> go $ HsPtr (HsPrimType HsPrimUnit)
        HsComplexType{} -> throwNotABaseForeignType
        HsStrLit{} -> throwNotABaseForeignType
      where
        throwNotABaseForeignType = Left $ NoBaseForeignType t

primIsBaseForeignType :: HsPrimType -> Either ToBaseForeignTypeError HsBaseForeignType
primIsBaseForeignType t = fmap HsBaseForeignType $ case t of
    HsPrimVoid -> throwPrimNotABaseForeignType
    HsPrimUnit -> pure Hs.Unit
    HsPrimCChar -> pure $ Hs.Basic Hs.CChar
    HsPrimCSChar -> pure $ Hs.Basic Hs.CSChar
    HsPrimCUChar -> pure $ Hs.Basic Hs.CUChar
    HsPrimCInt -> pure $ Hs.Basic Hs.CInt
    HsPrimCUInt -> pure $ Hs.Basic Hs.CUInt
    HsPrimCShort -> pure $ Hs.Basic Hs.CShort
    HsPrimCUShort -> pure $ Hs.Basic Hs.CUShort
    HsPrimCLong -> pure $ Hs.Basic Hs.CLong
    HsPrimCULong -> pure $ Hs.Basic Hs.CULong
    HsPrimCPtrDiff -> pure $ Hs.Basic Hs.CPtrdiff
    HsPrimCSize -> pure $ Hs.Basic Hs.CSize
    HsPrimCLLong -> pure $ Hs.Basic Hs.CLLong
    HsPrimCULLong -> pure $ Hs.Basic Hs.CULLong
    HsPrimCBool -> pure $ Hs.Basic Hs.CBool
    HsPrimCFloat -> pure $ Hs.Basic Hs.CFloat
    HsPrimCDouble -> pure $ Hs.Basic Hs.CDouble
    HsPrimCStringLen -> throwPrimNotABaseForeignType
    HsPrimInt -> pure $ Hs.Basic Hs.Int
    HsPrimWord32 -> pure $ Hs.Basic Hs.Word32
    HsPrimInt32 -> pure $ Hs.Basic Hs.Int32
  where
    throwPrimNotABaseForeignType = Left $ PrimNoBaseForeignType t


data NotForeign =
    NotForeign HsType
  | PrimNotForeign HsPrimType

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = \case
    Left{} -> Nothing
    Right x -> Just x

isBasicForeignType :: NewtypeMap -> HsType -> Maybe HsBasicForeignType
isBasicForeignType ntMap t = rightToMaybe $ isBasicForeignTypeE ntMap t

isBasicForeignTypeE ::
     NewtypeMap
  -> HsType
  -> Either NotForeign HsBasicForeignType
isBasicForeignTypeE ntMap = fmap HsBasicForeignType . go
  where
    go :: HsType -> Either NotForeign Hs.BasicForeignType
    go t = case t of
        HsPrimType pt -> unHsBasicForeignType <$> primIsBasicForeignTypeE pt
        HsTypRef name
          | Just t' <- Map.lookup name ntMap
          -> go t'
          -- If the referenced type is not in the newtype map, then it is
          -- assumed that it is not a basic foreign type.
          | otherwise
          -> no
        HsConstArray{} -> no
        HsIncompleteArray{} -> no
        HsConstPtr{} -> yes Hs.ConstPtr
        HsPtr{} -> yes Hs.Ptr
        HsFunPtr{} -> yes Hs.FunPtr
        HsIO{} -> no
        HsFun{} -> no
        HsExtBinding _ _ mHsTypeSpec ->
            case mHsTypeSpec >>= BindingSpec.hsTypeSpecBaseForeignType of
              Nothing -> no
              Just t' -> yes t'
        HsByteArray -> no
        HsSizedByteArray{} -> no
        HsBlock{} -> go $ HsPtr (HsPrimType HsPrimUnit)
        HsComplexType{} -> no
        HsStrLit{} -> no
      where
        no = Left $ NotForeign t
        yes = pure

primIsBasicForeignTypeE :: HsPrimType -> Either NotForeign HsBasicForeignType
primIsBasicForeignTypeE t = fmap HsBasicForeignType $ case t of
    HsPrimVoid -> no
    HsPrimUnit -> no
    HsPrimCChar -> yes Hs.CChar
    HsPrimCSChar -> yes Hs.CSChar
    HsPrimCUChar -> yes Hs.CUChar
    HsPrimCInt -> yes Hs.CInt
    HsPrimCUInt -> yes Hs.CUInt
    HsPrimCShort -> yes Hs.CShort
    HsPrimCUShort -> yes Hs.CUShort
    HsPrimCLong -> yes Hs.CLong
    HsPrimCULong -> yes Hs.CULong
    HsPrimCPtrDiff -> yes Hs.CPtrdiff
    HsPrimCSize -> yes Hs.CSize
    HsPrimCLLong -> yes Hs.CLLong
    HsPrimCULLong -> yes Hs.CULLong
    HsPrimCBool -> yes Hs.CBool
    HsPrimCFloat -> yes Hs.CFloat
    HsPrimCDouble -> yes Hs.CDouble
    HsPrimCStringLen -> no
    HsPrimInt -> yes Hs.Int
    HsPrimWord32 -> yes Hs.Word32
    HsPrimInt32 -> yes Hs.Int32
  where
    no = Left $ PrimNotForeign t
    yes = pure
