-- | FFI types of Haskell types
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Pass.TranslateTypes.FFIType qualified as FFIType
module HsBindgen.Frontend.Pass.TranslateTypes.FFIType (
    FFITypeError (..)
  , toFFIType
  , toFFIResType
  , toFFIFunType
  , pointerTo
  ) where
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Pass.TranslateTypes.Translation (Pointee (..))
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs

-- | Reasons why we could not derive an FFI type for a haskell type
data FFITypeError =
      -- | A reference to a type did not have an underlying type
      TypeRefNoUnderlyingType { name :: Hs.Name Hs.NsTypeConstr }
      -- | An external binding did not specify an FFI type
    | ExtBindingNoFFIType { extRef :: Hs.ExtRef }
      -- | Any other reason
    | NoFFIType
  deriving stock (Show, Eq)

no :: Either FFITypeError a
no = Left NoFFIType

noUnderlying :: Hs.Name Hs.NsTypeConstr -> Either FFITypeError a
noUnderlying = Left . TypeRefNoUnderlyingType

extNoFFIType :: Hs.ExtRef -> Either FFITypeError a
extNoFFIType = Left . ExtBindingNoFFIType

yes :: a -> Either e a
yes = Right

toFFIType :: Hs.Type -> Either FFITypeError Hs.FFIType
toFFIType = go
  where
    go :: Hs.Type -> Either FFITypeError Hs.FFIType
    go = \case
      Hs.PrimType pt          -> goPrim pt
      Hs.TypRef ref t         -> maybe (noUnderlying ref) go t
      Hs.ConstArray{}         -> no
      Hs.IncompleteArray{}    -> no
      Hs.PtrArrayElem {}      -> yes Hs.FFIPtrVoid
      Hs.PtrConstArrayElem {} -> yes Hs.FFIPtrVoid
      Hs.Ptr{}                -> yes Hs.FFIPtrVoid
      Hs.FunPtr{}             -> yes Hs.FFIFunPtrVoid
      Hs.PtrConst{}           -> yes Hs.FFIPtrVoid
      Hs.IO{}                 -> no
      Hs.Fun{}                -> no
      Hs.ExtBinding ref _cSpec hsSpec ->
        case BindingSpec.hsSpecFFIType hsSpec of
          Just hsFFIType -> yes $ Hs.FFIExternal hsFFIType.unwrap
          Nothing        -> extNoFFIType ref
      Hs.ByteArray            -> no
      Hs.SizedByteArray{}     -> no
      Hs.Block{}              -> yes Hs.FFIPtrVoid
      Hs.ComplexType{}        -> no
      Hs.StrLit{}             -> no
      Hs.WithFlam{}           -> no
      Hs.EquivStorable{}      -> no
      Hs.IsStructViaReadRaw{} -> no

    goPrim :: Hs.PrimType -> Either FFITypeError Hs.FFIType
    goPrim = \case
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

toFFIResType :: Hs.Type -> Either FFITypeError Hs.FFIResType
toFFIResType = \case
    Hs.IO (Hs.PrimType Hs.PrimUnit) -> yes Hs.FFIResIOUnit
    Hs.PrimType Hs.PrimUnit         -> yes Hs.FFIResUnit
    Hs.IO t                         -> Hs.FFIResIO <$> toFFIType t
    t                               -> Hs.FFIRes <$> toFFIType t

toFFIFunType :: Hs.Type -> Either FFITypeError Hs.FFIFunType
toFFIFunType = \case
    Hs.TypRef name t  -> maybe (noUnderlying name) toFFIFunType t
    Hs.Fun arg res -> addArg <$> toFFIType arg <*> toFFIFunType res
    t -> toFFIResType t >>= \res -> pure Hs.FFIFunType {args = [], res = res }
  where
    addArg arg t = Hs.FFIFunType {
          args = arg : t.args
        , res  = t.res
        }

-- | FFI type of a pointer
pointerTo :: Pointee -> Hs.FFIType
pointerTo = \case
    PointeeFunction    -> Hs.FFIFunPtrVoid
    PointeeConstObject -> Hs.FFIPtrVoid
    PointeeObject      -> Hs.FFIPtrVoid
