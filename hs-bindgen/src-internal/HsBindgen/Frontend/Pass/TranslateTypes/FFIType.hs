-- | FFI types of Haskell types
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Pass.TranslateTypes.FFIType qualified as FFIType
module HsBindgen.Frontend.Pass.TranslateTypes.FFIType (
    toFFIType
  , toFFITypeWithMsgs
  , toFFIResType
  , toFFIFunType
  , pointerTo
  ) where

import Control.Monad.Writer.Strict (Writer, censor, runWriter)

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg
import HsBindgen.Frontend.Pass.TranslateTypes.SugarInfo
import HsBindgen.Frontend.Pass.TranslateTypes.Translation (Pointee (..))
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs

type M = Writer [FFITypesMsg]

toFFIType :: SugarInfos -> Hs.Type -> Maybe Hs.FFIType
toFFIType sugars = fst . runWriter . toFFITypeWithMsgs sugars

toFFITypeWithMsgs ::
     SugarInfos
  -> Hs.Type
  -> M (Maybe Hs.FFIType)
toFFITypeWithMsgs sugars = go
  where
    no :: M (Maybe Hs.FFIType)
    no = pure Nothing

    yes :: Hs.FFIType -> M (Maybe Hs.FFIType)
    yes = pure . Just

    go :: Hs.Type -> M (Maybe Hs.FFIType)
    go = \case
      Hs.PrimType pt          -> goPrim pt
      Hs.TypRef ref t         ->
        lookupSugarInfoM ref sugars >>= \case
          MissingSugarInfo        -> maybe no go t
          RegSugarInfo NotSugar   -> no
          RegSugarInfo (Sugar t') -> go t'
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
          Nothing        -> censor (FFITypesExtBindingNotSpecified ref :) no
      Hs.ByteArray            -> no
      Hs.SizedByteArray{}     -> no
      Hs.Block{}              -> yes Hs.FFIPtrVoid
      Hs.ComplexType{}        -> no
      Hs.StrLit{}             -> no
      Hs.WithFlam{}           -> no
      Hs.EquivStorable{}      -> no
      Hs.IsStructViaReadRaw{} -> no

    goPrim :: Hs.PrimType -> M (Maybe Hs.FFIType)
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

toFFIResType :: SugarInfos -> Hs.Type -> Maybe Hs.FFIResType
toFFIResType sugars = \case
    Hs.IO (Hs.PrimType Hs.PrimUnit) -> Just Hs.FFIResIOUnit
    Hs.PrimType Hs.PrimUnit         -> Just Hs.FFIResUnit
    Hs.IO t                         -> Hs.FFIResIO <$> toFFIType sugars t
    t                               -> Hs.FFIRes <$> toFFIType sugars t

toFFIFunType :: SugarInfos -> Hs.Type -> Maybe Hs.FFIFunType
toFFIFunType sugars = \case
    Hs.TypRef ref t ->
      case lookupSugarInfo ref sugars of
        MissingSugarInfo -> t >>= toFFIFunType sugars
        RegSugarInfo NotSugar -> Nothing
        RegSugarInfo (Sugar t') -> toFFIFunType sugars t'
    Hs.Fun arg res -> addArg <$> toFFIType sugars arg <*> toFFIFunType sugars res
    t -> toFFIResType sugars t >>= \res -> pure Hs.FFIFunType {args = [], res = res }
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

lookupSugarInfoM :: Hs.Name Hs.NsTypeConstr -> SugarInfos -> Writer [FFITypesMsg] SugarInfo
lookupSugarInfoM name sugars = do
    let sugar = lookupSugarInfo name sugars
    censor (FFITypesLookupSugarInfo name sugar :) $ pure sugar
