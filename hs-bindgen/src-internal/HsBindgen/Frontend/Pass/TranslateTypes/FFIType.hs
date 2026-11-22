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



toFFIType :: SugarInfos -> Hs.Type -> Maybe Hs.FFIType
toFFIType sugars = fst . runWriter . toFFITypeWithMsgs sugars

toFFITypeWithMsgs ::
     SugarInfos
  -> Hs.Type
  -> Writer [FFITypesMsg] (Maybe Hs.FFIType)
toFFITypeWithMsgs sugars = go
  where
    no :: Writer [FFITypesMsg] (Maybe Hs.FFIType)
    no = pure Nothing

    yes :: Hs.FFIType -> Writer [FFITypesMsg] (Maybe Hs.FFIType)
    yes = pure . Just

    go :: Hs.Type -> Writer [FFITypesMsg] (Maybe Hs.FFIType)
    go = \case
      Hs.PrimType pt          -> pure $ goPrim pt
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
      Hs.ExtBinding ref _cSpec hsSpec t' ->
        case BindingSpec.hsSpecFFIType hsSpec of
          Just hsFFIType -> yes $ Hs.FFIExternal hsFFIType.unwrap
          Nothing -> do
            mFFIType <- go t'
            censor (maybe id (\ffiType -> (FFITypesExtBindingFallback (ExtBindingFallback ref ffiType) :)) mFFIType) $ pure mFFIType
      Hs.ByteArray            -> no
      Hs.SizedByteArray{}     -> no
      Hs.Block{}              -> yes Hs.FFIPtrVoid
      Hs.ComplexType{}        -> no
      Hs.StrLit{}             -> no
      Hs.WithFlam{}           -> no
      Hs.EquivStorable{}      -> no
      Hs.IsStructViaReadRaw{} -> no

    goPrim :: Hs.PrimType -> Maybe Hs.FFIType
    goPrim = \case
        Hs.PrimVoid    -> Nothing
        Hs.PrimUnit    -> Nothing
        -- We never generate foreign imports with the Haskell-specific @Int@
        -- type. We use @CInt@ or similar instead.
        Hs.PrimInt     -> Nothing
        -- Primitive types that correspond to C keywords
        Hs.PrimCChar   -> Just Hs.FFIPrimCChar
        Hs.PrimCSChar  -> Just Hs.FFIPrimCSChar
        Hs.PrimCUChar  -> Just Hs.FFIPrimCUChar
        Hs.PrimCShort  -> Just Hs.FFIPrimCShort
        Hs.PrimCUShort -> Just Hs.FFIPrimCUShort
        Hs.PrimCInt    -> Just Hs.FFIPrimCInt
        Hs.PrimCUInt   -> Just Hs.FFIPrimCUInt
        Hs.PrimCLong   -> Just Hs.FFIPrimCLong
        Hs.PrimCULong  -> Just Hs.FFIPrimCULong
        Hs.PrimCLLong  -> Just Hs.FFIPrimCLLong
        Hs.PrimCULLong -> Just Hs.FFIPrimCULLong
        Hs.PrimCBool   -> Just Hs.FFIPrimCBool
        Hs.PrimCFloat  -> Just Hs.FFIPrimCFloat
        Hs.PrimCDouble -> Just Hs.FFIPrimCDouble

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
