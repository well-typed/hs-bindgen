-- | FFI types of Haskell types
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Pass.TranslateTypes.FFIType qualified as FFIType
module HsBindgen.Frontend.Pass.TranslateTypes.FFIType (
    M
  , toFFIType
  , toFFITypeWithMsgs
  , pointerTo
  ) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer.Strict (Writer, censor, runWriter)

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg
import HsBindgen.Frontend.Pass.TranslateTypes.SugarInfo
import HsBindgen.Frontend.Pass.TranslateTypes.Translation (Pointee (..))
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs

type M = ReaderT SugarInfos (Writer [FFITypesMsg])

toFFIType :: SugarInfos -> Hs.Type -> Maybe Hs.FFIType
toFFIType sugars ty = fst $ runWriter $ runReaderT (toFFITypeWithMsgs ty) sugars

toFFITypeWithMsgs ::
     Hs.Type
  -> M (Maybe Hs.FFIType)
toFFITypeWithMsgs = go
  where
    no :: M (Maybe Hs.FFIType)
    no = pure Nothing

    yes :: Hs.FFIType -> M (Maybe Hs.FFIType)
    yes = pure . Just

    go :: Hs.Type -> M (Maybe Hs.FFIType)
    go = \case
      Hs.PrimType pt          -> goPrim pt
      Hs.TypRef ref t         ->
        lookupSugarInfoM ref >>= \case
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

-- | FFI type of a pointer
pointerTo :: Pointee -> Hs.FFIType
pointerTo = \case
    PointeeFunction    -> Hs.FFIFunPtrVoid
    PointeeConstObject -> Hs.FFIPtrVoid
    PointeeObject      -> Hs.FFIPtrVoid

lookupSugarInfoM :: Hs.Identifier -> M SugarInfo
lookupSugarInfoM ident = do
    sugars <- ask
    let sugar = lookupSugarInfo ident sugars
    censor (FFITypesLookupSugarInfo ident sugar :) $ pure sugar
