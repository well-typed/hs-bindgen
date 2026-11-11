-- | FFI types of Haskell types
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Pass.TranslateTypes.FFIType qualified as FFIType
module HsBindgen.Frontend.Pass.TranslateTypes.FFIType (
    ExtBindingFallback (..)
  , toFFIType
  , toFFITypeWithFallbacks
  , toFFIResType
  , toFFIFunType
  , pointerTo
  ) where

import Control.Monad.Writer.Strict (Writer, runWriter, tell)

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Pass.TranslateTypes.Translation (Pointee (..))
import HsBindgen.Imports
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs

-- | An external binding whose binding specification gives no FFI type
--
-- We assume the FFI type of its underlying type instead. The generated code
-- compiles only if the external type has a @HasFFIType@ instance with that
-- FFI type.
data ExtBindingFallback = ExtBindingFallback {
      extRef  :: Hs.ExtRef
      -- | The FFI type of the underlying type
    , assumed :: Hs.FFIType
    }
  deriving stock (Show, Eq)

toFFIType :: Hs.Type -> Maybe Hs.FFIType
toFFIType = fst . runWriter . toFFITypeWithFallbacks

toFFITypeWithFallbacks ::
     Hs.Type
  -> Writer [ExtBindingFallback] (Maybe Hs.FFIType)
toFFITypeWithFallbacks = go
  where
    no :: Writer [ExtBindingFallback] (Maybe Hs.FFIType)
    no = pure Nothing

    yes :: Hs.FFIType -> Writer [ExtBindingFallback] (Maybe Hs.FFIType)
    yes = pure . Just

    go :: Hs.Type -> Writer [ExtBindingFallback] (Maybe Hs.FFIType)
    go = \case
      Hs.PrimType pt          -> pure $ goPrim pt
      Hs.TypRef _ t           -> maybe no go t
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
            forM_ mFFIType $ \assumed -> tell [ExtBindingFallback ref assumed]
            pure mFFIType
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

toFFIResType :: Hs.Type -> Maybe Hs.FFIResType
toFFIResType = \case
    Hs.IO (Hs.PrimType Hs.PrimUnit) -> Just Hs.FFIResIOUnit
    Hs.PrimType Hs.PrimUnit         -> Just Hs.FFIResUnit
    Hs.IO t                         -> Hs.FFIResIO <$> toFFIType t
    t                               -> Hs.FFIRes <$> toFFIType t

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

-- | FFI type of a pointer
pointerTo :: Pointee -> Hs.FFIType
pointerTo = \case
    PointeeFunction    -> Hs.FFIFunPtrVoid
    PointeeConstObject -> Hs.FFIPtrVoid
    PointeeObject      -> Hs.FFIPtrVoid
