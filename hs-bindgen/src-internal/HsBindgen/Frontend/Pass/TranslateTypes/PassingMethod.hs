module HsBindgen.Frontend.Pass.TranslateTypes.PassingMethod (
    Classified
  , classifyResPassingMethod
  , classifyArgPassingMethod
  ) where

import Data.Bifunctor (second)
import Data.Text qualified as Text

import HsBindgen.Frontend.Pass.TranslateTypes.Error
import HsBindgen.Frontend.Pass.TranslateTypes.FFIType qualified as FFIType
import HsBindgen.Frontend.Pass.TranslateTypes.Position
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass.Types (PassTypes (Types))
import HsBindgen.IR.PassingMethod
import HsBindgen.IR.Translation

{-------------------------------------------------------------------------------
  Argument passing method
-------------------------------------------------------------------------------}

-- | The passing method, or why the value cannot be passed
type Classified a = FFIType.M (Either FunPositionError a)

-- | Classify how a function result is passed from C to Haskell
classifyResPassingMethod ::
     (Translation.IsPassCompat p, Types p ~ TranslatedTypes p)
  => C.Type p -- ^ Result type
  -> Hs.Type  -- ^ Translation of the result type
  -> Classified (ResPassingMethod p)
classifyResPassingMethod ty hsType
  -- No value to pass
  | C.isVoid ty
  = pure $ Right PassResVoid
  -- Heap types: the Haskell FFI cannot pass them by value, so they cross it
  -- through a buffer on the Haskell heap
  | isHeapType ty
  = pure $ Right $ PassResNonVoid $ PassSerialized
      (serializedPointer Translation.FunRes ty)
      (C.isErasedTypeConstQualified ty)
  -- Other types
  | otherwise
  = second (PassResNonVoid . PassDirect) <$>
      passDirect FunPositionRes ty hsType

-- | Classify how a function argument is passed from Haskell to C
classifyArgPassingMethod ::
     (Translation.IsPassCompat p, Types p ~ TranslatedTypes p)
  => FunPosition
  -> C.Type p -- ^ Argument type
  -> Hs.Type  -- ^ Translation of the argument type
  -> Classified (PassingMethod p)
classifyArgPassingMethod pos ty hsType
  -- Heap types: the Haskell FFI cannot pass them by value, so they cross it
  -- through a buffer on the Haskell heap
  | isHeapType ty
  = pure $ Right $ PassSerialized
      (serializedPointer Translation.FunArg ty)
      (C.isErasedTypeConstQualified ty)
  -- Other types
  | otherwise
  = second PassDirect <$> passDirect pos ty hsType

-- | Is the type a struct, union or complex type?
isHeapType :: Translation.IsPassCompat p => C.Type p -> Bool
isHeapType ty =
       C.isCanonicalTypeStruct  ty
    || C.isCanonicalTypeUnion   ty
    || C.isCanonicalTypeComplex ty

-- | The pointer to a serialised value
serializedPointer ::
     Translation.IsPassCompat p
  => Translation.TypeContext
  -> C.Type p
  -> TranslatedTypes p
serializedPointer ctx ty = Translated{
      c  = ty'
    , hs = Translation.inContext ctx ty'
    }
  where
    ty' = C.TypePointers 1 ty

passDirect ::
     Translation.IsPassCompat p
  => FunPosition
  -> C.Type p
  -> Hs.Type
  -> Classified Hs.FFIType
passDirect pos cType hsType = do
    mFFIType <-  FFIType.toFFITypeWithMsgs hsType
    pure $ case mFFIType of
      Just ffiType -> Right ffiType
      Nothing      -> Left $ FunPositionNoFFIType NoFFIType{
          position = pos
        , cType    = renderCType cType
        }

renderCType :: Translation.IsPassCompat p => C.Type p -> Text.Text
renderCType cType = Text.strip $ Text.pack $ C.showsType (const id) cType ""
