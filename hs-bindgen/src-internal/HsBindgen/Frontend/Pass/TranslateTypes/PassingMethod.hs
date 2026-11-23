module HsBindgen.Frontend.Pass.TranslateTypes.PassingMethod (
    Classified
  , classifyResPassingMethod
  , classifyArgPassingMethod
  ) where

import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Monad.Writer.Strict (MonadTrans (lift), Writer)
import Data.Bifunctor (second)
import Data.Text qualified as Text

import HsBindgen.Frontend.Pass.TranslateTypes.Error
import HsBindgen.Frontend.Pass.TranslateTypes.FFIType qualified as FFIType
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg (FFITypesMsg)
import HsBindgen.Frontend.Pass.TranslateTypes.Position
import HsBindgen.Frontend.Pass.TranslateTypes.SugarInfo (SugarInfos)
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
type Classified a =
    ReaderT SugarInfos (Writer [FFITypesMsg]) (Either FunPositionError a)

-- | Classify how a function result is passed from C to Haskell
classifyResPassingMethod ::
     (Translation.IsPassCompat p, Types p ~ TranslatedTypes p)
  => C.Type p -- ^ Result type
  -> Hs.Type  -- ^ Translation of the result type
  -> Classified (ResPassingMethod p)
classifyResPassingMethod ty hsType
  -- Bad types
  | C.isVoid ty
  = pure $ Right PassResVoid
  -- Heap types: the Haskell FFI cannot pass them by value, so they cross it
  -- through a buffer on the Haskell heap
  | isHeapType ty
  = pure $ Right $ PassResNonVoid $ PassSerialized $
      serializedPointer Translation.FunRes ty
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
  = pure $ Right $ PassSerialized $ serializedPointer Translation.FunArg ty
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
    sugars <- ask
    mFFIType <- lift $ FFIType.toFFITypeWithMsgs sugars hsType
    pure $ case mFFIType of
      Just ffiType -> Right ffiType
      Nothing      -> Left $ FunPositionNoFFIType NoFFIType{
          position = pos
        , cType    = renderCType cType
        }

renderCType :: Translation.IsPassCompat p => C.Type p -> Text.Text
renderCType cType = Text.strip $ Text.pack $ C.showsType (const id) cType ""
