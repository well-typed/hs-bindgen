module HsBindgen.Frontend.Pass.TranslateTypes.PassingMethod (
    Classified
  , classifyResPassingMethod
  , classifyArgPassingMethod
  ) where

import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Control.Monad.Writer.Strict (MonadTrans (lift), Writer)
import Data.Bifunctor (second)
import Data.Text qualified as Text

import HsBindgen.Frontend.Pass.MangleNames.IsPass (MangleNames)
import HsBindgen.Frontend.Pass.TranslateTypes.Error
import HsBindgen.Frontend.Pass.TranslateTypes.FFIType qualified as FFIType
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass (TranslateTypes)
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.Msg (FFITypesMsg)
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.PassingMethod
import HsBindgen.Frontend.Pass.TranslateTypes.SugarInfo (SugarInfos)
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass.Definition (CoercePass (coercePass))
import HsBindgen.IR.Translation

{-------------------------------------------------------------------------------
  Argument passing method
-------------------------------------------------------------------------------}

-- | The passing method, or why the value cannot be passed
type Classified a =
    ReaderT SugarInfos (Writer [FFITypesMsg]) (Either FunPositionError a)

-- | Classify how a function result is passed from C to Haskell
classifyResPassingMethod ::
     C.FunctionRes MangleNames
  -> Hs.Type -- ^ Translation of the result type
  -> Classified (ResPassingMethod TranslateTypes)
classifyResPassingMethod res hsType
  -- Bad types
  | C.isVoid res.typ
  = pure $ Right PassResVoid
  -- Heap types: the Haskell FFI cannot pass them by value, so they cross it
  -- through a buffer on the Haskell heap
  | C.isCanonicalTypeStruct res.typ ||
    C.isCanonicalTypeUnion res.typ ||
    C.isCanonicalTypeComplex res.typ
  = pure $ Right $ PassResNonVoid $ PassSerialized TranslatedTypes{
        c  = coercePass ty'
      , hs = Translation.inContext Translation.FunRes ty'
      }
  -- Other types
  | otherwise
  = second (PassResNonVoid . PassDirect) <$>
      passDirect FunPositionRes res.typ hsType
  where
    ty' = C.TypePointers 1 res.typ

-- | Classify how a function argument is passed from Haskell to C
classifyArgPassingMethod ::
     FunPosition
  -> C.FunctionArg MangleNames
  -> Hs.Type -- ^ Translation of the argument type
  -> Classified (PassingMethod TranslateTypes)
classifyArgPassingMethod pos arg hsType
  -- Heap types: the Haskell FFI cannot pass them by value, so they cross it
  -- through a buffer on the Haskell heap
  | C.isCanonicalTypeStruct  arg.typ.typ ||
    C.isCanonicalTypeUnion   arg.typ.typ ||
    C.isCanonicalTypeComplex arg.typ.typ
  = pure $ Right $ PassSerialized TranslatedTypes{
        c  = coercePass ty'
      , hs = Translation.inContext Translation.FunArg ty'
      }
  -- Other types
  | otherwise
  = second PassDirect <$> passDirect pos arg.typ.typ hsType
  where
    ty' = C.TypePointers 1 arg.typ.typ

passDirect ::
     FunPosition
  -> C.Type MangleNames
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

renderCType :: C.Type MangleNames -> Text.Text
renderCType cType = Text.strip $ Text.pack $ C.showsType (const id) cType ""
