module HsBindgen.Frontend.Pass.TranslateTypes.PassBy (
    Classified
  , classifyResPassingMethod
  , classifyArgPassingMethod
  ) where

import Control.Monad.Writer.Strict (Writer)
import Data.Bifunctor (second)
import Data.Text qualified as Text

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass (AdjustTypes,
                                                   AdjustedFrom (NotAdjusted))
import HsBindgen.Frontend.Pass.TranslateTypes.Error
import HsBindgen.Frontend.Pass.TranslateTypes.FFIType qualified as FFIType
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass (TranslateTypes)
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.PassBy
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass.Definition (CoercePass (coercePass))
import HsBindgen.IR.Translation

{-------------------------------------------------------------------------------
  Argument passing method
-------------------------------------------------------------------------------}

-- | The passing method, or why a value cannot be passed by value
type Classified a = Writer [FFIType.ExtBindingFallback] (Either NoFFIType a)

-- | Classify how a function result is passed from C to Haskell
classifyResPassingMethod ::
     C.FunctionRes AdjustTypes
  -> Hs.Type -- ^ Translation of the result type
  -> Classified (PassResBy TranslateTypes)
classifyResPassingMethod res hsType
  -- Bad types
  | C.isCanonicalTypeArray res.typ
  = panicPure "classifyResPassingMethod: an array can not be the result type of a function"
  | C.isCanonicalTypeFunction res.typ
  = panicPure "classifyResPassingMethod: a function can not be the result type of a function"
  | C.isVoid res.typ
  = pure $ Right PassResVoid
  -- Heap types
  | C.isCanonicalTypeStruct res.typ ||
    C.isCanonicalTypeUnion res.typ ||
    C.isCanonicalTypeComplex res.typ
  = pure $ Right $ PassResByAddress TranslatedTypes{
        c  = coercePass ty'
      , hs = Translation.inContext Translation.FunRes ty'
      }
  -- Other types
  | otherwise
  = second PassResByValue <$> passByValue FunPositionRes res.typ hsType
  where
    ty' = C.TypePointers 1 res.typ

-- | Classify how a function argument is passed from Haskell to C
classifyArgPassingMethod ::
     FunPosition
  -> C.FunctionArg AdjustTypes
  -> Hs.Type -- ^ Translation of the argument type
  -> Classified (PassArgBy TranslateTypes)
classifyArgPassingMethod pos arg hsType
  -- Bad types
  | C.isCanonicalTypeArray arg.typ.typ
  = panicPure
      "classifyArgPassingMethod: found a function argument/result type that is an array, \
      \which is unexpected because it should have been adjusted to a pointer. \
      \Is there a bug in the AdjustTypes frontend pass?"
  | C.isCanonicalTypeFunction arg.typ.typ
  = panicPure
      "classifyArgPassingMethod: found a function argument/result type that is a function, \
      \which is unexpected because it should have been adjusted to a pointer. \
      \Is there a bug in the AdjustTypes frontend pass?"
  -- Heap types
  | C.isCanonicalTypeStruct  arg.typ.typ ||
    C.isCanonicalTypeUnion   arg.typ.typ ||
    C.isCanonicalTypeComplex arg.typ.typ
  = pure $ Right $ PassArgByAddress TranslatedTypeFunArg{
        c  = coercePass ty'
      , hs = Translation.inContext Translation.FunArg ty'
      }
  -- Other types
  | otherwise
  = second PassArgByValue <$> passByValue pos arg.typ.typ hsType
  where
    ty' = C.TypeFunArgF (C.TypePointers 1 arg.typ.typ) NotAdjusted

passByValue ::
     FunPosition
  -> C.Type AdjustTypes
  -> Hs.Type
  -> Classified Hs.FFIType
passByValue pos cType hsType = do
    mFFIType <- FFIType.toFFITypeWithFallbacks hsType
    pure $ case mFFIType of
      Just ffiType -> Right ffiType
      Nothing      -> Left NoFFIType{
          position = pos
        , cType    = Text.strip $ Text.pack $ C.showsType (const id) cType ""
        }
