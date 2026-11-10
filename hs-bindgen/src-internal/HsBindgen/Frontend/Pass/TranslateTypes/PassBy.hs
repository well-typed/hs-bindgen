module HsBindgen.Frontend.Pass.TranslateTypes.PassBy (
    Classified
  , classifyResPassingMethod
  , classifyArgPassingMethod
  ) where

import Control.Monad.Writer.Strict (Writer)
import Data.Bifunctor (second)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text

import HsBindgen.Frontend.Pass.AdjustTypes.IsPass (AdjustTypes)
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

-- | The passing method, or why the value cannot be passed
type Classified a =
    Writer [FFIType.ExtBindingFallback] (Either TranslateTypesError a)

-- | Classify how a function result is passed from C to Haskell
classifyResPassingMethod ::
     C.FunctionRes AdjustTypes
  -> Hs.Type -- ^ Translation of the result type
  -> Classified (PassResBy TranslateTypes)
classifyResPassingMethod res hsType
  -- Bad types
  | C.isCanonicalTypeArray res.typ
  = unexpected FunPositionRes UnexpectedArray res.typ
  | C.isCanonicalTypeFunction res.typ
  = unexpected FunPositionRes UnexpectedFunction res.typ
  | C.isVoid res.typ
  = pure $ Right PassResVoid
  -- Types the Haskell FFI cannot pass by value
  | C.isCanonicalTypeStruct res.typ ||
    C.isCanonicalTypeUnion res.typ ||
    C.isCanonicalTypeComplex res.typ
  = pure $ Right $ PassResNonVoid $ PassByAddress TranslatedTypes{
        c  = coercePass ty'
      , hs = Translation.inContext Translation.FunRes ty'
      }
  -- Other types
  | otherwise
  = second (PassResNonVoid . PassByValue) <$>
      passByValue FunPositionRes res.typ hsType
  where
    ty' = C.TypePointers 1 res.typ

-- | Classify how a function argument is passed from Haskell to C
classifyArgPassingMethod ::
     FunPosition
  -> C.FunctionArg AdjustTypes
  -> Hs.Type -- ^ Translation of the argument type
  -> Classified (PassBy TranslateTypes)
classifyArgPassingMethod pos arg hsType
  -- Bad types
  | C.isCanonicalTypeArray arg.typ.typ
  = unexpected pos UnexpectedArray arg.typ.typ
  | C.isCanonicalTypeFunction arg.typ.typ
  = unexpected pos UnexpectedFunction arg.typ.typ
  -- Types the Haskell FFI cannot pass by value
  | C.isCanonicalTypeStruct  arg.typ.typ ||
    C.isCanonicalTypeUnion   arg.typ.typ ||
    C.isCanonicalTypeComplex arg.typ.typ
  = pure $ Right $ PassByAddress TranslatedTypes{
        c  = coercePass ty'
      , hs = Translation.inContext Translation.FunArg ty'
      }
  -- Other types
  | otherwise
  = second PassByValue <$> passByValue pos arg.typ.typ hsType
  where
    ty' = C.TypePointers 1 arg.typ.typ

passByValue ::
     FunPosition
  -> C.Type AdjustTypes
  -> Hs.Type
  -> Classified Hs.FFIType
passByValue pos cType hsType = do
    mFFIType <- FFIType.toFFITypeWithFallbacks hsType
    pure $ case mFFIType of
      Just ffiType -> Right ffiType
      Nothing      -> Left $ TranslateTypesNoFFIType $ NoFFIType{
          position = pos
        , cType    = renderCType cType
        } :| []

unexpected ::
     FunPosition
  -> UnexpectedTypeKind
  -> C.Type AdjustTypes
  -> Classified a
unexpected pos kind cType =
    pure $ Left $ TranslateTypesUnexpectedType $ UnexpectedType{
        position = pos
      , kind     = kind
      , cType    = renderCType cType
      } :| []

renderCType :: C.Type AdjustTypes -> Text.Text
renderCType cType = Text.strip $ Text.pack $ C.showsType (const id) cType ""
