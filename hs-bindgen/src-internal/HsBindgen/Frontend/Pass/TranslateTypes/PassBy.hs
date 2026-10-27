module HsBindgen.Frontend.Pass.TranslateTypes.PassBy (
    classifyResPassingMethod
  , classifyArgPassingMethod
  ) where

import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass (AdjustTypes,
                                                   AdjustedFrom (NotAdjusted))
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass (TranslateTypes)
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.PassBy
import HsBindgen.Frontend.Pass.TranslateTypes.Translation qualified as Translation
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass.Definition (CoercePass (coercePass))
import HsBindgen.IR.Translation

{-------------------------------------------------------------------------------
  Argument passing method
-------------------------------------------------------------------------------}

-- | Classify how a function result is passed from C to Haskell
classifyResPassingMethod ::
     C.FunctionRes AdjustTypes
  -> PassResBy TranslateTypes
classifyResPassingMethod res
  -- Bad types
  | C.isCanonicalTypeArray res.typ
  = panicPure "classifyResPassingMethod: an array can not be the result type of a function"
  | C.isCanonicalTypeFunction res.typ
  = panicPure "classifyResPassingMethod: a function can not be the result type of a function"
  -- Heap types
  | C.isCanonicalTypeStruct res.typ ||
    C.isCanonicalTypeUnion res.typ ||
    C.isCanonicalTypeComplex res.typ
  = passResByAddress res.typ
  -- Other types
  | otherwise
  = passResByValue res.typ

passResByAddress :: C.Type AdjustTypes -> PassResBy TranslateTypes
passResByAddress ty = PassResBy {
      wrapperType = TranslatedTypes (coercePass ty') (Translation.inContext Translation.FunRes ty')
    , byAddress = True
    }
  where
    ty' = C.TypePointers 1 ty

passResByValue :: C.Type AdjustTypes -> PassResBy TranslateTypes
passResByValue ty = PassResBy {
      wrapperType = TranslatedTypes (coercePass ty) (Translation.inContext Translation.FunRes ty)
    , byAddress = False
    }

-- | Classify how a function argument is passed from Haskell to C
classifyArgPassingMethod ::
     C.FunctionArg AdjustTypes
  -> PassArgBy TranslateTypes
classifyArgPassingMethod arg
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
  = passArgByAddress arg.typ
  -- Other types
  | otherwise
  = passArgByValue arg.typ

passArgByAddress :: C.TypeFunArg AdjustTypes -> PassArgBy TranslateTypes
passArgByAddress ty = PassArgBy {
      wrapperType = TranslatedTypeFunArg (coercePass ty') (Translation.inContext Translation.FunArg ty')
    , byAddress = True
    }
  where
    ty' = C.TypeFunArgF (C.TypePointers 1 ty.typ) NotAdjusted

passArgByValue :: C.TypeFunArg AdjustTypes -> PassArgBy TranslateTypes
passArgByValue ty = PassArgBy {
      wrapperType = TranslatedTypeFunArg (coercePass ty) (Translation.inContext Translation.FunArg ty)
    , byAddress = False
    }
