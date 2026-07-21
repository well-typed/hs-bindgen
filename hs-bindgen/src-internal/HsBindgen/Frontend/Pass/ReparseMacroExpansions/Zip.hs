-- | Zip the C ASTs before and after reparsing
module HsBindgen.Frontend.Pass.ReparseMacroExpansions.Zip (
    ZipResult (..)
  , ZipReparsed
  , zipEither
  ) where

import Control.Monad qualified as M

import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.ForgetAnn (coerceNonMacroType)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (ReparseMacroExpansions)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass.Msg
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.LanC (LanC)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Zip.Error
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Language.C qualified as C

type In  = PrepareReparse
type Out = LanC

{-------------------------------------------------------------------------------
  Monadic result type
-------------------------------------------------------------------------------}

data ZipResult a =
    ZipSuccess a
  | ZipFailure [ZipError]
  deriving stock Functor

instance Applicative ZipResult where
  pure = ZipSuccess
  ZipSuccess f <*> m  = fmap f m
  ZipFailure e <*> _m = ZipFailure e

instance Monad ZipResult where
  ZipSuccess x >>= k = k x
  ZipFailure e >>= _ = ZipFailure e

success :: a -> ZipResult a
success = ZipSuccess

failure :: [ZipError] -> ZipResult a
failure = ZipFailure

checkEq :: (Show a, Eq a) => a -> a -> ZipResult a
checkEq lhs rhs
  | lhs == rhs = success rhs
  | otherwise  = failure $ zipErrorNotEqual lhs rhs

checkEqCoerce ::
     forall a. (
         Eq (a ReparseMacroExpansions)
       , Show (a ReparseMacroExpansions)
       , CoercePass a Out ReparseMacroExpansions
       , CoercePass a In ReparseMacroExpansions
       )
  => a In
  -> a Out
  -> ZipResult (a ReparseMacroExpansions)
checkEqCoerce lhs rhs =
    if lhsA == rhsA then
      success rhsA
    else
      failure $ zipErrorNotEqual lhsA rhsA
  where
    lhsA, rhsA :: a ReparseMacroExpansions
    lhsA = coercePass lhs
    rhsA = coercePass rhs

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

-- | Zip the C ASTs before and after reparsing
--
-- The reparser parses raw @libclang@ tokens and replaces parts of the C AST
-- with the parse results. To ensure correct Haskell bindings, this replacement
-- must adhere to certain rules. For example, if we have a global variable of
-- type @int@ before reparsing, then it is fine if the reparser replaces that
-- @int@ with some syntactical sugar (e.g., a @typedef@) representing an @int@.
-- However, it is not okay to replace that @int@ by a reference to a @struct@,
-- for example.
--
-- The 'zip' function operates recursively on sub-trees of C ASTs. It takes a
-- tree from before reparsing, \( t_{pre} \), and a tree from after reparsing,
-- \( t_{post} \), and produces a zipped tree, \( t_{zipped} \). Firstly, the
-- 'zip' function checks that \( t_{pre} \) has undergone a small set of
-- allowed modifications with respect to \( t_{post} \). Moreover, 'zip'
-- combines information from both trees to produce a new \( t_{zipped} \),
-- which is sometimes necessary because the reparser has access to limited
-- information that is readily available in \( t_{pre} \).
--
-- For the most part, the process of 'zip' is simple:
--
-- * It checks that the root nodes of \( t_{pre} \) and \( t_{post} \) are of
--   the same type (for example, a 'C.TypeFun').
-- * If the root nodes are of the same type, then recursively zip sub-trees.
-- * If that was successful, produce a new \( t_{zipped} \) from the recursively
--   zipped sub-trees with the same type of root node as before.
-- * If any previous step fails, we return an error.
--
-- There are also more interesting cases (that only apply to C types):
--
-- * Before reparsing, there are no references to macro types, but after
--   reparsing there may be. As such, if \( t_{pre} \) is a C type, and \( t_{post} \)
--   is a reference to a macro type, then we consider the trees to be
--   zipped. However, the reparser does not know what the underlying type of
--   the macro reference is, but we know that the underlying type is precisely
--   the C type that was replaced. So, 'zip' produces a \( t_{zipped} \) that
--   is the macro reference from \( t_{post} \) with \( t_{pre} \) as the
--   underlying type.
--
-- * Before reparsing, all @char@ C types have a /known/ implicit sign. The
--   reparser is not smart enough to do the same, and only produces /unknown/
--   implicit signs. In such cases, \( t_{pre} \) and \( t_{post} \) are
--   considered to be zipped, but we produce \( t_{pre} \) as \( t_{zipped} \),
--   so that the implicit sign is also known from this point onward.
--
-- NOTE: This zip algorithm assumes that the reparser only produces
-- \( t_{post} \) that differ from \( t_{pre} \) exactly at single nodes in our
-- tree representation of the C AST. This assumption is warrented: the reparser
-- only replaces nodes in our tree representation of C types with macro
-- reference sugar, which behave just like @typedef@s. If the reparser were
-- changed in the future to produce \( t_{post} \) that are more generally
-- allowed to differ from \( t_{pre} \), then this algorithm might not work
-- anymore.
--
-- NOTE: the description of PR #1979 describes historically why we implemented
-- the 'zip' function, which mainly had to do with supporting underlying types
-- for macro references.
--
-- <https://github.com/well-typed/hs-bindgen/pull/1979>
--
class ZipReparsed a where
  zipReparsed :: a In -> a Out -> ZipResult (a ReparseMacroExpansions)

zipEither ::
     ZipReparsed a
  => a In
  -> a Out
  -> Either [DelayedReparseMacroExpansionsMsg] (a ReparseMacroExpansions)
zipEither pre post =
    case zipReparsed pre post of
      ZipFailure es -> Left $ fmap ReparseMacroExpansionsZip es
      ZipSuccess z -> Right z

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance ZipReparsed C.ExplicitField where
  zipReparsed fieldPre field = do
      info'   <- checkEqCoerce fieldPre.info   field.info
      typ'    <- zipType      fieldPre.typ    field.typ
      offset' <- checkEq      fieldPre.offset field.offset
      width'  <- checkEq      fieldPre.width  field.width
      success C.ExplicitField{
          info   = info'
        , typ    = typ'
        , offset = offset'
        , width  = width'
        , ann    = NoAnn
        }

instance ZipReparsed C.Typedef where
  zipReparsed typedefPre typedef = do
      typ' <- zipType typedefPre.typ typedef.typ
      success C.Typedef{
          typ = typ'
        , ann = NoAnn
        }

instance ZipReparsed C.Function where
  zipReparsed funPre fun = do
      args'  <- zipList zipFunctionArg funPre.args fun.args
      res'   <- zipType                funPre.res   fun.res
      attrs' <- checkEq                funPre.attrs fun.attrs
      success C.Function{
          args  = args'
        , res   = res'
        , attrs = attrs'
        , ann   = NoAnn
        }
    where
      zipFunctionArg ::
           C.FunctionArg In
        -> C.FunctionArg Out
        -> ZipResult (C.FunctionArg ReparseMacroExpansions)
      zipFunctionArg arg1 arg2 = do
            name'   <- checkEq       arg1.name   arg2.name
            argTyp' <- zipTypeFunArg arg1.argTyp arg2.argTyp
            success C.FunctionArg {
                name   = name'
              , argTyp = argTyp'
              }

instance ZipReparsed C.Global where
  zipReparsed globalPre global = do
      typ' <- zipType globalPre.typ global.typ
      success C.Global {
          typ = typ'
        , ann = NoAnn
        }

{-------------------------------------------------------------------------------
  Zip types
-------------------------------------------------------------------------------}

zipType :: C.Type In -> C.Type Out -> ZipResult (C.Type ReparseMacroExpansions)
zipType t1 t2 = case (t1, t2) of
      -- Interesting case: an arbitrary type is replaced by a macro reference
      (_, C.TypeMacro ref2) -> do
        case (ref2.name.name.text, t1) of
          -- Special case: @#define bool _Bool@ from stdbool.h is normalised so
          -- that @bool@ renders identically to @_Bool@ regardless of
          -- @language-c@ version.
          ("bool", C.TypePrim C.PrimBool) ->
            success $ coerceNonMacroType t1
          _otherwise ->
            success $ C.TypeMacro C.MacroRef {
                name       = ref2.name
              , underlying = coerceNonMacroType t1
              }

      -- boring recursive cases
      (C.TypePrim pt1, C.TypePrim pt2) ->
        C.TypePrim <$> zipPrimType pt1 pt2
      (C.TypeComplex pt1, C.TypeComplex pt2) ->
        C.TypeComplex <$> zipPrimType pt1 pt2
      (C.TypeRef id1, C.TypeRef id2) ->
        C.TypeRef <$> checkEq id1 id2
      (C.TypeEnum e1, C.TypeEnum e2) ->
        C.TypeEnum <$> zipRef e1 e2
      (C.TypeTypedef td1, C.TypeTypedef td2) ->
        C.TypeTypedef <$> zipRef td1 td2
      (C.TypePointers n1 t1', C.TypePointers n2 t2') ->
        C.TypePointers <$> checkEq n1 n2 <*> zipType t1' t2'
      (C.TypeConstArray n1 et1, C.TypeConstArray n2 et2) ->
        C.TypeConstArray <$> checkEq n1 n2 <*> zipType et1 et2
      (C.TypeIncompleteArray et1, C.TypeIncompleteArray et2) ->
        C.TypeIncompleteArray <$> zipType et1 et2
      (C.TypeFun args1 res1, C.TypeFun args2 res2) ->
        C.TypeFun
          <$> zipList zipTypeFunArg args1 args2
          <*> zipType res1 res2
      (C.TypeVoid, C.TypeVoid) ->
        success C.TypeVoid
      (C.TypeBlock t1', C.TypeBlock t2') ->
        C.TypeBlock <$> zipType t1' t2'
      (C.TypeQual q1 t1', C.TypeQual q2 t2') ->
        C.TypeQual <$> zipTypeQual q1 q2 <*> zipType t1' t2'

      -- fail in any other case
      (_, _) -> failure $ zipErrorNotZipped t1 t2
    where
      -- Impossible case: macro references do not exist in the C AST before
      -- reparsing. Caught by the type checker.
      _coveredAllCasesIn :: C.Type In -> ()
      _coveredAllCasesIn = \case
        C.TypePrim{} -> ()
        C.TypeComplex{} -> ()
        C.TypeRef{} -> ()
        C.TypeEnum{} -> ()
        C.TypeTypedef{} -> ()
        C.TypePointers{} -> ()
        C.TypeConstArray{} -> ()
        C.TypeIncompleteArray{} -> ()
        C.TypeFun{} -> ()
        C.TypeVoid{} -> ()
        C.TypeBlock{} -> ()
        C.TypeQual{} -> ()

      _coveredAllCasesOut :: C.Type Out -> ()
      _coveredAllCasesOut = \case
        C.TypePrim{} -> ()
        C.TypeComplex{} -> ()
        C.TypeRef{} -> ()
        C.TypeEnum{} -> ()
        C.TypeMacro{} -> ()
        C.TypeTypedef{} -> ()
        C.TypePointers{} -> ()
        C.TypeConstArray{} -> ()
        C.TypeIncompleteArray{} -> ()
        C.TypeFun{} -> ()
        C.TypeVoid{} -> ()
        C.TypeBlock{} -> ()
        C.TypeQual{} -> ()

zipTypeQual :: C.TypeQual -> C.TypeQual -> ZipResult C.TypeQual
zipTypeQual C.QualConst C.QualConst = success C.QualConst

zipPrimType :: C.PrimType -> C.PrimType -> ZipResult C.PrimType
zipPrimType t1 t2 = case (t1, t2) of
    (C.PrimChar csign1, C.PrimChar csign2) ->
        C.PrimChar <$> zipPrimSignChar csign1 csign2
    _ -> checkEq t1 t2
  where
    _coveredAllCases :: C.PrimType -> ()
    _coveredAllCases = \case
      C.PrimChar{}     -> ()
      C.PrimIntegral{} -> ()
      C.PrimFloating{} -> ()
      C.PrimBool{}     -> ()

zipPrimSignChar ::
     C.PrimSignChar
  -> C.PrimSignChar
  -> ZipResult C.PrimSignChar
zipPrimSignChar csign1 csign2 = case (csign1, csign2) of
    -- interesting case: the reparser does not know the sign of a character
    -- when it is implicit, but @libclang@ does know this
    (C.PrimSignImplicit (Just psign1), C.PrimSignImplicit Nothing) ->
      success $ C.PrimSignImplicit (Just psign1)
    _ -> checkEq csign1 csign2
  where
    _coveredAllCases :: C.PrimSignChar -> ()
    _coveredAllCases = \case
        C.PrimSignExplicit{} -> ()
        C.PrimSignImplicit{} -> ()

zipTypeFunArg ::
     C.TypeFunArg In
  -> C.TypeFunArg Out
  -> ZipResult (C.TypeFunArg ReparseMacroExpansions)
zipTypeFunArg arg1 arg2 =
    C.TypeFunArgF
      <$> zipType arg1.typ arg2.typ
      <*> checkEq arg1.ann arg2.ann

zipRef ::
     C.Ref (Id In) In
  -> C.Ref (Id Out) Out
  -> ZipResult (C.EnumRef ReparseMacroExpansions)
zipRef ref1 ref2 =
    C.Ref
      <$> checkEq ref1.name ref2.name
      <*> zipType ref1.underlying ref2.underlying

{-------------------------------------------------------------------------------
  Zip lists
-------------------------------------------------------------------------------}

-- | Like 'M.zipWithM', but fails if the two arguments lists are of non-equal
-- length
zipList :: (Show a, Show b) => (a -> b -> ZipResult c) -> [a] -> [b] -> ZipResult [c]
zipList zipOne xs ys
  | length xs /= length ys
  = failure $ zipErrorNotZipped xs ys
  | otherwise
  = M.zipWithM zipOne xs ys
