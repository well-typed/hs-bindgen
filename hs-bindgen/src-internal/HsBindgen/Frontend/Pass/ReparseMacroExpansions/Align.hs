-- | Align the C ASTs before and after reparsing
--
-- Intended to be imported qualified:
--
-- > import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Align.Error qualified as Align
module HsBindgen.Frontend.Pass.ReparseMacroExpansions.Align (
    AlignResult (..)
  , Align (..)
  , alignEither
  ) where

import Control.Monad (zipWithM)

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass (IsPass (Ann))
import HsBindgen.Frontend.Pass.Parse.Msg (DelayedParseMsg (ParseMacroErrorReparseAlign))
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Align.Error qualified as Align
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (ReparseMacroExpansions)
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Shorthand
-------------------------------------------------------------------------------}

type P = ReparseMacroExpansions

{-------------------------------------------------------------------------------
  AlignResult
-------------------------------------------------------------------------------}

-- | The result of the 'align' function is either a successfully aligned value,
-- or an 'AlignError'.
--
-- Note: this type is equivalent to @Either Alignerror a@
data AlignResult a =
    AlignSuccess a
  | AlignFailure [Align.Error]
  deriving stock Functor

instance Applicative AlignResult where
  pure = AlignSuccess
  AlignSuccess f <*> m  = fmap f m
  AlignFailure e <*> _m = AlignFailure e

instance Monad AlignResult where
  AlignSuccess x >>= k = k x
  AlignFailure e >>= _ = AlignFailure e

success :: a -> AlignResult a
success = AlignSuccess

failure :: [Align.Error] -> AlignResult a
failure = AlignFailure

checkEq :: (Show a, Eq a) => a -> a -> AlignResult a
checkEq lhs rhs
  | lhs == rhs = success rhs
  | otherwise = failure $ Align.errNotEqual lhs rhs

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

-- | Align the C ASTs before and after rearsing
--
-- The reparser parses raw @libclang@ tokens and replaces parts of the C AST
-- with the parse results. To ensure correct Haskell bindings, this replacement
-- must adhere to certain rules. For example, if we have a global variable of
-- type @int@ before reparsing, then it is fine if the reparser replaces that
-- @int@ with some syntactical sugar (e.g., a @typedef@) representing an @int@.
-- However, it is not okay to replace that @int@ by a reference to a @struct@,
-- for example.
--
-- The 'align' function operates recursively on sub-trees of C ASTs. It takes a
-- tree from before reparsing, \( t_{pre} \), and a tree from after reparsing,
-- \( t_{post} \), and produces an aligned tree, \( t_{aligned} \). Firstly, the
-- 'align' function checks that \( t_{pre} \) has undergone a small set of
-- allowed modifications with respect to \( t_{post} \). Moreover, 'align'
-- combines information from both trees to produce a new \( t_{aligned} \),
-- which is sometimes necessary because the reparser has access to limited
-- information that is readily available in \( t_{pre} \).
--
-- For the most part, the process of 'align' is simple:
--
-- * It checks that the root nodes of \( t_{pre} \) and \( t_{post} \) are of
--   the same type (for example, a 'C.TypeFun').
-- * If the root nodes are of the same type, then recursively align sub-trees.
-- * If that was successful, produce a new \( t_{align} \) from the recursively
--   aligned sub-trees with the same type of root node as before.
-- * If any previous step fails, we return an error.
--
-- There are also more interesting cases (that only apply to C types):
--
-- * Before reparsing, there are no references to macro types, but after
--   reparsing there may be. As such, if \( t_{pre} \) is a C type, and \( t_{post} \)
--   is a reference to a macro type, then we consider the trees to be
--   aligned. However, the reparser does not know what the underlying type of
--   the macro reference is, but we know that the underlying type is precisely
--   the C type that was replaced. So, 'align' produces a \( t_{align} \) that
--   is the macro reference from \( t_{post} \) with \( t_{pre} \) as the
--   underlying type.
--
-- * Before reparsing, all @char@ C types have a /known/ implicit sign. The
--   reparser is not smart enough to do the same, and only produces /unknown/
--   implicit signs. In such cases, \( t_{pre} \) and \( t_{post} \) are
--   considered to be aligned, but we produce \( t_{pre} \) as \( t_{aligned} \),
--   so that the implicit sign is also known from this point onward.
--
-- NOTE: This alignment algorithm assumes that the reparser only produces
-- \( t_{post} \) that differ from \( t_{pre} \) exactly at single nodes in our
-- tree representation of the C AST. This assumption is warrented: the reparser
-- only replaces nodes in our tree representation of C types with macro
-- reference sugar, which behave just like @typedef@s. If the reparser were
-- changed in the future to produce \( t_{post} \) that are more generally
-- allowed to differ from \( t_{pre} \), then this algorithm might not work
-- anymore.
--
-- NOTE: the description of PR #1979 describes historically why we implemented
-- the 'align' function, which mainly had to do with supporting underlying types
-- for macro references.
--
-- <https://github.com/well-typed/hs-bindgen/pull/1979>
--
class Align a where
  align :: a -> a -> AlignResult a

alignEither :: Align a => a -> a -> Either [DelayedParseMsg] a
alignEither x y =
    case align x y of
      AlignFailure es -> Left $ fmap ParseMacroErrorReparseAlign es
      AlignSuccess z -> Right z

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Align (C.StructField P) where
  align field1 field2 = do
      typ' <- align field1.typ field2.typ
      ann' <- checkEq field1.ann field2.ann
      offset' <- checkEq field1.offset field2.offset
      width' <- checkEq field1.width field2.width
      info' <- checkEq field1.info field2.info
      pure C.StructField {
          typ = typ'
        , ann = ann'
        , offset = offset'
        , width = width'
        , info = info'
        }

instance Align (C.UnionField P)where
  align field1 field2 = do
      typ' <- align field1.typ field2.typ
      ann' <- checkEq field1.ann field2.ann
      info' <- checkEq field1.info field2.info
      pure C.UnionField {
          typ = typ'
        , ann = ann'
        , info = info'
        }

instance Align (C.Function P) where
  align fun1 fun2 = do
      args' <- align fun1.args fun2.args
      res' <- align fun1.res fun2.res
      attrs' <- checkEq fun1.attrs fun2.attrs
      ann' <- checkEq fun1.ann fun2.ann
      pure C.Function {
          args = args'
        , res = res'
        , attrs = attrs'
        , ann = ann'
        }

instance Align [C.FunctionArg P] where
  align = zipWithM align

instance Align (C.FunctionArg P) where
  align arg1 arg2 = do
      name' <- checkEq arg1.name arg2.name
      argTyp' <- align arg1.argTyp arg2.argTyp
      pure C.FunctionArg {
          name = name'
        , argTyp = argTyp'
        }

instance Align (C.Global P) where
  align global1 global2 = do
      typ' <- align global1.typ global2.typ
      ann' <- checkEq global1.ann global2.ann
      pure C.Global {
          typ = typ'
        , ann = ann'
        }

instance Align (C.Type P) where
  align t1 t2 = case (t1, t2) of
      -- impossible case: macro references don't exist in our C AST before
      -- reparsing
      (C.TypeMacro m1, _) -> failure $ Align.errMacroRefBefore m1 t1 t2

      -- interesting case: an arbitrary type is replaced by a macro reference
      (_, C.TypeMacro m2) -> do
        underlying' <- align t1 m2.underlying
        success $ C.TypeMacro C.Ref {
            name = m2.name
          , underlying = underlying'
          }

      -- boring recursive cases
      (C.TypePrim pt1, C.TypePrim pt2) ->
        C.TypePrim <$> align pt1 pt2
      (C.TypeComplex pt1, C.TypeComplex pt2) ->
        C.TypeComplex <$> align pt1 pt2
      (C.TypeRef id1, C.TypeRef id2) ->
        C.TypeRef <$> checkEq id1 id2
      (C.TypeEnum e1, C.TypeEnum e2) ->
        C.TypeEnum <$> alignEnumRef e1 e2
      (C.TypeTypedef td1, C.TypeTypedef td2) ->
        C.TypeTypedef <$> alignTypedefRef td1 td2
      (C.TypePointers n1 t1', C.TypePointers n2 t2') ->
        C.TypePointers <$> checkEq n1 n2 <*> align t1' t2'
      (C.TypeConstArray n1 et1, C.TypeConstArray n2 et2) ->
        C.TypeConstArray <$> checkEq n1 n2 <*> align et1 et2
      (C.TypeIncompleteArray et1, C.TypeIncompleteArray et2) ->
        C.TypeIncompleteArray <$> align et1 et2
      (C.TypeFun args1 res1, C.TypeFun args2 res2) ->
        C.TypeFun <$> zipWithM align args1 args2 <*> align res1 res2
      (C.TypeVoid, C.TypeVoid) ->
        pure C.TypeVoid
      (C.TypeBlock t1', C.TypeBlock t2') ->
        C.TypeBlock <$> align t1' t2'
      (C.TypeQual q1 t1', C.TypeQual q2 t2') ->
        C.TypeQual <$> align q1 q2 <*> align t1' t2'

      -- fail in any other case
      (_, _) -> failure $ Align.errNotAligned t1 t2
    where
      _coveredAllCases :: C.Type P -> ()
      _coveredAllCases = \case
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

instance Align C.TypeQual where
  align C.QualConst C.QualConst = success C.QualConst

instance Align C.PrimType where
  align t1 t2 = case (t1, t2) of
      (C.PrimChar csign1, C.PrimChar csign2) ->
          C.PrimChar <$> align csign1 csign2
      _ -> checkEq t1 t2
    where
      _coveredAllCases :: C.PrimType -> ()
      _coveredAllCases = \case
        C.PrimChar{}     -> ()
        C.PrimIntegral{} -> ()
        C.PrimFloating{} -> ()
        C.PrimBool{}     -> ()

instance Align C.PrimSignChar where
  align csign1 csign2 = case (csign1, csign2) of
      -- interesting case: the reparser does not know the sign of a character
      -- when it is implicit, but @libclang@ does know this
      (C.PrimSignImplicit (Just psign1), C.PrimSignImplicit Nothing) ->
        pure $ C.PrimSignImplicit (Just psign1)
      _ -> checkEq csign1 csign2
    where
      _coveredAllCases :: C.PrimSignChar -> ()
      _coveredAllCases = \case
          C.PrimSignExplicit{} -> ()
          C.PrimSignImplicit{} -> ()

instance Align (C.TypeFunArg P) where
  align arg1 arg2 =
      C.TypeFunArgF <$> align arg1.typ arg2.typ <*> alignAnnTypeFunArg arg1.ann arg2.ann

alignEnumRef ::
     C.EnumRef P
  -> C.EnumRef P
  -> AlignResult (C.EnumRef P)
alignEnumRef ref1 ref2 =
    C.Ref <$> checkEq ref1.name ref2.name <*> align ref1.underlying ref2.underlying

alignTypedefRef ::
     C.EnumRef P
  -> C.EnumRef P
  -> AlignResult (C.EnumRef P)
alignTypedefRef ref1 ref2 =
    C.Ref <$> checkEq ref1.name ref2.name <*> align ref1.underlying ref2.underlying

alignAnnTypeFunArg ::
     Ann "TypeFunArg" P
  -> Ann "TypeFunArg" P
  -> AlignResult (Ann "TypeFunArg" P)
alignAnnTypeFunArg ann1 ann2 = checkEq ann1 ann2
