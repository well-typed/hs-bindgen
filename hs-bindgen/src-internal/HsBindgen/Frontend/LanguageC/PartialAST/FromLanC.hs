{-# LANGUAGE OverloadedLabels #-}

-- | Construct the partial AST from the language-C AST
module HsBindgen.Frontend.LanguageC.PartialAST.FromLanC (
    CanApply
  , mkPartialDecl
  , mkDecl
  ) where

import Control.Monad
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import GHC.Stack
import Language.C qualified as LanC
import Language.C.Data.Ident qualified as LanC
import Optics.Core (Lens')
import Optics.Core qualified as Optics

import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.LanguageC.Monad
import HsBindgen.Frontend.LanguageC.PartialAST
import HsBindgen.Frontend.LanguageC.PartialAST.ToBindgen
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level: construct the partial AST

  start with no information, then gradually apply updates
-------------------------------------------------------------------------------}

-- | Requirements on the hs-bindgen pass
--
-- See also discussion of 'FromLanC'.
class    (ValidPass p, Id p ~ PrelimDeclId) => CanApply p where
instance (ValidPass p, Id p ~ PrelimDeclId) => CanApply p where

mkPartialDecl ::
     ( HasCallStack
     , CanApply p
     )
  => LanC.CDeclaration a -> FromLanC p (PartialDecl p)
mkPartialDecl = \case
    LanC.CDecl declspecs declr _a ->
        (     repeatedly apply declspecs
          >=> repeatedly apply declr
        )
      $ unknownDecl
    other ->
      unexpectedF other

mkDecl :: CanApply p => LanC.CDeclaration a -> FromLanC p (Maybe CName, Type p)
mkDecl = mkPartialDecl >=> fromDecl

{-------------------------------------------------------------------------------
  Apply updates
-------------------------------------------------------------------------------}

class Apply p a b where
  apply :: Update p a b

type Update p a b = HasCallStack => a -> b -> FromLanC p b

{-------------------------------------------------------------------------------
  'PartialDecl'
-------------------------------------------------------------------------------}

instance CanApply p
      => Apply p (LanC.CDeclarationSpecifier a) (PartialDecl p) where
  apply = \case
      LanC.CTypeSpec x -> overM #partialType $ apply x
      LanC.CTypeQual x -> overM #partialType $ apply x

      LanC.CStorageSpec x ->
        case x of
         -- We ignore the @typedef@ specifier: when reparsing typedefs it adds
         -- no information, and elsewhere we don't expect it at all (we /could/
         -- in principle check in such cases that it's not there, to catch
         -- potential bugs, at the cost of some increased code complexity).
          LanC.CTypedef _a -> return

          -- TODO: other storage specifiers?
          other -> \_ -> unexpectedF other

      other -> \_ -> unexpectedF other

-- | See discussion of @init-declarator-list@ for 'CDecl'
--
-- <https://hackage-content.haskell.org/package/language-c-0.10.0/docs/Language-C-Syntax-AST.html#t:CDecl>
instance CanApply p
      => Apply p
          ( Maybe (LanC.CDeclarator  a)
          , Maybe (LanC.CInitializer a)
          , Maybe (LanC.CExpression  a)
          )
          (PartialDecl p) where
  apply = \case
      (Just decl, Nothing, Nothing) ->
        apply decl
      (declr, initr, expr) -> \_ ->
        unexpected $ show (
            nodeOmitted declr
          , nodeOmitted initr
          , nodeOmitted expr
          )

instance CanApply p
      => Apply p (LanC.CDeclarator a) (PartialDecl p) where
  apply = \case
      LanC.CDeclr name derived _asmname _attrs _a ->
            optionally setName name
        >=> repeatedly (overM #partialType . apply) (reverse derived)
    where
      setName :: LanC.Ident -> (PartialDecl p) -> FromLanC p (PartialDecl p)
      setName name = return . Optics.set #partialName (Just $ mkCName name)

{-------------------------------------------------------------------------------
  'PartialType'
-------------------------------------------------------------------------------}

withSign ::
     ValidPass p
  => Update p (Maybe C.PrimSign -> C.PrimType) (PartialType p)
withSign f = \case
    PartialUnknown unknown -> do
      let UnknownType{unknownSign, unknownConst} = unknown
      return $
          PartialKnown . KnownType
        $ ( if unknownConst then TypeConst else id )
        $ TypePrim $ f unknownSign
    other ->
      unexpected $ show other

notFun :: ValidPass p => Update p (Type p) (PartialType p)
notFun typ = \case
    PartialUnknown unknown -> do
      let UnknownType{unknownSign, unknownConst} = unknown
      case unknownSign of
        Nothing ->
          return $
              PartialKnown . KnownType
            $ ( if unknownConst then TypeConst else id )
            $ typ
        Just sign ->
          unexpected $ show (typ, sign)
    other ->
      unexpected $ show other

setSign :: ValidPass p => Update p C.PrimSign (PartialType p)
setSign sign = \case
    PartialUnknown unknown ->
      return $ PartialUnknown $ Optics.set #unknownSign (Just sign) unknown
    other ->
      unexpected $ show other

-- | Transition from unknown types to known types
instance CanApply p
      => Apply p (LanC.CTypeSpecifier a) (PartialType p) where
  apply = \case
      -- Void (for function result types only)
      LanC.CVoidType _a -> notFun $ TypeVoid

      -- Primitive types
      LanC.CCharType   _a -> withSign $ C.PrimChar . charSign
      LanC.CShortType  _a -> withSign $ C.PrimIntegral C.PrimShort . fromMaybe C.Signed
      LanC.CIntType    _a -> withSign $ C.PrimIntegral C.PrimInt   . fromMaybe C.Signed
      LanC.CLongType   _a -> withSign $ C.PrimIntegral C.PrimLong  . fromMaybe C.Signed
      LanC.CFloatType  _a -> notFun $ TypePrim $ C.PrimFloating C.PrimFloat
      LanC.CDoubleType _a -> notFun $ TypePrim $ C.PrimFloating C.PrimDouble
      LanC.CBoolType   _a -> notFun $ TypePrim $ C.PrimBool

      -- Complex types
      LanC.CComplexType _a -> \case
        PartialKnown (KnownType (TypePrim prim)) ->
          return $ PartialKnown . KnownType $ TypeComplex prim
        other ->
          unexpected $ show other

      -- Sign specifiers
      LanC.CSignedType _a -> setSign C.Signed
      LanC.CUnsigType  _a -> setSign C.Unsigned

      -- Unsupported types
      LanC.CInt128Type{}   -> \_ -> unsupported "CInt128Type"
      LanC.CUInt128Type{}  -> \_ -> unsupported "CUInt128Type"
      LanC.CBFloat16Type{} -> \_ -> unsupported "CBFloat16Type"
      LanC.CFloatNType{}   -> \_ -> unsupported "CFloatNType"
      LanC.CTypeOfExpr{}   -> \_ -> unsupported "CTypeOfExpr"
      LanC.CTypeOfType{}   -> \_ -> unsupported "CTypeOfType"
      LanC.CAtomicType{}   -> \_ -> unsupported "CAtomicType"

      -- User-defined types
      LanC.CSUType (LanC.CStruct su mTag mDef _attrs _a) _a' -> \partial -> do
        let tagKind = case su of
                        LanC.CStructTag -> C.TagKindStruct
                        LanC.CUnionTag  -> C.TagKindUnion
        name <- checkNotAnon mTag tagKind
        checkNoDef "struct or union definition" mDef
        notFun (typeRef name) partial
      LanC.CEnumType (LanC.CEnum mTag mDef _attrs _a) _a' -> \partial -> do
        name <- checkNotAnon mTag C.TagKindEnum
        checkNoDef "enum definition" mDef
        notFun (typeRef name) $ partial
      LanC.CTypeDef name _a -> \partial -> do
        let name' = mkCName name
        typeEnv <- getReparseEnv
        case Map.lookup name' typeEnv of
          Nothing  -> unexpected $ "user-defined type " ++ show name
          Just typ -> notFun typ partial
    where
      charSign :: Maybe C.PrimSign -> C.PrimSignChar
      charSign Nothing     = C.PrimSignImplicit Nothing
      charSign (Just sign) = C.PrimSignExplicit sign

      typeRef :: C.DeclName -> Type p
      typeRef = TypeRef . PrelimDeclIdNamed

      checkNotAnon :: Maybe LanC.Ident -> C.TagKind -> FromLanC p C.DeclName
      checkNotAnon mName tagKind =
          case mName of
            Just name ->
              return $ C.DeclName (mkCName name) (C.NameKindTagged tagKind)
            Nothing ->
              unsupported $ "Anonymous " ++ show tagKind

      checkNoDef :: String -> Maybe def -> FromLanC p ()
      checkNoDef _   Nothing  = return ()
      checkNoDef err (Just _) = unsupported err

instance Apply p (LanC.CTypeQualifier a) (PartialType p) where
  apply qual = \case
      PartialKnown   typ -> PartialKnown   <$> apply qual typ
      PartialUnknown typ -> PartialUnknown <$> apply qual typ

instance CanApply p
      => Apply p (LanC.CDerivedDeclarator a) (PartialType p) where
  apply deriv partial = case partial of
      PartialUnknown{} ->
        unexpected $ show (nodeOmitted deriv, partial)
      PartialKnown typ ->
        PartialKnown <$> apply deriv typ

{-------------------------------------------------------------------------------
  'UnknownType'
-------------------------------------------------------------------------------}

instance Apply p (LanC.CTypeQualifier a) UnknownType where
  apply = \case
      LanC.CConstQual _a ->
        return . Optics.set #unknownConst True
      other -> \_ ->
        unexpectedF other

{-------------------------------------------------------------------------------
  'KnownType'
-------------------------------------------------------------------------------}

defaultApplyKnownType :: Apply p a (Type p) => Update p a (KnownType p)
defaultApplyKnownType x = fmap KnownType . apply x . fromKnownType

instance Apply p (LanC.CTypeQualifier a) (KnownType p) where
  apply = defaultApplyKnownType

instance CanApply p
      => Apply p (LanC.CDerivedDeclarator a) (KnownType p) where
  apply deriv = case deriv of
      LanC.CPtrDeclr{} -> defaultApplyKnownType deriv
      LanC.CArrDeclr{} -> defaultApplyKnownType deriv

      -- Special case for functions, to get argument names
      LanC.CFunDeclr params _attrs _a -> \res -> do
        params' <- mapM mkDecl =<< viewFunParams params
        return $ TopLevelFun params' (fromKnownType res)

{-------------------------------------------------------------------------------
  'Type' 'p'
-------------------------------------------------------------------------------}

instance Apply p (LanC.CTypeQualifier a) (Type p) where
  apply = \case
      LanC.CConstQual _ -> return . TypeConst
      LanC.CRestrQual _ -> return -- ignore @__restrict@
      other             -> \_ -> unexpectedF other

instance Apply p (LanC.CDerivedDeclarator a) (Type p) where
  apply = \case
      LanC.CPtrDeclr quals _a ->
        repeatedly apply quals . TypePointer
      LanC.CArrDeclr quals (LanC.CNoArrSize isCompleteType) _a ->
        if isCompleteType
          then \_ -> unexpected "complete array without size"
          else repeatedly apply quals . TypeIncompleteArray
      LanC.CArrDeclr quals (LanC.CArrSize _isStatic expr) _a -> \typ -> do
        sz <- case expr of
                LanC.CConst (LanC.CIntConst n _a') ->
                  return $ fromIntegral $ LanC.getCInteger n
                other ->
                  unsupported $ show (nodeOmitted other)
        -- TODO: Should we do something with _isStatic?
        repeatedly apply quals $ TypeConstArray sz typ
      other -> \_ ->
        unexpectedF other

{-------------------------------------------------------------------------------
  Internal auxiliary: language-c
-------------------------------------------------------------------------------}

mkCName :: LanC.Ident -> CName
mkCName (LanC.Ident name _hash _a) = Text.pack name

{-------------------------------------------------------------------------------
  Internal auxiliary: optics
-------------------------------------------------------------------------------}

overM :: Functor m => Lens' a b -> (b -> m b) -> a -> m a
overM l f a = flip (Optics.set l) a <$> f (Optics.view l a)

viewFunParams ::
     Either [LanC.Ident] ([LanC.CDeclaration a], Bool)
  -> FromLanC p [LanC.CDeclaration a]
viewFunParams = \case
    Left params ->
      unsupported $ "old-style parameters " ++ show params
    Right (_params, True) ->
      unsupported "variadic function"
    Right (params, False) ->
      case params of
        [LanC.CDecl [LanC.CTypeSpec (LanC.CVoidType _)] [] _] ->
          -- Explicitly empty parameter list, as in
          --
          -- > void f(void)
          return []
        _otherwise ->
          return $ params
