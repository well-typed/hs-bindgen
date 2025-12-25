{-# LANGUAGE OverloadedLabels #-}

-- | Construct the partial AST from the language-C AST
module HsBindgen.Frontend.LanguageC.PartialAST.FromLanC (
    mkPartialDecl
  , mkDecl
  ) where

import Control.Monad
import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.Stack
import Language.C qualified as LanC
import Language.C.Data.Ident qualified as LanC
import Optics.Core (Lens')
import Optics.Core qualified as Optics

import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LanguageC.Monad
import HsBindgen.Frontend.LanguageC.PartialAST
import HsBindgen.Frontend.LanguageC.PartialAST.ToBindgen
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level: construct the partial AST

  start with no information, then gradually apply updates
-------------------------------------------------------------------------------}

mkPartialDecl :: HasCallStack => LanC.CDeclaration a -> FromLanC PartialDecl
mkPartialDecl = \case
    LanC.CDecl declspecs declr _a ->
        (     repeatedly apply declspecs
          >=> repeatedly apply declr
        )
      $ unknownDecl
    other ->
      unexpectedF other

mkDecl :: LanC.CDeclaration a -> FromLanC (Maybe CName, C.Type HandleMacros)
mkDecl = mkPartialDecl >=> fromDecl

{-------------------------------------------------------------------------------
  Apply updates
-------------------------------------------------------------------------------}

class Apply a b where
  apply :: Update a b

type Update a b = HasCallStack => a -> b -> FromLanC b

{-------------------------------------------------------------------------------
  'PartialDecl'
-------------------------------------------------------------------------------}

instance Apply (LanC.CDeclarationSpecifier a) PartialDecl where
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
instance Apply
           ( Maybe (LanC.CDeclarator  a)
           , Maybe (LanC.CInitializer a)
           , Maybe (LanC.CExpression  a)
           )
           PartialDecl where
  apply = \case
      (Just decl, Nothing, Nothing) ->
        apply decl
      (declr, initr, expr) -> \_ ->
        unexpected $ show (
            nodeOmitted declr
          , nodeOmitted initr
          , nodeOmitted expr
          )

instance Apply (LanC.CDeclarator a) PartialDecl where
  apply = \case
      LanC.CDeclr name derived _asmname _attrs _a ->
            optionally setName name
        >=> repeatedly (overM #partialType . apply) (reverse derived)
    where
      setName :: LanC.Ident -> PartialDecl -> FromLanC PartialDecl
      setName name = return . Optics.set #partialName (Just $ mkCName name)

{-------------------------------------------------------------------------------
  'PartialType'
-------------------------------------------------------------------------------}

withSign :: Update (Maybe C.PrimSign -> C.PrimType) PartialType
withSign f = \case
    PartialUnknown unknown -> do
      let UnknownType{unknownSign, unknownConst} = unknown
      return $
          PartialKnown . KnownType
        $ (if unknownConst then C.TypeQualified C.TypeQualifierConst else id)
        $ C.TypePrim $ f unknownSign
    other ->
      unexpected $ show other

notFun :: Update (C.Type HandleMacros) PartialType
notFun typ = \case
    PartialUnknown unknown -> do
      let UnknownType{unknownSign, unknownConst} = unknown
      case unknownSign of
        Nothing ->
          return $
              PartialKnown . KnownType
            $ (if unknownConst then C.TypeQualified C.TypeQualifierConst else id)
            $ typ
        Just sign ->
          unexpected $ show (typ, sign)
    other ->
      unexpected $ show other

setSign :: Update C.PrimSign PartialType
setSign sign = \case
    PartialUnknown unknown ->
      return $ PartialUnknown $ Optics.set #unknownSign (Just sign) unknown
    other ->
      unexpected $ show other

-- | Transition from unknown types to known types
instance Apply (LanC.CTypeSpecifier a) PartialType where
  apply = \case
      -- Void (for function result types only)
      LanC.CVoidType _a -> notFun $ C.TypeVoid

      -- Primitive types
      LanC.CCharType   _a -> withSign $ C.PrimChar . charSign
      LanC.CShortType  _a -> withSign $ C.PrimIntegral C.PrimShort . fromMaybe C.Signed
      LanC.CIntType    _a -> withSign $ C.PrimIntegral C.PrimInt   . fromMaybe C.Signed
      LanC.CLongType   _a -> withSign $ C.PrimIntegral C.PrimLong  . fromMaybe C.Signed
      LanC.CFloatType  _a -> notFun $ C.TypePrim $ C.PrimFloating C.PrimFloat
      LanC.CDoubleType _a -> notFun $ C.TypePrim $ C.PrimFloating C.PrimDouble
      LanC.CBoolType   _a -> notFun $ C.TypePrim $ C.PrimBool

      -- Complex types
      LanC.CComplexType _a -> \case
        PartialKnown (KnownType (C.TypePrim prim)) ->
          return $ PartialKnown . KnownType $ C.TypeComplex prim
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

      typeRef :: C.DeclName -> C.Type HandleMacros
      typeRef name = C.TypeRef $ C.DeclId{name, isAnon = False}

      checkNotAnon :: Maybe LanC.Ident -> C.TagKind -> FromLanC C.DeclName
      checkNotAnon mName tagKind =
          case mName of
            Just name ->
              return $ C.DeclName (mkCName name) (C.NameKindTagged tagKind)
            Nothing ->
              unsupported $ "Anonymous " ++ show tagKind

      checkNoDef :: String -> Maybe def -> FromLanC ()
      checkNoDef _   Nothing  = return ()
      checkNoDef err (Just _) = unsupported err

instance Apply (LanC.CTypeQualifier a) PartialType where
  apply qual = \case
      PartialKnown   typ -> PartialKnown   <$> apply qual typ
      PartialUnknown typ -> PartialUnknown <$> apply qual typ

instance Apply (LanC.CDerivedDeclarator a) PartialType where
  apply deriv partial = case partial of
      PartialUnknown{} ->
        unexpected $ show (nodeOmitted deriv, partial)
      PartialKnown typ ->
        PartialKnown <$> apply deriv typ

{-------------------------------------------------------------------------------
  'UnknownType'
-------------------------------------------------------------------------------}

instance Apply (LanC.CTypeQualifier a) UnknownType where
  apply = \case
      LanC.CConstQual _a ->
        return . Optics.set #unknownConst True
      other -> \_ ->
        unexpectedF other

{-------------------------------------------------------------------------------
  'KnownType'
-------------------------------------------------------------------------------}

defaultApplyKnownType :: Apply a (C.Type HandleMacros) => Update a KnownType
defaultApplyKnownType x = fmap KnownType . apply x . fromKnownType

instance Apply (LanC.CTypeQualifier a) KnownType where
  apply = defaultApplyKnownType

instance Apply (LanC.CDerivedDeclarator a) KnownType where
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

instance Apply (LanC.CTypeQualifier a) (C.Type HandleMacros) where
  apply = \case
      LanC.CConstQual _ -> return . C.TypeQualified C.TypeQualifierConst
      LanC.CRestrQual _ -> return -- ignore @__restrict@
      other             -> \_ -> unexpectedF other

instance Apply (LanC.CDerivedDeclarator a) (C.Type HandleMacros) where
  apply = \case
      LanC.CPtrDeclr quals _a ->
        repeatedly apply quals . C.TypePointers 1
      LanC.CArrDeclr quals (LanC.CNoArrSize isCompleteType) _a ->
        if isCompleteType
          then \_ -> unexpected "complete array without size"
          else repeatedly apply quals . C.TypeIncompleteArray
      LanC.CArrDeclr quals (LanC.CArrSize _isStatic expr) _a -> \typ -> do
        sz <- case expr of
                LanC.CConst (LanC.CIntConst n _a') ->
                  return $ fromIntegral $ LanC.getCInteger n
                other ->
                  unsupported $ show (nodeOmitted other)
        -- TODO: Should we do something with _isStatic?
        repeatedly apply quals $ C.TypeConstArray sz typ
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
  -> FromLanC [LanC.CDeclaration a]
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
