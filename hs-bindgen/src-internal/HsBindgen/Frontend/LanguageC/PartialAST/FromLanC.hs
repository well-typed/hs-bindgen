{-# LANGUAGE CPP #-}

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
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
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

mkDecl :: LanC.CDeclaration a -> FromLanC (Maybe CName, C.Type ReparseMacroExpansions)
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
      LanC.CTypeSpec x -> overM #typ $ apply x
      LanC.CTypeQual x -> overM #typ $ apply x

      LanC.CStorageSpec x ->
        case x of
         -- We ignore the @typedef@ specifier: when reparsing typedefs it adds
         -- no information, and elsewhere we don't expect it at all (we /could/
         -- in principle check in such cases that it's not there, to catch
         -- potential bugs, at the cost of some increased code complexity).
          LanC.CTypedef _a -> return

          -- TODO <https://github.com/well-typed/hs-bindgen/issues/1768>
          -- We could deal other storage specifiers (auto, static, extern, ..).
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
        >=> repeatedly (overM #typ . apply) (reverse derived)
    where
      setName :: LanC.Ident -> PartialDecl -> FromLanC PartialDecl
      setName name = return . Optics.set #name (Just $ mkCName name)

{-------------------------------------------------------------------------------
  'PartialType'
-------------------------------------------------------------------------------}

notFun :: Update (C.Type ReparseMacroExpansions) PartialType
notFun typ = \case
    PartialUnknown unknown -> do
      if null unknown.base && null unknown.sign && null unknown.size then
          return $
              PartialKnown . KnownType
            $ (if unknown.isConst then C.TypeQual C.QualConst else id)
            $ typ
      else
          unexpected $ show (typ, unknown)
    other ->
      unexpected $ show other

addSign :: Update C.PrimSign PartialType
addSign sign = \case
    PartialUnknown unknown ->
      return $ PartialUnknown $ unknown & #sign %~ (sign:)
    other@PartialKnown{} ->
      unexpected $ show other

addBase :: Update Base PartialType
addBase base = \case
    PartialUnknown unknown ->
      return $ PartialUnknown $ unknown & #base %~ (base:)
    other@PartialKnown{} ->
      unexpected $ show other

addSize :: Update Size PartialType
addSize size = \case
    PartialUnknown unknown ->
      return $ PartialUnknown $ unknown & #size %~ (size:)
    other@PartialKnown{} ->
      unexpected $ show other

addComplex :: Update () PartialType
addComplex () = \case
    PartialUnknown unknown ->
      return $ PartialUnknown $ unknown & #isComplex .~ True
    other@PartialKnown{} ->
      unexpected $ show other

-- | Transition from unknown types to known types
instance Apply (LanC.CTypeSpecifier a) PartialType where
  apply = \case
      -- Void (for function result types only)
      LanC.CVoidType _a -> notFun $ C.TypeVoid

      -- Primitive types
      LanC.CCharType   _a -> addBase Char
      LanC.CShortType  _a -> addSize Short
      LanC.CIntType    _a -> addBase Int
      LanC.CLongType   _a -> addSize Long
      LanC.CFloatType  _a -> addBase Float
      LanC.CDoubleType _a -> addBase Double
#if MIN_VERSION_language_c(0,10,2)
      -- language-c 0.10.2 lexes both @bool@ and @_Bool@ as @CBoolType@.
      -- If @bool@ has been redefined (via @#define@ or @typedef@), the
      -- reparse env will contain that definition and we use it here.
      -- The standard stdbool.h alias @#define bool _Bool@ is normalised
      -- away by 'addKnownType' in the ReparseMacroExpansions pass, so the
      -- lookup returns @TypePrim PrimBool@ in that case.
      LanC.CBoolType   _a -> \partial -> do
        typeEnv <- getReparseEnv
        case Map.lookup "bool" typeEnv of
          Just typ -> notFun typ partial
          Nothing  -> notFun (C.TypePrim C.PrimBool) partial
#else
      LanC.CBoolType   _a -> notFun $ C.TypePrim $ C.PrimBool
#endif

      -- Complex types
      LanC.CComplexType _a -> addComplex ()

      -- Sign specifiers
      LanC.CSignedType _a -> addSign C.Signed
      LanC.CUnsigType  _a -> addSign C.Unsigned

      -- Unsupported types
      LanC.CInt128Type{}   -> \_ -> unsupported "CInt128Type"
#if MIN_VERSION_language_c(0,9,2)
      LanC.CUInt128Type{}  -> \_ -> unsupported "CUInt128Type"
#endif
#if MIN_VERSION_language_c(0,10,0)
      LanC.CBFloat16Type{} -> \_ -> unsupported "CBFloat16Type"
#endif
      LanC.CFloatNType{}   -> \_ -> unsupported "CFloatNType"
      LanC.CTypeOfExpr{}   -> \_ -> unsupported "CTypeOfExpr"
      LanC.CTypeOfType{}   -> \_ -> unsupported "CTypeOfType"
      LanC.CAtomicType{}   -> \_ -> unsupported "CAtomicType"

      -- User-defined types
      LanC.CSUType (LanC.CStruct su mTag mDef _attrs _a) _a' -> \partial -> do
        let tagKind = case su of
                        LanC.CStructTag -> CTagKindStruct
                        LanC.CUnionTag  -> CTagKindUnion
        name <- checkNotAnon mTag tagKind
        checkNoDef "struct or union definition" mDef
        notFun (typeRef name) partial
      LanC.CEnumType (LanC.CEnum mTag mDef _attrs _a) _a' -> \partial -> do
        name <- checkNotAnon mTag CTagKindEnum
        checkNoDef "enum definition" mDef
        typeEnv <- getReparseEnv
        case Map.lookup (renderCDeclNameC name) typeEnv of
          Nothing  -> unexpected $ "enum reference " ++ show name
          Just typ -> notFun typ partial
      LanC.CTypeDef name _a -> \partial -> do
        let name' = mkCName name
        typeEnv <- getReparseEnv
        case Map.lookup name' typeEnv of
          Nothing  -> unexpected $ "typedef reference " ++ show name
          Just typ -> notFun typ partial
    where
      typeRef :: CDeclName -> C.Type ReparseMacroExpansions
      typeRef name = C.TypeRef $ DeclId{name = name, isAnon = False}

      checkNotAnon :: Maybe LanC.Ident -> CTagKind -> FromLanC CDeclName
      checkNotAnon mName cTagKind =
          case mName of
            Just name ->
              return $ CDeclName (mkCName name) (CNameKindTagged cTagKind)
            Nothing ->
              unsupported $ "Anonymous " ++ show cTagKind

      checkNoDef :: String -> Maybe def -> FromLanC ()
      checkNoDef _   Nothing  = return ()
      checkNoDef err (Just _) = unsupported err

instance Apply (LanC.CTypeQualifier a) PartialType where
  apply qual = \case
      PartialKnown   typ -> PartialKnown   <$> apply qual typ
      PartialUnknown typ -> PartialUnknown <$> apply qual typ

instance Apply (LanC.CDerivedDeclarator a) PartialType where
  apply deriv partial = do
      typ <- fromPartialType partial
      PartialKnown <$> apply deriv typ

{-------------------------------------------------------------------------------
  'UnknownType'
-------------------------------------------------------------------------------}

instance Apply (LanC.CTypeQualifier a) UnknownType where
  apply = \case
      LanC.CConstQual _a ->
        return . Optics.set #isConst True
      other -> \_ ->
        unexpectedF other

{-------------------------------------------------------------------------------
  'KnownType'
-------------------------------------------------------------------------------}

defaultApplyKnownType :: Apply a (C.Type ReparseMacroExpansions) => Update a KnownType
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

instance Apply (LanC.CTypeQualifier a) (C.Type ReparseMacroExpansions) where
  apply = \case
      LanC.CConstQual _ -> return . C.TypeQual C.QualConst
      LanC.CRestrQual _ -> return -- ignore @__restrict@
      other             -> \_ -> unexpectedF other

instance Apply (LanC.CDerivedDeclarator a) (C.Type ReparseMacroExpansions) where
  apply = \case
      LanC.CPtrDeclr quals _a ->
        repeatedly apply quals . C.TypePointers 1
      LanC.CArrDeclr quals (LanC.CNoArrSize isCompleteType) _a ->
        if isCompleteType
          then \_ -> unexpected "complete array without size"
          else repeatedly apply quals . C.TypeIncompleteArray
      LanC.CArrDeclr quals (LanC.CArrSize _isStatic expr) _a -> \typ -> do
        -- For arrays of known size, we only support constant sizes
        sz <- case expr of
                LanC.CConst (LanC.CIntConst n _a') ->
                  return $ fromIntegral $ LanC.getCInteger n
                other ->
                  unsupported $ show (nodeOmitted other)
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
