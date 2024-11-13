{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Low-level translation of the C header to a Haskell module
--
-- TODO: This module is intended to implement the following milestones:
--
-- * Milestone 1: @Storable@ instances
--   <https://github.com/well-typed/hs-bindgen/milestone/2>
-- * Milestone 2: Low-level API
--   <https://github.com/well-typed/hs-bindgen/milestone/3>
module HsBindgen.Translation.LowLevel
  ( generateDeclarations
  , integralTyp, floatingTyp
  ) where

import Data.Foldable
import Data.Maybe
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Type.Nat
import Data.Vec.Lazy (Vec (..))
import Data.Vec.Lazy qualified as Vec

import HsBindgen.Imports
import HsBindgen.C.AST qualified as C
import HsBindgen.C.Tc.Macro qualified as C
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Util.PHOAS
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

generateDeclarations :: C.Header -> [Hs.Decl f]
generateDeclarations = getList . toHs

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

class ToHs (a :: Star) where
  type InHs a :: PHOAS
  toHs :: a -> InHs a f

instance ToHs C.Header where
  type InHs C.Header = List Hs.Decl
  toHs (C.Header decs) = List $ concatMap getList (map toHs decs)

instance ToHs C.Decl where
  type InHs C.Decl = List Hs.Decl
  toHs (C.DeclStruct struct) = reifyStructFields struct $ structDecs struct
  toHs (C.DeclEnum e)        = enumDecs e
  toHs (C.DeclTypedef d)     = typedefDecs d
  toHs (C.DeclMacro m)       = macroDecs m

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

reifyStructFields ::
     C.Struct
  -> (forall n. SNatI n => Vec n C.StructField -> a)
  -> a
reifyStructFields struct k = Vec.reifyList (C.structFields struct) k

-- | Generate declarations for given C struct
--
-- This is just a first sketch so far.
--
-- TODO:
--
-- * We currently generate only the 'Storable' instance. We should also
--   generate the @data@ declaration.
-- * Name mangling
-- * Deal with untagged structs.
-- * ..
structDecs :: forall n f.
     SNatI n
  => C.Struct -> Vec n C.StructField -> List Hs.Decl f
structDecs struct fields = List
    [ Hs.DeclData $ Hs.WithStruct hs Hs.MkDataDecl
    , Hs.DeclInstance $ Hs.InstanceStorable storable
    ]
  where
    hs :: Hs.Struct n
    hs =
      let cStructName = fromMaybe "X" $ C.structTag struct
          nm@NameMangler{..} = defaultNameMangler
          typeConstrCtx = TypeConstrContext cStructName
          structName = mangleTypeConstrName typeConstrCtx
          structConstr = mangleConstrName $ ConstrContext typeConstrCtx
          mkField f =
            ( mangleVarName $ FieldVarContext typeConstrCtx True (C.fieldName f)
            , typ nm (C.fieldType f)
            )
          structFields = Vec.map mkField fields
      in  Hs.Struct{..}

    storable :: Hs.WithStruct Hs.StorableInstance f
    storable = Hs.WithStruct hs $ Hs.StorableInstance {
          Hs.storableSizeOf    = C.structSizeof struct
        , Hs.storableAlignment = C.structAlignment struct
        , Hs.storablePeek      = Hs.Lambda $ \ptr ->
                                  Hs.Ap (Hs.IntroStruct hs) $
                                    map (peek ptr) (C.structFields struct)
        , Hs.storablePoke      = Hs.Lambda $ \ptr ->
                                   Hs.ElimStruct hs $ \xs -> Hs.Seq . List $
                                     toList $ Vec.zipWith (poke ptr) fields xs
        }

    peek :: f Bound -> C.StructField -> Hs.PeekByteOff f
    peek ptr f = Hs.PeekByteOff ptr (C.fieldOffset f `div` 8)

    poke :: f Bound -> C.StructField -> f Bound -> Hs.PokeByteOff f
    poke ptr f i = Hs.PokeByteOff ptr (C.fieldOffset f `div` 8) i

{-------------------------------------------------------------------------------
  Enum
-------------------------------------------------------------------------------}

enumDecs :: forall f.
  C.Enu -> List Hs.Decl f
enumDecs e = List [
      Hs.DeclNewtype newtype_
    , Hs.DeclInstance $ Hs.InstanceStorable storable
    ]
  where
    newtype_ :: Hs.Newtype
    newtype_ =
      let cEnumName = fromMaybe "X" $ C.enumTag e
          nm@NameMangler{..} = defaultNameMangler
          typeConstrCtx = TypeConstrContext cEnumName
          newtypeName = mangleTypeConstrName typeConstrCtx
          newtypeConstr = mangleConstrName $ ConstrContext typeConstrCtx
          newtypeField = mangleVarName $ EnumVarContext typeConstrCtx
          newtypeType    = typ nm (C.enumType e)
      in Hs.Newtype {..}

    hs :: Hs.Struct (S Z)
    hs =
      let cEnumName = fromMaybe "X" $ C.enumTag e
          nm@NameMangler{..} = defaultNameMangler
          typeConstrCtx = TypeConstrContext cEnumName
          structName = mangleTypeConstrName typeConstrCtx
          structConstr = mangleConstrName $ ConstrContext typeConstrCtx
          structFields = Vec.singleton
            ( mangleVarName $ EnumVarContext typeConstrCtx
            , typ nm (C.enumType e)
            )
      in  Hs.Struct{..}

    storable :: Hs.WithStruct Hs.StorableInstance f
    storable = Hs.WithStruct hs $ Hs.StorableInstance {
          Hs.storableSizeOf    = C.enumSizeof e
        , Hs.storableAlignment = C.enumAlignment e
        , Hs.storablePeek      = Hs.Lambda $ \ptr ->
                                  Hs.Ap (Hs.IntroStruct hs) $
                                    [ peek ptr ]
        , Hs.storablePoke      = Hs.Lambda $ \ptr ->
                                   Hs.ElimStruct hs $ \xs -> Hs.Seq . List $
                                     [ poke ptr (Vec.head xs) ]
        }

    peek :: f Bound -> Hs.PeekByteOff f
    peek ptr = Hs.PeekByteOff ptr 0

    poke :: f Bound -> f Bound -> Hs.PokeByteOff f
    poke ptr i = Hs.PokeByteOff ptr 0 i

{-------------------------------------------------------------------------------
  Typedef
-------------------------------------------------------------------------------}

typedefDecs :: C.Typedef -> List Hs.Decl f
typedefDecs d = List [
      Hs.DeclNewtype newtype_
    , Hs.DeclNewtypeInstance Hs.Storable newtypeName
    ]
  where
    cName              = C.typedefName d
    nm@NameMangler{..} = defaultNameMangler
    typeConstrCtx      = TypeConstrContext cName
    newtypeName        = mangleTypeConstrName typeConstrCtx

    newtype_ :: Hs.Newtype
    newtype_ = Hs.Newtype {..}
      where
        newtypeConstr = mangleConstrName $ ConstrContext typeConstrCtx
        newtypeField  = mangleVarName $ EnumVarContext typeConstrCtx
        newtypeType   = typ nm (C.typedefType d)

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

macroDecs :: C.MacroDecl -> List Hs.Decl f
macroDecs C.MacroDecl { macroDeclMacro = m, macroDeclMacroTy = ty }
    | C.QuantTy bf <- ty
    , C.isPrimTy bf
    = macroDecsTypedef m

    | otherwise
    = macroVarDecs m ty
    where

macroDecs C.MacroReparseError {} = List []
macroDecs C.MacroTcError {}      = List []

macroDecsTypedef :: C.Macro -> List Hs.Decl f
macroDecsTypedef m = List [
        Hs.DeclNewtype newtype_
      ]
  where
    newtype_ :: Hs.Newtype
    newtype_ =
      let cName = C.macroName m
          nm@NameMangler{..} = defaultNameMangler
          typeConstrCtx = TypeConstrContext cName
          newtypeName = mangleTypeConstrName typeConstrCtx
          newtypeConstr = mangleConstrName $ ConstrContext typeConstrCtx
          newtypeField = mangleVarName $ EnumVarContext typeConstrCtx

          -- TODO: this type conversion is very simple, but works for now.
          newtypeType    = typ nm $ case C.macroBody m of
              C.MTerm (C.MType pt) -> C.TypPrim pt
              _                    -> C.TypPrim C.PrimVoid --

      in Hs.Newtype {..}

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

typ :: NameMangler -> C.Typ -> Hs.HsType
typ NameMangler{..} (C.TypElaborated c) =
  Hs.HsTypRef (mangleTypeConstrName (TypeConstrContext c)) -- wrong
typ _     (C.TypStruct s)      =Hs.HsType (show (C.structTag s)) -- also wrong
typ _     (C.TypPrim p)       = case p of
  C.PrimVoid                   -> Hs.HsPrimType HsPrimVoid
  C.PrimChar Nothing           -> Hs.HsPrimType HsPrimCChar
  C.PrimChar (Just C.Signed)   -> Hs.HsPrimType HsPrimCSChar
  C.PrimChar (Just C.Unsigned) -> Hs.HsPrimType HsPrimCSChar
  C.PrimIntegral i -> Hs.HsPrimType $ integralTyp i
  C.PrimFloating f -> Hs.HsPrimType $ floatingTyp f
typ nm (C.TypPointer t)    = Hs.HsPtr (typ nm t)
typ nm (C.TypConstArray n ty) = Hs.HsConstArray n (typ nm ty)

integralTyp :: C.PrimIntType -> HsPrimType
integralTyp = \case
  C.PrimInt C.Signed        -> HsPrimCInt
  C.PrimInt C.Unsigned      -> HsPrimCUInt
  C.PrimShort C.Signed      -> HsPrimCShort
  C.PrimShort C.Unsigned    -> HsPrimCUShort
  C.PrimLong C.Signed       -> HsPrimCLong
  C.PrimLong C.Unsigned     -> HsPrimCULong
  C.PrimLongLong C.Signed   -> HsPrimCLLong
  C.PrimLongLong C.Unsigned -> HsPrimCULLong

floatingTyp :: C.PrimFloatType -> HsPrimType
floatingTyp = \case
  C.PrimFloat      -> HsPrimCFloat
  C.PrimDouble     -> HsPrimCDouble
  C.PrimLongDouble -> HsPrimCDouble -- wrong (see #247)

{-------------------------------------------------------------------------------
  Macro
-------------------------------------------------------------------------------}

macroVarDecs :: C.Macro -> C.QuantTy -> List Hs.Decl f
macroVarDecs (C.Macro { macroName = cVarNm, macroArgs = args, macroBody = body } ) qty =
  List [
    Hs.DeclVar $
      Hs.VarDecl
        { varDeclName = hsVarName
        , varDeclType = quantTyHsTy qty
        , varDeclBody = hsBody
        }
  | hsBody <- maybeToList $ macroLamHsExpr cVarNm args body
  ]
  where
    hsVarName = mangleVarName defaultNameMangler $ VarContext cVarNm

quantTyHsTy :: C.QuantTy -> Hs.SigmaType f
quantTyHsTy qty@( C.QuantTy @n _ ) =
  case C.mkQuantTyBody qty of
    C.QuantTyBody { quantTyQuant = cts, quantTyBody = ty } -> do
      goForallTy (C.tyVarNames @n) cts ty
  where
    goTy :: Map Text ( f Bound ) -> C.Type C.Ty -> Hs.TauType f
    goTy bound = \case
      C.TyVarTy tv -> Hs.TyVarTy $ bound Map.! ( C.tyVarName tv )
      C.FunTy as r ->
        foldr (Hs.FunTy . goTy bound) (goTy bound r) as
      C.TyConAppTy ( C.DataTyCon tc ) as ->
        Hs.TyConAppTy ( Hs.TyConApp tc $ fmap ( goTy bound ) as )
    goCt :: Map Text ( f Bound ) -> C.Type C.Ct -> Hs.ClassTy f
    goCt bound = \case
      C.TyConAppTy ( C.ClassTyCon tc ) as ->
        Hs.ClassTy tc ( fmap ( goTy bound ) as )
    goForallTy :: Vec n Text -> [ C.Type C.Ct ] -> C.Type C.Ty -> Hs.SigmaType f
    goForallTy args cts body =
      Hs.ForallTy (fmap mkHsName args) $ Hs.Forall $ \ qtvs ->
        let bound = Map.fromList $ toList $ Vec.zipWith (,) args qtvs
        in  Hs.QuantTy ( fmap ( goCt bound ) cts ) ( goTy bound body )

macroLamHsExpr :: C.CName -> [C.CName] -> C.MExpr -> Maybe (Hs.VarDeclRHS f)
macroLamHsExpr macroNm macroArgs expr = ($ Map.empty) <$> go Set.empty macroArgs
  where
    go :: Set C.CName -> [C.CName] -> Maybe (Map C.CName (f Bound) -> Hs.VarDeclRHS f)
    go varSet [] = ( . (Map.!) ) <$> macroExprHsExpr macroNm varSet expr
    go varSet (argNm:args) =
      case go (Set.insert argNm varSet) args of
        Nothing -> Nothing
        Just e ->
          Just $ \ bound0 ->
            let hsArgNm = mangleVarName defaultNameMangler $ VarContext argNm
            in
              Hs.VarDeclLambda hsArgNm $
                Hs.Lambda $ \ bound ->
                  e (Map.insert argNm bound bound0)

macroExprHsExpr :: C.CName -> Set C.CName -> C.MExpr -> Maybe ((C.CName -> f Bound) -> Hs.VarDeclRHS f)
macroExprHsExpr macroNm macroArgs = goExpr
  where
    goExpr :: C.MExpr -> Maybe ((C.CName -> f Bound) -> Hs.VarDeclRHS f)
    goExpr = \case
      C.MTerm tm -> goTerm tm
      C.MApp fun args ->
        goApp (Hs.InfixAppHead fun) (toList args)

    goTerm :: C.MTerm -> Maybe ((C.CName -> f Bound) -> Hs.VarDeclRHS f)
    goTerm = \case
      C.MEmpty -> Nothing
      C.MInt i -> fmap const $ goInt i
      C.MFloat f -> fmap const $ goFloat f
      varApp@(C.MVar nm args) ->
        if nm `Set.member` macroArgs
        then
          if null args
          then
            return $
              \ lk -> Hs.VarDeclVar (lk nm)
          else
            error $ unlines
              [ "'macroExprHsExpr': macro argument used as a function"
              , "     macro: " ++ show macroNm
              , "  argument: " ++ show varApp
              ]
        else
          let hsVar = mangleVarName defaultNameMangler $ VarContext nm
          in  goApp (Hs.VarAppHead hsVar) args

      C.MType {} -> Nothing
      C.MAttr _attr tm' -> goTerm tm'
      C.MStringize {} -> Nothing
      C.MConcat {} -> Nothing

    goApp :: Hs.VarDeclRHSAppHead -> [C.MExpr] -> Maybe ((C.CName -> f Bound) -> Hs.VarDeclRHS f)
    goApp appHead args = do
      args' <- traverse goExpr args
      return $ \ lk -> Hs.VarDeclApp appHead (map ( $ lk ) args')

    goInt :: C.IntegerLiteral -> Maybe (Hs.VarDeclRHS f)
    goInt (C.IntegerLiteral { integerLiteralType = mbIntTy, integerLiteralValue = i }) =
      Just $ Hs.VarDeclIntegral i (maybe HsPrimCInt integralTyp mbIntTy)

    goFloat :: C.FloatingLiteral -> Maybe (Hs.VarDeclRHS f)
    goFloat flt@(C.FloatingLiteral { floatingLiteralType = mbFty }) =
      case mbFty of
        Nothing -> Just $ Hs.VarDeclDouble (C.floatingLiteralDoubleValue flt)
        Just fty ->
          case fty of
            C.PrimFloat  -> Just $ Hs.VarDeclFloat (C.floatingLiteralFloatValue flt)
            C.PrimDouble -> Just $ Hs.VarDeclDouble (C.floatingLiteralDoubleValue flt)
            C.PrimLongDouble -> Nothing
