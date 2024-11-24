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
module HsBindgen.Hs.Translation (
    generateDeclarations,
    -- * leaky exports:
    --   perfectly, translation will happen in *this* module.
    integralType,
    floatingType,
) where

import Data.Type.Nat (SNatI, induction)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vec.Lazy qualified as Vec

import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.C.AST qualified as C
import HsBindgen.C.Tc.Macro qualified as C
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type

import DeBruijn (Idx (..), pattern I1, weaken, Add (..), pattern I2, EmptyCtx, Size (..))

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

generateDeclarations :: C.Header -> [Hs.Decl]
generateDeclarations = toHs

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

class ToHs (a :: Star) where
  type InHs a :: Star
  toHs :: a -> InHs a

instance ToHs C.Header where
  type InHs C.Header = [Hs.Decl]
  toHs (C.Header decs) = concatMap toHs decs

instance ToHs C.Decl where
  type InHs C.Decl = [Hs.Decl]
  toHs (C.DeclStruct struct)  = reifyStructFields struct $ structDecs struct
  toHs (C.DeclOpaqueStruct n) = opaqueStructDecs n
  toHs (C.DeclEnum e)         = enumDecs e
  toHs (C.DeclTypedef d)      = typedefDecs d
  toHs (C.DeclMacro m)        = macroDecs m

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
structDecs :: forall n.
     SNatI n
  => C.Struct -> Vec n C.StructField -> [Hs.Decl]
structDecs struct fields =
    [ Hs.DeclData hs
    , Hs.DeclInstance $ Hs.InstanceStorable hs storable
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

    storable :: Hs.StorableInstance
    storable = Hs.StorableInstance {
          Hs.storableSizeOf    = C.structSizeof struct
        , Hs.storableAlignment = C.structAlignment struct
        , Hs.storablePeek      = Hs.Lambda "ptr" $
            Hs.Ap (Hs.StructCon hs) $ map (peek IZ) (C.structFields struct)
        , Hs.storablePoke      = Hs.Lambda "ptr" $ Hs.Lambda "s" $
            Hs.makeElimStruct IZ hs $ \wk xs -> Hs.Seq $ toList $ Vec.zipWith (poke (weaken wk I1)) fields xs
        }

    peek :: Idx ctx -> C.StructField -> Hs.PeekByteOff ctx
    peek ptr f = Hs.PeekByteOff ptr (C.fieldOffset f `div` 8)

    poke :: Idx ctx -> C.StructField -> Idx ctx -> Hs.PokeByteOff ctx
    poke ptr f i = Hs.PokeByteOff ptr (C.fieldOffset f `div` 8) i

{-------------------------------------------------------------------------------
  Opaque struct
-------------------------------------------------------------------------------}

opaqueStructDecs :: C.CName -> [Hs.Decl]
opaqueStructDecs cname =
    [ Hs.DeclEmpty hsName
    ]
  where
    NameMangler{..} = defaultNameMangler
    typeConstrCtx = TypeConstrContext cname
    hsName = mangleTypeConstrName typeConstrCtx

{-------------------------------------------------------------------------------
  Enum
-------------------------------------------------------------------------------}

enumDecs :: C.Enu -> [Hs.Decl]
enumDecs e = [
      Hs.DeclNewtype newtype_
    , Hs.DeclInstance $ Hs.InstanceStorable hs storable
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

    storable :: Hs.StorableInstance
    storable = Hs.StorableInstance {
          Hs.storableSizeOf    = C.enumSizeof e
        , Hs.storableAlignment = C.enumAlignment e
        , Hs.storablePeek      = Hs.Lambda "ptr" $
            Hs.Ap (Hs.StructCon hs) [ peek IZ 0 ]
        , Hs.storablePoke      = Hs.Lambda "ptr" $ Hs.Lambda "s" $
            Hs.ElimStruct IZ hs (AS AZ) $ Hs.Seq [ poke I2 0 IZ ]
        }

    peek :: Idx ctx -> Int -> Hs.PeekByteOff ctx
    peek = Hs.PeekByteOff

    poke :: Idx ctx -> Int -> Idx ctx -> Hs.PokeByteOff ctx
    poke = Hs.PokeByteOff

{-------------------------------------------------------------------------------
  Typedef
-------------------------------------------------------------------------------}

typedefDecs :: C.Typedef -> [Hs.Decl]
typedefDecs d = [
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

macroDecs :: C.MacroDecl -> [Hs.Decl]
macroDecs C.MacroDecl { macroDeclMacro = m, macroDeclMacroTy = ty }
    | C.QuantTy bf <- ty
    , C.isPrimTy bf
    = macroDecsTypedef m

    | otherwise
    = macroVarDecs m ty
    where

macroDecs C.MacroReparseError {} = []
macroDecs C.MacroTcError {}      = []

macroDecsTypedef :: C.Macro -> [Hs.Decl]
macroDecsTypedef m = [
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
  C.PrimIntegral i -> Hs.HsPrimType $ integralType i
  C.PrimFloating f -> Hs.HsPrimType $ floatingType f
typ nm (C.TypPointer t)    = Hs.HsPtr (typ nm t)
typ nm (C.TypConstArray n ty) = Hs.HsConstArray n (typ nm ty)

integralType :: C.PrimIntType -> HsPrimType
integralType = \case
  C.PrimInt C.Signed        -> HsPrimCInt
  C.PrimInt C.Unsigned      -> HsPrimCUInt
  C.PrimShort C.Signed      -> HsPrimCShort
  C.PrimShort C.Unsigned    -> HsPrimCUShort
  C.PrimLong C.Signed       -> HsPrimCLong
  C.PrimLong C.Unsigned     -> HsPrimCULong
  C.PrimLongLong C.Signed   -> HsPrimCLLong
  C.PrimLongLong C.Unsigned -> HsPrimCULLong

floatingType :: C.PrimFloatType -> HsPrimType
floatingType = \case
  C.PrimFloat      -> HsPrimCFloat
  C.PrimDouble     -> HsPrimCDouble
  C.PrimLongDouble -> HsPrimCDouble -- wrong (see #247)

{-------------------------------------------------------------------------------
  Macro
-------------------------------------------------------------------------------}

macroVarDecs :: C.Macro -> C.QuantTy -> [Hs.Decl]
macroVarDecs (C.Macro { macroName = cVarNm, macroArgs = args, macroBody = body } ) qty =
  [
    Hs.DeclVar $
      Hs.VarDecl
        { varDeclName = hsVarName
        , varDeclType = quantTyHsTy qty
        , varDeclBody = hsBody
        }
  | hsBody <- toList $ macroLamHsExpr cVarNm args body
  ]
  where
    hsVarName = mangleVarName defaultNameMangler $ VarContext cVarNm

quantTyHsTy :: C.QuantTy -> Hs.SigmaType
quantTyHsTy qty@(C.QuantTy @n _) =
  case C.mkQuantTyBody qty of
    C.QuantTyBody { quantTyQuant = cts, quantTyBody = ty } -> do
      goForallTy (C.tyVarNames @n) cts ty
  where
    size :: Size n
    size = induction SZ SS

    goCt :: Map Text (Idx ctx) -> C.Type C.Ct -> Hs.ClassTy ctx
    goCt env (C.TyConAppTy (C.ClassTyCon tc) as) = Hs.ClassTy tc (fmap (goTy env) as)

    goTy :: Map Text (Idx ctx) -> C.Type C.Ty -> Hs.TauType ctx
    goTy env (C.TyVarTy tv) = Hs.TyVarTy (env Map.! C.tyVarName tv) -- XXX: partial Map.!
    goTy env (C.FunTy as r) =
      foldr (Hs.FunTy . goTy env) (goTy env r) as
    goTy env (C.TyConAppTy (C.DataTyCon tc) as) =
      Hs.TyConAppTy (Hs.TyConApp tc $ fmap (goTy env) as)

    goForallTy :: Vec n Text -> [ C.Type C.Ct ] -> C.Type C.Ty -> Hs.SigmaType
    goForallTy args cts body = Hs.ForallTy
        { forallTySize    = size
        , forallTyBinders = fmap (fromString . T.unpack) args
        , forallTy        = Hs.QuantTy
            { quantTyCts  = fmap (goCt env) cts
            , quantTyBody = goTy env body
            }
        }
      where
        env :: Map Text (Idx n)
        env = Map.fromList $ toList $ Vec.zipWith (,) args qtvs

        qtvs :: Vec n (Idx n)
        qtvs = unU (induction (U VNil) (\(U v) -> U (IZ ::: fmap IS v)))

newtype U n = U { unU :: Vec n (Idx n) }

macroLamHsExpr :: C.CName -> [C.CName] -> C.MExpr -> Maybe (Hs.VarDeclRHS EmptyCtx)
macroLamHsExpr _macroName macroArgs expr =
    makeNames macroArgs Map.empty
  where
    makeNames :: [C.CName] -> Map C.CName (Idx ctx) -> Maybe (Hs.VarDeclRHS ctx)
    makeNames []     env = macroExprHsExpr env expr
    makeNames (n:ns) env = Hs.VarDeclLambda . Hs.Lambda (cnameToHint n) <$> makeNames ns (Map.insert n IZ (fmap IS env))

cnameToHint :: C.CName -> NameHint
cnameToHint (C.CName t) = fromString (T.unpack t)

macroExprHsExpr :: Map C.CName (Idx ctx) -> C.MExpr -> Maybe (Hs.VarDeclRHS ctx)
macroExprHsExpr = goExpr where
    goExpr :: Map C.CName (Idx ctx) -> C.MExpr -> Maybe (Hs.VarDeclRHS ctx)
    goExpr env = \case
      C.MTerm tm -> goTerm env tm
      C.MApp fun args ->
        goApp env (Hs.InfixAppHead fun) (toList args)

    goTerm :: Map C.CName (Idx ctx) -> C.MTerm -> Maybe (Hs.VarDeclRHS ctx)
    goTerm env = \case
      C.MEmpty -> Nothing
      C.MInt i -> goInt i
      C.MFloat f -> goFloat f
      C.MVar nm args ->
        --  TODO: removed the macro arguement used as a function check.
        case Map.lookup nm env of
          Just i  -> return (Hs.VarDeclVar i)
          Nothing ->
            let hsVar = mangleVarName defaultNameMangler $ VarContext nm
            in  goApp env (Hs.VarAppHead hsVar) args

      C.MType {} -> Nothing
      C.MAttr _attr tm' -> goTerm env tm'
      C.MStringize {} -> Nothing
      C.MConcat {} -> Nothing

    goApp :: Map C.CName (Idx ctx) -> Hs.VarDeclRHSAppHead -> [C.MExpr] -> Maybe (Hs.VarDeclRHS ctx)
    goApp env appHead args = do
      args' <- traverse (goExpr env) args
      return $ Hs.VarDeclApp appHead args'

    goInt :: C.IntegerLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goInt (C.IntegerLiteral { integerLiteralType = mbIntTy, integerLiteralValue = i }) =
      Just $ Hs.VarDeclIntegral i (maybe HsPrimCInt integralType mbIntTy)

    goFloat :: C.FloatingLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goFloat flt@(C.FloatingLiteral { floatingLiteralType = mbFty }) =
      case mbFty of
        Nothing -> Just $ Hs.VarDeclDouble (C.floatingLiteralDoubleValue flt)
        Just fty ->
          case fty of
            C.PrimFloat  -> Just $ Hs.VarDeclFloat (C.floatingLiteralFloatValue flt)
            C.PrimDouble -> Just $ Hs.VarDeclDouble (C.floatingLiteralDoubleValue flt)
            C.PrimLongDouble -> Nothing