{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type

import DeBruijn
  (Idx (..), pattern I1, weaken, Add (..), pattern I2, EmptyCtx)

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
  toHs (C.DeclOpaqueStruct o) = opaqueStructDecs $ C.opaqueStructTag o
  toHs (C.DeclEnum e)         = enumDecs e
  toHs (C.DeclOpaqueEnum o)   = opaqueStructDecs $ C.opaqueEnumTag o -- TODO?
  toHs (C.DeclTypedef d)      = typedefDecs d
  toHs (C.DeclMacro m)        = macroDecs m
  toHs (C.DeclFunction f)     = functionDecs f

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
    ++ flamInstance
  where
    nm@NameMangler{..} = defaultNameMangler

    hs :: Hs.Struct n
    hs =
      let typeConstrCtx = StructTypeConstrContext $ C.structDeclPath struct
          structName = mangleTypeConstrName typeConstrCtx
          structConstr = mangleConstrName $ ConstrContext typeConstrCtx
          structFields = flip Vec.map fields $ \f -> Hs.Field {
              fieldName   = mangleVarName $
                FieldVarContext typeConstrCtx (C.fieldName f)
            , fieldType   = typ nm (C.fieldType f)
            , fieldOrigin = Hs.FieldOriginStructField f
            }
          structOrigin = Hs.StructOriginStruct struct
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

    flamInstance :: [Hs.Decl]
    flamInstance = case C.structFlam struct of
      Nothing  -> []
      Just flam -> singleton $ Hs.DeclInstance $ Hs.InstanceHasFLAM
        hs
        (typ nm (C.fieldType flam))
        (C.fieldOffset flam `div` 8)

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
      Hs.DeclNewtype Hs.Newtype{..}
    , Hs.DeclInstance $ Hs.InstanceStorable hs storable
    ] ++ valueDecls
  where
    cEnumName          = C.enumTag e
    nm@NameMangler{..} = defaultNameMangler
    typeConstrCtx      = TypeConstrContext cEnumName
    newtypeName        = mangleTypeConstrName typeConstrCtx
    newtypeConstr      = mangleConstrName $ ConstrContext typeConstrCtx
    newtypeField       = Hs.Field {
        fieldName   = mangleVarName $ EnumVarContext typeConstrCtx
      , fieldType   = typ nm (C.enumType e)
      , fieldOrigin = Hs.FieldOriginNone
      }
    newtypeOrigin      = Hs.NewtypeOriginEnum e

    hs :: Hs.Struct (S Z)
    hs =
      let structName = mangleTypeConstrName typeConstrCtx
          structConstr = mangleConstrName $ ConstrContext typeConstrCtx
          structFields = Vec.singleton newtypeField
          structOrigin = Hs.StructOriginEnum e
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

    valueDecls :: [Hs.Decl]
    valueDecls =
        [ Hs.DeclPatSyn Hs.PatSyn
          { patSynName   = mangleConstrName $ ConstrContext $ TypeConstrContext valueName
          , patSynType   = newtypeName
          , patSynConstr = newtypeConstr
          , patSynValue  = valueValue
          , patSynOrigin = Hs.PatSynOriginEnumValue enumValue
          }
        | enumValue@C.EnumValue{..} <- C.enumValues e
        ]

{-------------------------------------------------------------------------------
  Typedef
-------------------------------------------------------------------------------}

typedefDecs :: C.Typedef -> [Hs.Decl]
typedefDecs d = [
      Hs.DeclNewtype Hs.Newtype{..}
    , Hs.DeclNewtypeInstance Hs.Storable newtypeName
    ]
  where
    cName              = C.typedefName d
    nm@NameMangler{..} = defaultNameMangler
    typeConstrCtx      = TypeConstrContext cName
    newtypeName        = mangleTypeConstrName typeConstrCtx
    newtypeConstr      = mangleConstrName $ ConstrContext typeConstrCtx
    newtypeField       = Hs.Field {
        fieldName   = mangleVarName $ EnumVarContext typeConstrCtx
      , fieldType   = typ nm (C.typedefType d)
      , fieldOrigin = Hs.FieldOriginNone
      }
    newtypeOrigin      = Hs.NewtypeOriginTypedef d

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

macroDecs :: C.MacroDecl -> [Hs.Decl]
macroDecs C.MacroDecl { macroDeclMacro = m, macroDeclMacroTy = ty }
    | Macro.Quant bf <- ty
    , Macro.isPrimTy bf
    = macroDecsTypedef m

    | otherwise
    = macroVarDecs m ty
    where

macroDecs C.MacroReparseError {} = []
macroDecs C.MacroTcError {}      = []

macroDecsTypedef :: C.Macro -> [Hs.Decl]
macroDecsTypedef m =
    -- For now we only support primitive types
    case C.macroBody m of
      C.MTerm (C.MType ty) ->
        let newtypeField = mkField ty
        in [Hs.DeclNewtype Hs.Newtype{..}]
      _otherwise ->
        []
  where
    nm@NameMangler{..} = defaultNameMangler

    cName         = C.macroName m
    typeConstrCtx = TypeConstrContext cName
    newtypeName   = mangleTypeConstrName typeConstrCtx
    newtypeConstr = mangleConstrName $ ConstrContext typeConstrCtx
    newtypeOrigin = Hs.NewtypeOriginMacro m

    mkField :: C.Type -> Hs.Field
    mkField ty = Hs.Field {
          fieldName   = mangleVarName $ EnumVarContext typeConstrCtx
        , fieldType   = typ nm ty
        , fieldOrigin = Hs.FieldOriginNone
        }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data TypeContext =
    CTop     -- ^ Anything else
  | CFunArg  -- ^ Function argument
  | CFunRes  -- ^ Function result
  | CPtrArg  -- ^ Pointer argument
  deriving stock (Show)

typ :: NameMangler -> C.Type -> Hs.HsType
typ nm = go CTop
  where
    go :: TypeContext -> C.Type -> Hs.HsType
    go _ (C.TypeTypedef c) =
        Hs.HsTypRef (mangleTypeConstrName nm (TypeConstrContext c)) -- wrong
    go _ (C.TypeStruct declPath) =
        Hs.HsTypRef (mangleTypeConstrName nm (StructTypeConstrContext declPath))
    go _ (C.TypeEnum name) =
        Hs.HsTypRef (mangleTypeConstrName nm (TypeConstrContext name))
    go c C.TypeVoid =
        Hs.HsPrimType (goVoid c)
    go _ (C.TypePrim p) =
        Hs.HsPrimType (goPrim p)
    go _ (C.TypePointer t) = case t of
        C.TypeFun {} -> Hs.HsFunPtr (go CPtrArg t)
        _            -> Hs.HsPtr (go CPtrArg t)
    go _ (C.TypeConstArray n ty) =
        Hs.HsConstArray n (go CTop ty)
    go c (C.TypeIncompleteArray ty) =
        goArrayUnknownSize c ty
    go _ (C.TypeFun xs y) =
        foldr (\x res -> Hs.HsFun (go CFunArg x) res) (Hs.HsIO (go CFunRes y)) xs

    goPrim :: C.PrimType -> HsPrimType
    goPrim C.PrimBool                     = HsPrimCBool
    goPrim (C.PrimChar Nothing)           = HsPrimCChar
    goPrim (C.PrimChar (Just C.Signed))   = HsPrimCSChar
    goPrim (C.PrimChar (Just C.Unsigned)) = HsPrimCSChar
    goPrim (C.PrimIntegral i s)           = integralType i s
    goPrim (C.PrimFloating f)             = floatingType f
    goPrim C.PrimPtrDiff                  = HsPrimCPtrDiff

    goVoid :: TypeContext -> HsPrimType
    goVoid CFunRes = HsPrimUnit
    goVoid CPtrArg = HsPrimVoid
    goVoid c       = error $ "typ: unexpected void in context " ++ show c

    goArrayUnknownSize :: TypeContext -> C.Type -> HsType
    goArrayUnknownSize CFunArg t =
         -- Arrays of unknown size as function args are treated as pointers.
         -- <https://en.cppreference.com/w/c/language/array#Arrays_of_unknown_size>
         Hs.HsPtr $ go CTop t
    goArrayUnknownSize c _ =
        -- TODO <https://github.com/well-typed/hs-bindgen/issues/377>
        -- We need to extend 'TypeContext' with a context for extern
        -- declarations, and then allow for arrays of unknown size.
        error $ "typ: unexpected array of unknown size in context " ++ show c

integralType :: C.PrimIntType -> C.PrimSign -> HsPrimType
integralType C.PrimInt      C.Signed   = HsPrimCInt
integralType C.PrimInt      C.Unsigned = HsPrimCUInt
integralType C.PrimShort    C.Signed   = HsPrimCShort
integralType C.PrimShort    C.Unsigned = HsPrimCUShort
integralType C.PrimLong     C.Signed   = HsPrimCLong
integralType C.PrimLong     C.Unsigned = HsPrimCULong
integralType C.PrimLongLong C.Signed   = HsPrimCLLong
integralType C.PrimLongLong C.Unsigned = HsPrimCULLong

floatingType :: C.PrimFloatType -> HsPrimType
floatingType = \case
  C.PrimFloat      -> HsPrimCFloat
  C.PrimDouble     -> HsPrimCDouble
  C.PrimLongDouble -> HsPrimCDouble -- wrong (see #247)

{-------------------------------------------------------------------------------
  Function
-------------------------------------------------------------------------------}

functionDecs :: C.Function -> [Hs.Decl]
functionDecs f =
    [ Hs.DeclForeignImport $ Hs.ForeignImportDecl
        { foreignImportName       = mangleVarName nm $ VarContext $ C.functionName f
        , foreignImportType       = typ nm $ C.functionType f
        , foreignImportOrigName   = C.getCName (C.functionName f)
        , foreignImportHeader     = C.functionHeader f  -- TODO: https://github.com/well-typed/hs-bindgen/issues/333
        , foreignImportDeclOrigin = Hs.ForeignImportDeclOriginFunction f
        }
    ]
  where
    nm = defaultNameMangler

{-------------------------------------------------------------------------------
  Macro
-------------------------------------------------------------------------------}

macroVarDecs :: C.Macro -> Macro.Quant ( Macro.Type Macro.Ty ) -> [Hs.Decl]
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

quantTyHsTy :: Macro.Quant ( Macro.Type Macro.Ty ) -> Hs.SigmaType
quantTyHsTy qty@(Macro.Quant @kis _) =
  case Macro.mkQuantTyBody qty of
    Macro.QuantTyBody { quantTyQuant = cts, quantTyBody = ty } -> do
      goForallTy (Macro.tyVarNames @kis) cts ty
  where

    goCt :: Map Text (Idx ctx) -> Macro.Type Macro.Ct -> Hs.PredType ctx
    goCt env (Macro.TyConAppTy cls as) =
      Hs.DictTy (Hs.AClass cls) (goTys env as)
    goCt env (Macro.NomEqPred a b) =
      Hs.NomEqTy (goTy env a) (goTy env b)

    goTy :: Map Text (Idx ctx) -> Macro.Type Macro.Ty -> Hs.TauType ctx
    goTy env (Macro.TyVarTy tv) = Hs.TyVarTy (env Map.! Macro.tyVarName tv) -- XXX: partial Map.!
    goTy env (Macro.FunTy as r) =
      foldr (Hs.FunTy . goTy env) (goTy env r) as
    goTy env (Macro.TyConAppTy tc as) =
      Hs.TyConAppTy (Hs.ATyCon tc) (goTys env as)

    goTys :: Map Text (Idx ctx) -> Vec n ( Macro.Type Macro.Ty ) -> [ Hs.TauType ctx ]
    goTys env as = toList $ fmap (goTy env) as

    goForallTy :: forall n. SNatI n => Vec n (Int, Text) -> [ Macro.Type Macro.Ct ] -> Macro.Type Macro.Ty -> Hs.SigmaType
    goForallTy args cts body =
        let
          env :: Map Text (Idx n)
          env = Map.fromList $ toList $ Vec.zipWith (,) ( fmap snd args ) qtvs
          qtvs :: Vec n (Idx n)
          qtvs = unU (induction (U VNil) (\(U v) -> U (IZ ::: fmap IS v)))
        in
          Hs.ForallTy
            { forallTyBinders = fmap (fromString . T.unpack . snd) args
            , forallTy        = Hs.QuantTy
                { quantTyCts  = fmap (goCt env) cts
                , quantTyBody = goTy env body
                }
            }

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
      Just $ Hs.VarDeclIntegral i (maybe HsPrimCInt (uncurry integralType) mbIntTy)

    goFloat :: C.FloatingLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goFloat flt@(C.FloatingLiteral { floatingLiteralType = mbFty }) =
      case mbFty of
        Nothing -> Just $ Hs.VarDeclDouble (C.floatingLiteralDoubleValue flt)
        Just fty ->
          case fty of
            C.PrimFloat  -> Just $ Hs.VarDeclFloat (C.floatingLiteralFloatValue flt)
            C.PrimDouble -> Just $ Hs.VarDeclDouble (C.floatingLiteralDoubleValue flt)
            C.PrimLongDouble -> Nothing
