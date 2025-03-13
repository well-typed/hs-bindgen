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
    TranslationOpts(..)
  , defaultTranslationOpts
  , generateDeclarations
    -- * leaky exports:
    --   perfectly, translation will happen in *this* module.
  , integralType
  , floatingType
  ) where

import Data.Type.Nat (SNatI, induction)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vec.Lazy qualified as Vec
import GHC.Exts qualified as IsList (IsList(..))

import C.Char qualified as C

import HsBindgen.C.AST qualified as C
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.Clang.Paths
import HsBindgen.Errors
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Type
import HsBindgen.Hs.NameMangler
import HsBindgen.Imports
import HsBindgen.NameHint

import DeBruijn
  (Idx (..), pattern I1, weaken, Add (..), pattern I2, EmptyCtx)

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data TranslationOpts = TranslationOpts {
      -- | Default set of classes to derive for structs
      translationDeriveStruct :: [(Hs.Strategy, Hs.TypeClass)]

      -- | Default set of classes to derive for enums
    , translationDeriveEnum :: [(Hs.Strategy, Hs.TypeClass)]

      -- | Default set of classes to derive for typedefs around primitive types
      --
      -- The situation for typedefs is more intricate, because the instances we
      -- can generate depend on the instances available for the type we're
      -- defining a newtype for. However, for typedefs of /primitive/ types
      -- this is easier, as we do know which instances are available for those.
      --
      -- Any classes in this list that are /not/ supported by the underlying
      -- (primitive) type will simply not be generated, so it's okay for this
      -- to contain classes such as 'Num' which are only supported by /some/
      -- primitive types.
    , translationDeriveTypedefPrim :: [(Hs.Strategy, Hs.TypeClass)]
    }
  deriving stock (Show)

defaultTranslationOpts :: TranslationOpts
defaultTranslationOpts = TranslationOpts {
      translationDeriveStruct = [
          (Hs.DeriveStock, Hs.Show)
        , (Hs.DeriveStock, Hs.Eq)
        ]
    , translationDeriveEnum = [
          (Hs.DeriveStock, Hs.Show)
        , (Hs.DeriveStock, Hs.Read)
        , (Hs.DeriveStock, Hs.Eq)
        , (Hs.DeriveStock, Hs.Ord)
        , (Hs.DeriveNewtype, Hs.Enum)
        ]
    , translationDeriveTypedefPrim = [
          (Hs.DeriveStock, Hs.Eq)
        , (Hs.DeriveStock, Hs.Ord)
        , (Hs.DeriveStock, Hs.Read)
        , (Hs.DeriveStock, Hs.Show)
        , (Hs.DeriveNewtype, Hs.Enum)
        , (Hs.DeriveNewtype, Hs.Ix)
        , (Hs.DeriveNewtype, Hs.Bounded)
        , (Hs.DeriveNewtype, Hs.Bits)
        , (Hs.DeriveNewtype, Hs.FiniteBits)
        , (Hs.DeriveNewtype, Hs.Floating)
        , (Hs.DeriveNewtype, Hs.Fractional)
        , (Hs.DeriveNewtype, Hs.Integral)
        , (Hs.DeriveNewtype, Hs.Num)
        , (Hs.DeriveNewtype, Hs.Real)
        , (Hs.DeriveNewtype, Hs.RealFloat)
        , (Hs.DeriveNewtype, Hs.RealFrac)
        ]
    }

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- filepath argument https://github.com/well-typed/hs-bindgen/issues/333
generateDeclarations ::
     CHeaderIncludePath
  -> TranslationOpts
  -> NameMangler
  -> C.Header
  -> [Hs.Decl]
generateDeclarations = toHs

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

class ToHs (a :: Star) where
  type InHs a :: Star
  toHs :: CHeaderIncludePath -> TranslationOpts -> NameMangler -> a -> InHs a

instance ToHs C.Header where
  type InHs C.Header = [Hs.Decl]
  toHs p opts nm (C.Header decs) = concatMap (toHs p opts nm) decs

instance ToHs C.Decl where
  type InHs C.Decl = [Hs.Decl]
  toHs _ opts nm (C.DeclStruct struct)  = reifyStructFields struct $ structDecs opts nm struct
  toHs _ opts nm (C.DeclUnion union)    = unionDecs opts nm union
  toHs _ opts nm (C.DeclOpaqueStruct o) = opaqueStructDecs opts nm $ C.opaqueStructTag o
  toHs _ opts nm (C.DeclEnum e)         = enumDecs opts nm e
  toHs _ opts nm (C.DeclOpaqueEnum o)   = opaqueStructDecs opts nm $ C.opaqueEnumTag o -- TODO?
  toHs _ opts nm (C.DeclTypedef d)      = typedefDecs opts nm d
  toHs _ opts nm (C.DeclMacro m)        = macroDecs opts nm m
  toHs p opts nm (C.DeclFunction f)     = functionDecs p opts nm f

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

reifyStructFields ::
     C.Struct
  -> (forall n. SNatI n => Vec n C.StructField -> a)
  -> a
reifyStructFields struct k = Vec.reifyList (C.structFields struct) k

-- | Generate declarations for given C struct
structDecs :: forall n.
     SNatI n
  => TranslationOpts
  -> NameMangler
  -> C.Struct -> Vec n C.StructField -> [Hs.Decl]
structDecs opts nm@NameMangler{..} struct fields = concat
    [ [ Hs.DeclData hs ]
    , [ Hs.DeclDefineInstance $ Hs.InstanceStorable hs storable]
    , [ Hs.DeclDeriveInstance strat clss (Hs.structName hs)
      | (strat, clss) <- translationDeriveStruct opts
      ]
    , flamInstance
    ]
  where
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
    peek ptr f = case C.fieldWidth f of
      Nothing -> Hs.PeekByteOff ptr (C.fieldOffset f `div` 8)
      Just w  -> Hs.PeekBitOffWidth ptr (C.fieldOffset f) w

    poke :: Idx ctx -> C.StructField -> Idx ctx -> Hs.PokeByteOff ctx
    poke ptr f i = case C.fieldWidth f of
      Nothing -> Hs.PokeByteOff ptr (C.fieldOffset f `div` 8) i
      Just w  -> Hs.PokeBitOffWidth ptr (C.fieldOffset f) w i

    flamInstance :: [Hs.Decl]
    flamInstance = case C.structFlam struct of
      Nothing  -> []
      Just flam -> singleton $ Hs.DeclDefineInstance $ Hs.InstanceHasFLAM
        hs
        (typ nm (C.fieldType flam))
        (C.fieldOffset flam `div` 8)

{-------------------------------------------------------------------------------
  Opaque struct
-------------------------------------------------------------------------------}

opaqueStructDecs :: TranslationOpts -> NameMangler -> C.CName -> [Hs.Decl]
opaqueStructDecs _opts NameMangler{..} cname =
    [ Hs.DeclEmpty hsName
    ]
  where
    typeConstrCtx = TypeConstrContext cname
    hsName = mangleTypeConstrName typeConstrCtx

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

unionDecs :: TranslationOpts -> NameMangler -> C.Union -> [Hs.Decl]
unionDecs _opts NameMangler{..} union =
    -- TODO: only empty data declaration for now
    [ Hs.DeclEmpty hsName
    ]
  where
    typeConstrCtx = StructTypeConstrContext $ C.unionDeclPath union
    hsName = mangleTypeConstrName typeConstrCtx

{-------------------------------------------------------------------------------
  Enum
-------------------------------------------------------------------------------}

enumDecs :: TranslationOpts -> NameMangler -> C.Enu -> [Hs.Decl]
enumDecs opts nm@NameMangler{..} e = concat [
      [ Hs.DeclNewtype Hs.Newtype{..} ]
    , [ Hs.DeclDefineInstance $ Hs.InstanceStorable hs storable ]
    , [ Hs.DeclDeriveInstance strat clss (Hs.structName hs)
      | (strat, clss) <- translationDeriveEnum opts
      ]
    , valueDecls
    ]
  where
    cEnumName     = C.enumTag e
    typeConstrCtx = TypeConstrContext cEnumName
    newtypeName   = mangleTypeConstrName typeConstrCtx
    newtypeConstr = mangleConstrName $ ConstrContext typeConstrCtx
    newtypeField  = Hs.Field {
        fieldName   = mangleVarName $ EnumVarContext typeConstrCtx
      , fieldType   = typ nm (C.enumType e)
      , fieldOrigin = Hs.FieldOriginNone
      }
    newtypeOrigin = Hs.NewtypeOriginEnum e

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

typedefDecs :: TranslationOpts -> NameMangler -> C.Typedef -> [Hs.Decl]
typedefDecs opts nm@NameMangler{..} d = concat [
      [ Hs.DeclNewtype Hs.Newtype{..} ]
    , [ Hs.DeclDeriveInstance Hs.DeriveNewtype Hs.Storable newtypeName ]
    , [ Hs.DeclDeriveInstance strat clss newtypeName
      | C.TypePrim pt <- [C.typedefType d]
      , (strat, clss) <- translationDeriveTypedefPrim opts
      , clss `elem` primTypeInstances pt
      ]
    ]
  where
    cName         = C.typedefName d
    typeConstrCtx = TypeConstrContext cName
    newtypeName   = mangleTypeConstrName typeConstrCtx
    newtypeConstr = mangleConstrName $ ConstrContext typeConstrCtx
    newtypeField  = Hs.Field {
        fieldName   = mangleVarName $ EnumVarContext typeConstrCtx
      , fieldType   = typ nm (C.typedefType d)
      , fieldOrigin = Hs.FieldOriginNone
      }
    newtypeOrigin = Hs.NewtypeOriginTypedef d

primTypeInstances :: C.PrimType -> [Hs.TypeClass]
primTypeInstances (C.PrimFloating _) = [
      Hs.Enum
    , Hs.Floating
    , Hs.RealFloat
    , Hs.Storable
    , Hs.Num
    , Hs.Read
    , Hs.Fractional
    , Hs.Real
    , Hs.RealFrac
    , Hs.Show
    , Hs.Eq
    , Hs.Ord
    ]
primTypeInstances _otherwise = [
      Hs.Bits
    , Hs.FiniteBits
    , Hs.Bounded
    , Hs.Enum
    , Hs.Storable
    , Hs.Ix
    , Hs.Num
    , Hs.Read
    , Hs.Integral
    , Hs.Real
    , Hs.Show
    , Hs.Eq
    , Hs.Ord
    ]

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

macroDecs :: TranslationOpts -> NameMangler -> C.MacroDecl -> [Hs.Decl]
macroDecs opts nm C.MacroDecl { macroDeclMacro = m, macroDeclMacroTy = ty }
    | Macro.Quant bf <- ty
    , Macro.isPrimTy bf
    = macroDecsTypedef opts nm m

    | otherwise
    = macroVarDecs nm m ty
    where

macroDecs _ _ C.MacroReparseError {} = []
macroDecs _ _ C.MacroTcError {}      = []

macroDecsTypedef :: TranslationOpts -> NameMangler -> C.Macro -> [Hs.Decl]
macroDecsTypedef opts nm@NameMangler{..} m =
    case C.macroBody m of
      C.MTerm (C.MType ty) ->
        let newtypeField = mkField ty in
        concat [
            [ Hs.DeclNewtype Hs.Newtype{..} ]
          , [ Hs.DeclDeriveInstance Hs.DeriveNewtype Hs.Storable newtypeName ]
          , [ Hs.DeclDeriveInstance strat clss newtypeName
            | C.TypePrim pt <- [ty]
            , (strat, clss) <- translationDeriveTypedefPrim opts
            , clss `elem` primTypeInstances pt
            ]
          ]
      _otherwise ->
        []
  where
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
    go _ (C.TypeUnion declPath) =
        -- TODO: UnionTypeConstrContext?
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
    go _ (C.TypeExtBinding extId) =
        Hs.HsExtBinding extId

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
    goVoid c       = panicPure $ "unexpected type void in context " ++ show c

    goArrayUnknownSize :: TypeContext -> C.Type -> HsType
    goArrayUnknownSize CFunArg t =
         -- Arrays of unknown size as function args are treated as pointers.
         -- <https://en.cppreference.com/w/c/language/array#Arrays_of_unknown_size>
         Hs.HsPtr $ go CTop t
    goArrayUnknownSize c _ =
        -- TODO <https://github.com/well-typed/hs-bindgen/issues/377>
        -- We need to extend 'TypeContext' with a context for extern
        -- declarations, and then allow for arrays of unknown size.
        panicPure $ "unexpected array of unknown size in context " ++ show c

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

functionDecs ::
     CHeaderIncludePath
  -> TranslationOpts
  -> NameMangler
  -> C.Function
  -> [Hs.Decl]
functionDecs headerIncludePath _opts nm f =
    [ Hs.DeclForeignImport $ Hs.ForeignImportDecl
        { foreignImportName       = mangleVarName nm $ VarContext $ C.functionName f
        , foreignImportType       = typ nm $ C.functionType f
        , foreignImportOrigName   = C.getCName (C.functionName f)
        , foreignImportHeader     = getCHeaderIncludePath headerIncludePath
        , foreignImportDeclOrigin = Hs.ForeignImportDeclOriginFunction f
        }
    ]

{-------------------------------------------------------------------------------
  Macro
-------------------------------------------------------------------------------}

macroVarDecs ::
     NameMangler
  -> C.Macro
  -> Macro.Quant ( Macro.Type Macro.Ty )
  -> [Hs.Decl]
macroVarDecs nm (C.Macro { macroName = cVarNm, macroArgs = args, macroBody = body } ) qty =
  [
    Hs.DeclVar $
      Hs.VarDecl
        { varDeclName = hsVarName
        , varDeclType = quantTyHsTy qty
        , varDeclBody = hsBody
        }
  | hsBody <- toList $ macroLamHsExpr nm cVarNm args body
  ]
  where
    hsVarName = mangleVarName nm $ VarContext cVarNm

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

macroLamHsExpr ::
     NameMangler
  -> C.CName
  -> [C.CName]
  -> C.MExpr
  -> Maybe (Hs.VarDeclRHS EmptyCtx)
macroLamHsExpr nm _macroName macroArgs expr =
    makeNames macroArgs Map.empty
  where
    makeNames :: [C.CName] -> Map C.CName (Idx ctx) -> Maybe (Hs.VarDeclRHS ctx)
    makeNames []     env = macroExprHsExpr nm env expr
    makeNames (n:ns) env = Hs.VarDeclLambda . Hs.Lambda (cnameToHint n) <$> makeNames ns (Map.insert n IZ (fmap IS env))

cnameToHint :: C.CName -> NameHint
cnameToHint (C.CName t) = fromString (T.unpack t)

macroExprHsExpr ::
     NameMangler
  -> Map C.CName (Idx ctx)
  -> C.MExpr
  -> Maybe (Hs.VarDeclRHS ctx)
macroExprHsExpr nm = goExpr where
    goExpr :: Map C.CName (Idx ctx) -> C.MExpr -> Maybe (Hs.VarDeclRHS ctx)
    goExpr env = \case
      C.MTerm tm -> goTerm env tm
      C.MEmpty -> Nothing
      C.MApp fun args ->
        goApp env (Hs.InfixAppHead fun) (toList args)

    goTerm :: Map C.CName (Idx ctx) -> C.MTerm -> Maybe (Hs.VarDeclRHS ctx)
    goTerm env = \case
      C.MInt i -> goInt i
      C.MFloat f -> goFloat f
      C.MChar c -> goChar c
      C.MString s -> goString s
      C.MVar cname args ->
        --  TODO: removed the macro argument used as a function check.
        case Map.lookup cname env of
          Just i  -> return (Hs.VarDeclVar i)
          Nothing ->
            let hsVar = mangleVarName nm $ VarContext cname
            in  goApp env (Hs.VarAppHead hsVar) args

      C.MType {} -> Nothing
      C.MAttr _attr tm' -> goTerm env =<< tm'
      C.MStringize {} -> Nothing
      C.MConcat {} -> Nothing

    goApp :: Map C.CName (Idx ctx) -> Hs.VarDeclRHSAppHead -> [C.MExpr] -> Maybe (Hs.VarDeclRHS ctx)
    goApp env appHead args = do
      args' <- traverse (goExpr env) args
      return $ Hs.VarDeclApp appHead args'

    goInt :: C.IntegerLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goInt (C.IntegerLiteral { integerLiteralType = mbIntTy, integerLiteralValue = i }) =
      Just $ Hs.VarDeclIntegral i (maybe HsPrimCInt (uncurry integralType) mbIntTy)

    goChar :: C.CharLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goChar (C.CharLiteral { charLiteralValue = c }) =
      return $ Hs.VarDeclChar c

    goString :: C.StringLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goString (C.StringLiteral { stringLiteralValue = s }) = do
      let bytes = concatMap (IsList.toList . C.charValue) s
      return $
        Hs.VarDeclString (IsList.fromList bytes)

    goFloat :: C.FloatingLiteral -> Maybe (Hs.VarDeclRHS ctx)
    goFloat flt@(C.FloatingLiteral { floatingLiteralType = mbFty }) =
      case mbFty of
        Nothing -> Just $ Hs.VarDeclDouble (C.floatingLiteralDoubleValue flt)
        Just fty ->
          case fty of
            C.PrimFloat  -> Just $ Hs.VarDeclFloat (C.floatingLiteralFloatValue flt)
            C.PrimDouble -> Just $ Hs.VarDeclDouble (C.floatingLiteralDoubleValue flt)
            C.PrimLongDouble -> Nothing
