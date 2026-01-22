{-# LANGUAGE CPP #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE BangPatterns #-}

module C.Operator.TH
  (

  -- * Generating type family declarations
    genTyFam, genUnaryTyFam, genBinaryTyFam

  -- * Generating class instances
  , ClassMethod(..)
  , genUnaryInstances, genBinaryInstances
  , genClassInstances, genInstance
  , AssocTyFam(..), AssocTyFamArgs(..)

  -- * Generating proofs
  , withInstanceProofs

  ) where

-- base
import Data.Foldable
  ( toList
#if !MIN_VERSION_base(4,20,0)
  , foldl'
#endif
  )
import Data.Function
  ( on )
import Data.Kind qualified as Hs
import Data.List
  ( sortOn )
import Data.List.NonEmpty
  ( groupBy )

import qualified Data.List.NonEmpty as NE
import Data.Maybe
  ( fromJust, maybeToList, mapMaybe )
import Data.Proxy
  ( Proxy(..) )
import Data.Type.Equality
  ( type (:~:)(Refl) )
import Data.Void
  ( absurd )
import Foreign
  ( Ptr, plusPtr, minusPtr )
import Foreign.C
import qualified GHC.Exts as Foreign.C
  ( Ptr(Ptr) )
import GHC.Exts
  ( Int(I#), addr2Int# )

-- containers
import Data.Map.Strict qualified as Map

-- fin
import Data.Type.Nat qualified as Fin
import Data.Type.Nat
  ( Nat(..) )

-- some
import Data.GADT.Compare
  ( GEq(geq) )

-- template-haskell
import Language.Haskell.TH qualified as TH

-- vec
import Data.Vec.Lazy
  ( Vec(..) )
import Data.Vec.Lazy qualified as Vec

-- c-expr
import C.Type
import C.Type.Internal.Universe
import C.Operator.Internal
  ( OpImpl(..), Conversion(..) )

--------------------------------------------------------------------------------
-- Type families

-- | Generate a unary closed type family with the equations ranging over
-- all types, with the RHS being given by the given function.
--
-- If the function returns 'Nothing', that particular type family equation
-- is omitted.
genUnaryTyFam
  :: Platform
  -> TH.Name -- ^ type family name
  -> ( Platform -> Type TH.Name -> Maybe ( Type TH.Name, details ) )
      -- ^ function implementing the type family reduction rules
  -> TH.Q [ TH.Dec ]
genUnaryTyFam platform fam f = genTyFam @( S Z ) platform fam g
  where
    g :: Platform -> Vec ( S Z ) ( Type TH.Name ) -> Maybe ( Type TH.Name )
    g p ( a ::: VNil ) = fst <$> f p a

-- | Generate a binary closed type family with the equations ranging over
-- all pairs of types, with the RHS being given by the given function.
--
-- If the function returns 'Nothing', that particular type family equation
-- is omitted.
genBinaryTyFam
  :: Platform
  -> TH.Name -- ^ type family name
  -> ( Platform -> Type TH.Name -> Type TH.Name -> Maybe ( Type TH.Name, details ) )
      -- ^ function implementing the type family reduction rules
  -> TH.Q [ TH.Dec ]
genBinaryTyFam platform fam f = genTyFam @( S ( S Z ) ) platform fam g
  where
    g :: Platform -> Vec ( S ( S Z ) ) ( Type TH.Name ) -> Maybe ( Type TH.Name )
    g p ( a ::: b ::: VNil ) = fst <$> f p a b

-- | Generate a closed type family with the equations ranging over
-- all @n@-tuples of types, with the RHS being given by the given function.
--
-- If the function returns 'Nothing', that particular type family equation
-- is omitted.
genTyFam
  :: forall n
  .  Fin.SNatI n
  => Platform
  -> TH.Name -- ^ type family name
  -> ( Platform -> Vec n ( Type TH.Name ) -> Maybe ( Type TH.Name ) )
      -- ^ function implementing the type family reduction rules
  -> TH.Q [ TH.Dec ]
genTyFam platform famName impl = do
  let hsTy :: TH.Type
      hsTy = TH.ConT ''Hs.Type
      n :: Int
      n = Fin.reflectToNum @n Proxy
      args = fmap TH.mkName $ fromJust $ Vec.fromListPrefix @n [ "t" ++ show i | i <- [(1 :: Int)..]]

      kiSig, famDecl :: TH.Dec
      kiSig =
        TH.KiSigD famName ( foldr ( \ arg acc -> TH.AppT ( TH.AppT TH.ArrowT arg ) acc ) hsTy ( replicate n hsTy ) )
      famDecl =
        TH.ClosedTypeFamilyD
          ( TH.TypeFamilyHead famName
              [ TH.PlainTV
                  a
#if MIN_VERSION_template_haskell(2,21,0)
                  TH.BndrReq
#else
                  ()
#endif
              | a <- toList args
              ]
              TH.NoSig
              Nothing
          )
          ( mkTyFamEqs famName ( impl platform ) )

  return
    [ kiSig, famDecl ]

-- | Generate the equations of a closed type family, with the RHS of each
-- equation being given by the given function.
--
-- If the function returns 'Nothing', that particular type family equation
-- is omitted.
mkTyFamEqs
  :: forall n
  .  Fin.SNatI n
  => TH.Name -- ^ type family name
  -> ( Vec n ( Type TH.Name ) -> Maybe ( Type TH.Name ) )
      -- ^ function implementing the type family reduction rules
  -> [ TH.TySynEqn ]
mkTyFamEqs fam impl =
  [ TH.TySynEqn Nothing ( foldl' TH.AppT ( TH.ConT fam ) ( toList $ fmap mkType args ) ) ( mkType res )
  | ( args :: Vec n ( Type a ) ) <- map mkNames $ enumerateTypeTuples @n
  , res <- maybeToList $ impl args
  ]

--------------------------------------------------------------------------------
-- Class instances

-- | Information needed to generate a class instance (of the form we need
-- for the @c-expr@ library) with Template Haskell.
data ClassInstance
  = ClassInstance
  { className      :: !TH.Name
  , instanceTys    :: ![ TH.Type ]
  , classMethods   :: ![ ClassMethod ]
  }
  deriving stock Show

-- | Information needed to generate the methods in a class instance for the
-- @c-expr@ library, using Template Haskell.
data ClassMethod
  = ClassMethod
  { methodName   :: !TH.Name
  , proveName    :: !String
  , methodNbArgs :: !Int
  , methodFn     :: !TH.Exp
  }
  deriving stock Show

-- | Information needed to generate associated type family instances for
-- the @c-expr@ library, using Template Haskell.
data AssocTyFam
  = AssocTyFam
      { assocTyFamName :: !TH.Name
      , assocTyFamArgs :: !AssocTyFamArgs
      , assocTyFamImplName :: !TH.Name
      }

-- | Information about the arity/arguments of an associated type family.
data AssocTyFamArgs
  -- | The associated type family has the same arguments as the class.
  = SameArgs
  -- | The associated type family has a single argument, which is the same
  -- as the first argument of the class.
  | FirstArgOnly


-- | Generate TH declarations for a collection of instances of a unary class,
-- where the argument ranges over all supported types.
genUnaryInstances
  :: TH.Name
     -- ^ class name
  -> Either TH.Type AssocTyFam
     -- ^ result type (either constant, or given by an associated type family)
  -> ( Type TH.Name -> Maybe ( Type TH.Name, OpImpl ( S Z ) ) )
     -- ^ function computing the result type and implementation strategy
  -> [ ClassMethod ]
     -- ^ class methods
  -> TH.Q ( [ TH.Dec ], [ ( TH.Name, ( TH.Type, TH.Clause ) ) ] )
genUnaryInstances cls fam f = genClassInstances @( S Z ) cls fam ( \ ( a ::: VNil ) -> f a )

-- | Generate TH declarations for a collection of instances of a binary class,
-- where the arguments range over pairs of supported types.
genBinaryInstances
  :: TH.Name
     -- ^ class name
  -> Either TH.Type AssocTyFam
     -- ^ result type (either constant, or given by an associated type family)
  -> ( Type TH.Name -> Type TH.Name -> Maybe ( Type TH.Name, OpImpl ( S ( S Z ) ) ) )
     -- ^ function computing the result type and implementation strategy
  -> [ ClassMethod ]
     -- ^ class methods
  -> TH.Q ( [ TH.Dec ], [ ( TH.Name, ( TH.Type, TH.Clause ) ) ] )
genBinaryInstances cls fam f =
  genClassInstances @( S ( S Z ) ) cls fam ( \ ( a ::: b ::: VNil ) -> f a b )

-- | Generate TH declarations for a collection of instances of a class,
-- where the arguments range over all @n@-tuples of supported types.
genClassInstances
  :: forall n
  .  Fin.SNatI n
  => TH.Name
     -- ^ class name
  -> Either TH.Type AssocTyFam
     -- ^ result type (either constant, or given by an associated type family)
  -> ( Vec n ( Type TH.Name ) -> Maybe ( Type TH.Name, OpImpl n ) )
     -- ^ function computing the result type and implementation strategy
  -> [ ClassMethod ]
     -- ^ class methods
  -> TH.Q ( [ TH.Dec ], [ ( TH.Name, ( TH.Type, TH.Clause ) ) ] )
genClassInstances cls fam resTyFn meths = do
  instDecs0 <-
    sequence
      [ ( argTys , ) <$> genInstance cls ( case fam of { Left {} -> Nothing; Right tf -> Just tf } ) argTys resTy opImpl meths
      | argTys <- map mkNames $ enumerateTypeTuples @n
      , ( resTy, opImpl ) <- maybeToList $ resTyFn argTys
      ]
  let instDecs = discardSubsumed instDecs0
      ( insts, singFuns ) = unzip instDecs
  return
    ( insts, map ( \ ( nm, c ) -> ( nm, ( proveType ( Fin.reflectToNum @n Proxy ) fam, c ) ) ) ( concat singFuns ) )

-- | Generate singletons that prove the availability of instances.
--
-- Example: @singAdd :: SType ty1 -> SType ty2 -> (SType (AddRes ty1 ty2), ty1 -> ty2 -> AddRes ty1 ty2)@.
withInstanceProofs :: [ TH.Q ( [ TH.Dec ], [ ( TH.Name, ( TH.Type, TH.Clause ) ) ] ) ] -> TH.Q [ TH.Dec ]
withInstanceProofs inner = do
  ( decs, singFuns ) <- unzip <$> sequence inner
  return $
    concat decs ++ concatMap funDecl ( groupBy ( (==) `on` fst ) $ sortOn fst $ concat singFuns )
  where
    funDecl :: NE.NonEmpty ( TH.Name, ( TH.Type, TH.Clause ) ) -> [ TH.Dec ]
    funDecl ( ( nm, ( ty, c ) ) NE.:| cs ) =
      [ TH.SigD nm ty
      , TH.FunD nm ( c : map ( snd . snd ) cs )
      ]

-- | Discard instances that are subsumed by more general instances, to avoid
-- overlapping instances.
--
-- Example: @instance Add (Ptr ty1) (Ptr ty2)@ is more general
-- than @instance Add (Ptr ty) (Ptr ty)@; discard the latter.
discardSubsumed :: forall n b. Fin.SNatI n => [ ( Vec n ( Type TH.Name ), b ) ] -> [ b ]
discardSubsumed insts = Map.elems $ Map.filterWithKey keepInst allInsts
  where
    allInsts = Map.fromList insts
    keepInst :: Vec n ( Type TH.Name ) -> b -> Bool
    keepInst k _
      -- NB: for simplicity we only handle the n=2 case,
      -- as we don't have any ternary instances.
      | Just Refl <- Fin.eqNat @n @( S ( S Z ) )
      , Ptr ty1 ::: Ptr ty2 ::: VNil <- k
      , ty1 == ty2
      , Map.member ( Ptr ( TH.mkName "ty_1" ) ::: Ptr ( TH.mkName "ty_2" ) ::: VNil ) allInsts
      = False
      | otherwise
      = True

-- | The type of one of the "prove" functions.
--
-- Example:
--
-- @singAdd :: SType rec ty1 -> SType rec ty2 -> ( SType rec ( AddRes ty1 ty2 ), ty1 -> ty2 -> AddRes ty1 ty2 )@
proveType :: Int -> Either TH.Type AssocTyFam -> TH.Type
proveType nbArgs resFam =
  TH.ForallT
    ( map mkTv ( TH.mkName "rec" : tvs ) )
    [ TH.ConT ''GEq `TH.AppT` TH.VarT ( TH.mkName "rec" ) ] $ go 1
  where
    tvs = [ TH.mkName $ "ty_" ++ show j | j <- [ 1 .. nbArgs ] ]
    mkTv tv = TH.PlainTV tv TH.SpecifiedSpec

    mkFunTy [] res = res
    mkFunTy (a:as) res = TH.ArrowT `TH.AppT` a `TH.AppT` (mkFunTy as res)

    resTy =
      case resFam of
        Left ty -> ty
        Right AssocTyFam
          { assocTyFamName = famNm
          , assocTyFamArgs = famArgs
          } -> case famArgs of
            SameArgs     -> foldl' TH.AppT ( TH.ConT famNm ) ( map TH.VarT tvs )
            FirstArgOnly -> ( TH.ConT famNm ) `TH.AppT` ( TH.VarT $ TH.mkName "ty_1" )

    go i
      | i > nbArgs
      = TH.TupleT 2 `TH.AppT` mkSingTy resTy `TH.AppT` ( mkFunTy ( map TH.VarT tvs ) resTy )
      | otherwise
      = TH.ArrowT `TH.AppT` ( mkSingTy $ TH.VarT ( TH.mkName $ "ty_" ++ show i ) ) `TH.AppT` go ( i + 1 )

-- | Generate one TH declaration for a class instance.
genInstance
  :: forall n
  .  TH.Name
     -- ^ class name
  -> Maybe AssocTyFam
     -- ^ optional associated type family definition
  -> Vec n ( Type TH.Name )
     -- ^ class instance argument types
  -> Type TH.Name
     -- ^ result type
  -> OpImpl n
     -- ^ class instance implementation strategy
  -> [ ClassMethod ]
     -- ^ class methods
  -> TH.Q ( TH.Dec, [ ( TH.Name, TH.Clause ) ] )
genInstance cls fam argTys resTy methImpl meths = do
  let clsTy :: TH.Type
      clsTy = mkTcApp cls argTys
      famDecs :: [ TH.Dec ]
      famDecs =
        [ TH.TySynInstD $
            TH.TySynEqn Nothing ( mkTcApp famName args ) ( mkTcApp famImplName args )
        | AssocTyFam
           { assocTyFamName     = famName
           , assocTyFamImplName = famImplName
           , assocTyFamArgs     = assocArgs
           } <- maybeToList fam
        , let args :: [ Type TH.Name ]
              args =
               case assocArgs of
                  SameArgs -> toList argTys
                  FirstArgOnly ->
                    case argTys of
                      VNil -> []
                      ( a ::: _ ) -> [ a ]

        ]
      methDecs :: [ TH.Dec ]
      proveDecs :: [ ( TH.Name, TH.Clause ) ]
      ( methDecs, proveDecs ) = unzip
        -- NB: use scoped type variables in the function, because Template Haskell
        -- doesn't support generating instances with explicit quantification
        -- (https://gitlab.haskell.org/ghc/ghc/-/issues/21794).
        --
        -- We would want:
        --
        -- instance forall ty1 ty2. Sub (Ptr ty1) (Ptr ty2) where
        --   (-) x y = ... @(SubRes (Ptr ty1) (Ptr ty2))
        --
        -- but we instead generate:
        --
        -- instance Sub (Ptr ty1) (Ptr ty2) where
        --   (-) (x :: Ptr ty1) (y :: Ptr ty2) = ... @(SubRes (Ptr ty1) (Ptr ty2))
        [ ( TH.FunD meth [ TH.Clause ( map ( \ ( arg, ty ) -> TH.SigP ( TH.VarP arg ) ( mkType ty ) ) args ) ( TH.NormalB body ) [ ] ]
          , ( proveNm,
                TH.Clause provePats
                  ( case mbProveGuard of
                      Nothing -> TH.NormalB proveRes
                      Just g -> TH.GuardedB [ ( g, proveRes ) ]
                  )
                  []
            )
          )
        | ClassMethod
            { methodName   = meth
            , proveName    = proveStr
            , methodNbArgs = nbArgs
            , methodFn     = fn
            } <- meths
        , let argNms :: [ TH.Name ]
              argNms = map ( TH.mkName . ( "a" ++ ) . show ) [ 1 .. nbArgs ]
              args :: [ ( TH.Name, Type TH.Name ) ]
              args = zip argNms ( toList argTys )

              proveNm = TH.mkName proveStr
              ( provePats, mbProveGuard ) = mkSingPats ( toList argTys )
              proveRes = TH.TupE [ Just ( mkSingExp resTy ), Just $ TH.VarE meth ]

              mkConversions :: [ Conversion ] -> TH.Exp -> TH.Exp
              mkConversions [] e = e
              mkConversions (c1 : cs) e = mkConversions cs ( mkConversion c1 e )
              mkConversion :: Conversion -> TH.Exp -> TH.Exp
              mkConversion c e = ( `TH.AppE` e ) $ case c of
                FromIntegralTo { fromIntegralTo = to } ->
                  TH.VarE 'Prelude.fromIntegral `TH.AppTypeE` TH.WildCardT `TH.AppTypeE` mkType (fmap absurd to)
                RealToFracTo   { realToFracTo   = to } ->
                  TH.VarE 'Prelude.realToFrac `TH.AppTypeE` TH.WildCardT `TH.AppTypeE` mkType (fmap absurd to)
                PtrToInt ->
                  TH.LamE [ TH.ConP 'Foreign.C.Ptr [] [ TH.VarP ( TH.mkName "ptr" ) ] ]
                      ( TH.AppE ( TH.ConE 'I# ) $
                        TH.AppE ( TH.VarE 'addr2Int# ) ( TH.VarE $ TH.mkName "ptr" ) )
              body =
                case methImpl of
                  ConvertThenOp convs ->
                    foldl' TH.AppE fn $
                      zipWith mkConversions ( toList convs ) ( map TH.VarE argNms )
                  AddIntegralAndPtr ->
                    case argNms of
                      [ i, p ] ->
                        TH.VarE 'plusPtr
                            `TH.AppE`
                          ( TH.VarE p )
                            `TH.AppE`
                          ( TH.VarE 'fromIntegral `TH.AppE` TH.VarE i )
                      _ -> error $ "genInstance AddIntegralAndPtr: expected 2 arguments, but got: " ++ show args
                  AddPtrAndIntegral ->
                    case argNms of
                      [ p, i ] ->
                        TH.VarE 'plusPtr
                            `TH.AppE`
                          ( TH.VarE p )
                            `TH.AppE`
                          ( TH.VarE 'fromIntegral `TH.AppE` TH.VarE i )
                      _ -> error $ "genInstance AddPtrAndIntegral: expected 2 arguments, but got: " ++ show args
                  SubPtrAndPtr ->
                    case argNms of
                      [ p1, p2 ] ->
                        ( TH.VarE 'fromIntegral `TH.AppTypeE` TH.WildCardT `TH.AppTypeE` TH.ConT ''CPtrdiff )
                            `TH.AppE`
                          ( TH.VarE 'minusPtr `TH.AppE` TH.VarE p1 `TH.AppE` TH.VarE p2 )
                      _ -> error $ "genInstance SubPtrAndPtr: expected 2 arguments, but got: " ++ show args
                  SubPtrAndIntegral ->
                    case argNms of
                      [ p, i ] ->
                        TH.VarE 'plusPtr
                            `TH.AppE`
                          ( TH.VarE p )
                            `TH.AppE`
                          ( TH.VarE 'fromIntegral `TH.AppE` ( TH.VarE 'Prelude.negate `TH.AppE` TH.VarE i ) )
                      _ -> error $ "genInstance SubPtrAndIntegral: expected 2 arguments, but got: " ++ show args
        ]
      overlap :: Maybe TH.Overlap
      overlap = Nothing
      ctxt :: [ TH.Type ]
      ctxt = [ ]
  return $
    ( TH.InstanceD overlap ctxt clsTy ( famDecs ++ methDecs )
    , proveDecs
    )

--------------------------------------------------------------------------------
-- Util

mkNames :: Vec n ( Type OpaqueTy ) -> Vec n ( Type TH.Name )
mkNames = fmap $ fmap $ \ ( OpaqueTy i ) -> TH.mkName ( "ty_" ++ show i )

mkTcApp :: Foldable f => TH.Name -> f ( Type TH.Name ) -> TH.Type
mkTcApp tc args =
  foldl' ( \ a t -> TH.AppT a ( mkType t ) ) ( TH.ConT tc ) args

mkType :: Type TH.Name -> TH.Type
mkType = \case
  Void -> TH.ConT ''()
  Ptr ty -> TH.AppT ( TH.ConT ''Ptr ) ( TH.VarT ty )
  Arithmetic a ->
    mkArithmeticType a

mkSingTy :: TH.Type -> TH.Type
mkSingTy ty = ( TH.ConT ''SType ) `TH.AppT` TH.VarT ( TH.mkName "rec" ) `TH.AppT` ty

mkSingPats :: [ Type TH.Name ] -> ( [ TH.Pat ], Maybe TH.Guard )
mkSingPats tys = ( map ( mkSingPat . mkOne ) tickedTys, if null guards then Nothing else Just $ TH.PatG guards )
  where
    tickedTys = mkTicked 0 tys
    mkTicked :: Int -> [ Type TH.Name ] -> [ Either ( TH.Name, Int ) ( Type TH.Name ) ]
    mkTicked _ [] = []
    mkTicked i ( Ptr nm : rest ) = Left ( nm, i ) : mkTicked ( i + 1 )rest
    mkTicked i ( ty : rest ) = Right ty : mkTicked i rest
    mkOne ( Left ( nm, i ) ) = Ptr $ mkTickedName nm i
    mkOne ( Right ty ) = ty
    guards = concat $ mapMaybe mkGroup $ groupBy ( (==) `on` fst ) $ mapMaybe oneGuard tickedTys
    mkGroup :: NE.NonEmpty ( TH.Name, Int ) -> Maybe [ TH.Stmt ]
    mkGroup ( _ NE.:| [] ) = Nothing
    mkGroup ( ( nm, i ) NE.:| ( fmap snd -> js ) ) =
      Just
        [ TH.BindS
           ( TH.ConP 'Just [] [ TH.ConP 'Refl [] [] ] )
           ( TH.VarE 'geq `TH.AppE` TH.VarE ( mkTickedName nm i ) `TH.AppE` TH.VarE ( mkTickedName nm j ) )
        | j <- js ]
    mkTickedName nm i = TH.mkName $ show nm ++ replicate i '\''
    oneGuard ( Left ( nm, i ) ) = Just ( nm, i )
    oneGuard _                  = Nothing

mkSingPat :: Type TH.Name -> TH.Pat
mkSingExp :: Type TH.Name -> TH.Exp
( mkSingPat, mkSingExp ) =
  ( go_type TH.VarP ( \ c -> TH.ConP c [] [] ) ( \ c a -> TH.ConP c [] [a] )
  , go_type TH.VarE TH.ConE ( \ c e -> TH.AppE ( TH.ConE c ) e )
  )
  where
    go_type var con conapp = \case
      Void -> con 'SVoid
      Ptr ty -> conapp 'SPtr ( var ty )
      Arithmetic a -> conapp 'SArithmetic $ go_arith a

        where

        go_arith = \case
          Integral i -> conapp 'SIntegral $ go_integral i
          FloatLike f -> conapp 'SFloatLike $ go_floatlike f
        go_integral = \case
          Bool -> con 'SBool
          CharLike c -> conapp 'SCharLike $ go_charlike c
          IntLike i -> conapp 'SIntLike $ go_intlike i
        go_floatlike = \case
          FloatType -> con 'SFloatType
          DoubleType -> con 'SDoubleType
        go_charlike = \case
          Char -> con 'S_Char
          SChar -> con 'S_SChar
          UChar -> con 'S_UChar
        go_intlike = \case
          Short s ->
            case s of
              Signed -> con 'SShort
              Unsigned -> con 'SUShort
          Int s ->
            case s of
              Signed -> con 'SInt
              Unsigned -> con 'SUInt
          Long s ->
            case s of
              Signed -> con 'SLong
              Unsigned -> con 'SULong
          LongLong s ->
            case s of
              Signed -> con 'SLongLong
              Unsigned -> con 'SULongLong
          PtrDiff -> con 'SPtrDiff

mkArithmeticType :: ArithmeticType -> TH.Type
mkArithmeticType ( Integral i ) = mkIntegralType i
mkArithmeticType ( FloatLike f ) =
  TH.ConT $
    case f of
      FloatType  -> ''CFloat
      DoubleType -> ''CDouble

mkIntegralType :: IntegralType -> TH.Type
mkIntegralType = \case
  Bool -> TH.ConT ''CBool
  CharLike c -> TH.ConT $
    case c of
      Char  -> ''CChar
      SChar -> ''CSChar
      UChar -> ''CUChar
  IntLike i ->
    mkIntLikeType i

mkIntLikeType :: IntLikeType -> TH.Type
mkIntLikeType = TH.ConT . \case
  Short    s ->
    case s of
      Signed   -> ''CShort
      Unsigned -> ''CUShort
  Int      s ->
    case s of
      Signed   -> ''CInt
      Unsigned -> ''CUInt
  Long     s ->
    case s of
      Signed   -> ''CLong
      Unsigned -> ''CULong
  LongLong s ->
    case s of
      Signed   -> ''CLLong
      Unsigned -> ''CULLong
  PtrDiff -> ''CPtrdiff
