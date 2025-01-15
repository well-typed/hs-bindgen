{-# LANGUAGE CPP #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module C.Operator.TH
  (

  -- * Generating type family declarations
    genTyFam, genUnaryTyFam, genBinaryTyFam

  -- * Generating class instances
  , ClassMethod(..)
  , genUnaryInstances, genBinaryInstances
  , genInstances, genInstance
  , AssocTyFam(..), AssocTyFamArgs(..)

  ) where

-- base
import Data.Foldable
  ( toList
#if !MIN_VERSION_base(4,20,0)
  , foldl'
#endif
  )
import Data.Kind qualified as Hs
import Data.Maybe
  ( fromJust, maybeToList )
import Data.Proxy
  ( Proxy(..) )
import Data.Void
  ( Void, absurd )
import Foreign
  ( Ptr, plusPtr, minusPtr )
import Foreign.C
import qualified GHC.Exts as Foreign.C
  ( Ptr(Ptr) )
import GHC.Exts
  ( Int(I#), addr2Int# )

-- fin
import Data.Type.Nat qualified as Fin
import Data.Type.Nat
  ( Nat(..) )

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
  -> Maybe AssocTyFam
     -- ^ optional associated type family definition
  -> ( Type TH.Name -> Maybe ( Type TH.Name, OpImpl ( S Z ) ) )
     -- ^ function computing the result type and implementation strategy
  -> [ ClassMethod ]
     -- ^ class methods
  -> TH.Q [ TH.Dec ]
genUnaryInstances cls fam f = genInstances @( S Z ) cls fam ( \ ( a ::: VNil ) -> f a )

-- | Generate TH declarations for a collection of instances of a binary class,
-- where the arguments range over pairs of supported types.
genBinaryInstances
  :: TH.Name
     -- ^ class name
  -> Maybe AssocTyFam
     -- ^ optional associated type family definition
  -> ( Type TH.Name -> Type TH.Name -> Maybe ( Type TH.Name, OpImpl ( S ( S Z ) ) ) )
     -- ^ function computing the result type and implementation strategy
  -> [ ClassMethod ]
     -- ^ class methods
  -> TH.Q [ TH.Dec ]
genBinaryInstances cls fam f =
  genInstances @( S ( S Z ) ) cls fam ( \ ( a ::: b ::: VNil ) -> f a b )

-- | Generate TH declarations for a collection of instances of a class,
-- where the arguments range over all @n@-tuples of supported types.
genInstances
  :: forall n
  .  Fin.SNatI n
  => TH.Name
     -- ^ class name
  -> Maybe AssocTyFam
     -- ^ optional associated type family definition
  -> ( Vec n ( Type TH.Name ) -> Maybe ( Type TH.Name, OpImpl n ) )
     -- ^ function computing the result type and implementation strategy
  -> [ ClassMethod ]
     -- ^ class methods
  -> TH.Q [ TH.Dec ]
genInstances cls fam resTyFn meths =
  sequence
    [ genInstance cls fam tys opImpl meths
    | tys <- map mkNames $ enumerateTypeTuples @n
    , ( _, opImpl ) <- maybeToList $ resTyFn tys ]

-- | Generate one TH declaration for a class instance.
genInstance
  :: forall n
  .  TH.Name
     -- ^ class name
  -> Maybe AssocTyFam
     -- ^ optional associated type family definition
  -> Vec n ( Type TH.Name )
     -- ^ class instance argument types
  -> OpImpl n
     -- ^ class instance implementation strategy
  -> [ ClassMethod ]
     -- ^ class methods
  -> TH.Q TH.Dec
genInstance cls fam tys methImpl meths = do
  let clsTy :: TH.Type
      clsTy = mkTcApp cls tys
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
                  SameArgs -> toList tys
                  FirstArgOnly ->
                    case tys of
                      VNil -> []
                      ( a ::: _ ) -> [ a ]

        ]
      methDecs :: [ TH.Dec ]
      methDecs =
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
        [ TH.FunD meth [ TH.Clause ( map ( \ ( arg, ty ) -> TH.SigP ( TH.VarP arg ) ( mkType ty ) ) argsTys ) ( TH.NormalB body ) [ ] ]
        | ClassMethod
            { methodName   = meth
            , methodNbArgs = nbArgs
            , methodFn     = fn
            } <- meths
        , let args :: [ TH.Name ]
              args = map ( TH.mkName . ( "a" ++ ) . show ) [ 1 .. nbArgs ]
              argsTys :: [ ( TH.Name, Type TH.Name ) ]
              argsTys = zip args ( toList tys )
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
                      zipWith mkConversions ( toList convs ) ( map TH.VarE args )
                  AddIntegralAndPtr ->
                    case args of
                      [ i, p ] ->
                        TH.VarE 'plusPtr
                            `TH.AppE`
                          ( TH.VarE p )
                            `TH.AppE`
                          ( TH.VarE 'fromIntegral `TH.AppE` TH.VarE i )
                      _ -> error $ "genInstance AddIntegralAndPtr: expected 2 arguments, but got: " ++ show args
                  AddPtrAndIntegral ->
                    case args of
                      [ p, i ] ->
                        TH.VarE 'plusPtr
                            `TH.AppE`
                          ( TH.VarE p )
                            `TH.AppE`
                          ( TH.VarE 'fromIntegral `TH.AppE` TH.VarE i )
                      _ -> error $ "genInstance AddPtrAndIntegral: expected 2 arguments, but got: " ++ show args
                  SubPtrAndPtr ->
                    case args of
                      [ p1, p2 ] ->
                        ( TH.VarE 'fromIntegral `TH.AppTypeE` TH.WildCardT `TH.AppTypeE` TH.ConT ''CPtrdiff )
                            `TH.AppE`
                          ( TH.VarE 'minusPtr `TH.AppE` TH.VarE p1 `TH.AppE` TH.VarE p2 )
                      _ -> error $ "genInstance SubPtrAndPtr: expected 2 arguments, but got: " ++ show args
                  SubPtrAndIntegral ->
                    case args of
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
    TH.InstanceD overlap ctxt clsTy ( famDecs ++ methDecs )

--------------------------------------------------------------------------------
-- Util

mkNames :: Vec n ( Type OpaqueTy ) -> Vec n ( Type TH.Name )
mkNames = fmap $ fmap $ \ ( OpaqueTy i ) -> TH.mkName ( "ty_" ++ show i )

mkTcApp :: Foldable f => TH.Name -> f ( Type TH.Name ) -> TH.Type
mkTcApp tc args =
  foldl' ( \ a t -> TH.AppT a ( mkType t ) ) ( TH.ConT tc ) args

mkType :: Type TH.Name -> TH.Type
mkType = \case
  Void -> TH.ConT ''Void
  Ptr ty -> TH.AppT ( TH.ConT ''Ptr ) ( TH.VarT ty )
  Arithmetic a ->
    mkArithmeticType a

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
