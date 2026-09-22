-- | Map a function 'SHs.SExpr' from one type to another by converting its
-- arguments and result
--
-- Intended for unqualified import.
--
-- > import HsBindgen.Backend.SHs.Translation.Convert
module HsBindgen.Backend.SHs.Translation.MapFunction (
    MapFunctionParams (..)
  , ConvArg (..)
  , ConvRes (..)
  , mapFunctionExpr
  ) where

import DeBruijn (EmptyCtx, Env (..), Idx, Size (..), Wk (..), weakenIdx)

import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Translation.Common qualified as SHs
import HsBindgen.Imports
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.NameHint (NameHint (NameHint))

-- | Parameters for 'mapFunctionExpr'
data MapFunctionParams ctx = MapFunctionParams {
    -- | The conversion expression to apply to function arguments
    convArg  :: ConvArg
    -- | The conversion expression to apply to the function result
  , convRes  :: ConvRes
    -- | The type each function arguments
  , args     :: [Hs.Type]
    -- | The type of the function result
  , res      :: Hs.Type
    -- | The function expression that we apply the mapping to
  , funExpr  :: SHs.SExpr ctx
    -- | The size of the context of 'funExpr'
  , size     :: Size ctx
  }

-- | The conversion expression to apply to function arguments
newtype ConvArg = ConvArg (forall ctx. Hs.Type -> Idx ctx -> SHs.SExpr ctx)

applyConvArg :: ConvArg -> Hs.Type -> Idx ctx -> SHs.SExpr ctx
applyConvArg (ConvArg k) = k

-- | The conversion expression to apply to the function result
newtype ConvRes = ConvRes (forall ctx. Hs.Type -> SHs.SExpr ctx -> SHs.SExpr ctx)

applyConvRes :: ConvRes -> Hs.Type -> SHs.SExpr ctx -> SHs.SExpr ctx
applyConvRes (ConvRes k) = k


-- local type synonyms
type ArgType = Hs.Type
type ResType = Hs.Type
type FunExpr ctx = SHs.SExpr ctx

-- | Map a function 'SHs.SExpr' from one type to another by converting its
-- arguments and result
--
-- For example, let's say:
--
-- * @fun@ is the name of the function we are converting
-- * @convArg@ is the expression we apply to arguments
-- * @convRes@ is the expression we apply to the result
--
-- Then the result of @mapFunctionExpr@ looks roughly like:
--
-- >  \x0 -> \x1 -> \x2 -> \x3 ->
-- >    convRes (fun (convArg x0) (convArg x1) (convArg x2) (convArg x3))
--
-- NOTE: Currently 'mapFunctionExpr' is only used by
-- 'HsBindgen.Backend.Hs.Translation.ForeignImport.mapToFFI' and
-- 'HsBindgen.Backend.Hs.Translation.ForeignImport.mapFromFFI'
--
mapFunctionExpr ::  MapFunctionParams ctx -> SHs.SExpr ctx
mapFunctionExpr params =
    -- construct lambdas for all function arguments
    lambdas params.funExpr (mkWk params.size) EmptyEnv params.args $ \funExpr' wk env ->
      -- convert the function result
      convertRes params.res $
        -- call the function
        callFunction funExpr' $
          -- convert the function arguments
          convertArgs wk env
  where
    -- | We use a weakening ('Wk') to track that the context of local function
    -- arguments is smaller than than the complete context of 'funExpr'.
    mkWk :: forall ctx1. Size ctx1 -> Wk EmptyCtx ctx1
    mkWk sz = case sz of
      SZ -> IdWk
      SS x -> SkipWk $ mkWk x

    -- | Construct a sequence of lambdas:
    --
    -- > \x0 -> \x1 -> \x2 -> \x3 -> ...
    --
    lambdas ::
         forall ctx1 ctx2.
         -- | The function expression has to be shifted under lambdas
         FunExpr ctx1
      -> Wk ctx2 ctx1
      -> Env ctx2 ArgType
      -> [ArgType]
      -> (forall ctx1' ctx2'. FunExpr ctx1' -> Wk ctx2' ctx1' -> Env ctx2' ArgType -> SHs.SExpr ctx1')
      -> SHs.SExpr ctx1
    lambdas fun0 wk0 env0 xs0 kont = go fun0 wk0 env0 xs0
      where
        -- | Run down the context of function arguments, and include a lambda
        -- for each.
        go ::
             forall ctx1' ctx2'.
             FunExpr ctx1'
          -> Wk ctx2' ctx1'
          -> Env ctx2'  ArgType
          -> [ArgType]
          -> SHs.SExpr ctx1'
        go fun wk env []     = kont fun wk env
        go fun wk env (x:xs) = SHs.ELam (NameHint "x") $ go (SHs.shiftExpr fun) (KeepWk wk) (env :> x) xs

    -- | Convert the function result
    --
    -- > ... convRes (...)
    --
    convertRes ::
         forall ctx1.
         ResType
      -> SHs.SExpr ctx1
      -> SHs.SExpr ctx1
    convertRes res e = applyConvRes params.convRes res e

    -- | Construct a call to the function
    --
    -- > ... fun ...
    --
    callFunction ::
         forall ctx1.
         FunExpr ctx1
      -> [SHs.SExpr ctx1]
      -> SHs.SExpr ctx1
    callFunction funExpr args = funExpr `SHs.eAppMany` args

    -- | Convert function arguments
    --
    -- > ... (convArg x0) (convArg x1) (convArg x2) (convArg x3)
    --
    convertArgs ::
         forall ctx1 ctx2.
         Wk ctx2 ctx1
      -> Env ctx2 ArgType
      -> [SHs.SExpr ctx1]
    convertArgs wk args = map (uncurry $ flip $ applyConvArg params.convArg) args'
      where
        args' = fmap (first (weakenIdx wk)) $ toList $ SHs.idxsPairEnv args
