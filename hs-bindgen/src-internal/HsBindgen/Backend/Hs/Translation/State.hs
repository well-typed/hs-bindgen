module HsBindgen.Backend.Hs.Translation.State (
    HsM
  , runHsM
  , TranslationState(..)
  , emptyTranslationState
    -- * Actions with optional delays
    -- $actions
  , Action
  , runAction
  , immediate
  , immediateM
  , delayM
  ) where

import Control.Monad.State.Lazy
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.Translation.Instances
import HsBindgen.Imports

newtype HsM a = HsM (State TranslationState a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadState TranslationState)

runHsM :: HsM a -> a
runHsM (HsM m) = evalState m emptyTranslationState

data TranslationState = TranslationState {
      instanceMap :: InstanceMap
    }
  deriving stock (Generic)

emptyTranslationState :: TranslationState
emptyTranslationState = TranslationState Map.empty

{-------------------------------------------------------------------------------
  Actions with optional delays
-------------------------------------------------------------------------------}

{- $actions

Normally in the @Backend.Hs@ translation step, an action in the 'HsM' monad can
be immediately executed with possible side effects as a result. There is one
exception where we want to delay actions as long as possible: when we generate
auxiliary newtypes for function pointer types.

Normally for @typedef@s we will generate a single @newtype@ of the same name.
For @typedef@s of function pointers, we generate an additional auxiliary
@newtype@ for utility purposes. For example, if we have this C code:

> typedef int T;
> typedef void (*F)(T);

Then we will generate *one* typedef for @T@, and *two* @newtype@s for @F@:

> newtype T = T CInt
> newtype F_Aux = F_Aux (T -> IO ())
> newtype F = F (FunPtr F_Aux)

For each of these newtypes we may define\/derive type class instances, and these
type classes might have dependencies. In the example above, we have one
dependency:

* @F_Aux@ depends on @T@ by value

It is important that we process @T@ before @F_Aux@ so that we have resolved (and
cached) type class instances for @T@ before we try to resolve type instances
class instances for @F_Aux@. It is not important when @F@ is processed because
it has no by-value dependencies on @F_Aux@ or @T@.

The tricky bit here is that @F_Aux@ is not represented in the C AST; we only
generate the @newtype@ in the backend. As such, @F_Aux@ is not taken into
account when deciding the order in which C declarations should be processed. We
process @F_Aux@ when @F@ is processed, and @F@ might be processed before @T@.
When @F@ is processed before @T@, we can see panics.

The solution is this: we make a distinction between actions we want to execute
immediately and ones we want to delay until the end of the @Backend.Hs@
translation step. The 'Action' abstraction is used for this distinction.

Very crudely, we could process @F@ and @F_Aux@ before @T@ as long as we delay
side effects of processing @F_Aux@ until after @T@ is processed. In pseudo-code:

> example = do
>     x <- immediateM processF     {- compute side effects -}
>     y <- delayM     processF_Aux {- delay side effects   -}
>     z <- immediateM processT     {- compute side effects -}
>     mapM runAction [
>         x {- no additional side effects -}
>       , y {- compute delayed side effects -}
>       , z {- no additional side effects -}
>       ]

To achieve the same goal, we could have used a different abstraction than
'Action'. For example, we could have stored a list of 'HsM' actions in
'TranslationState' to be computed at the very end of the @Backend.Hs@
translation step. However, this would cause @_Aux@ newtypes to be rendered at
the very end of Haskell bindings. 'Action' does not have this downside.

NOTE: Right now the only use case for the 'Action' abstraction is auxiliary
@newtype@s for function pointers. If there are other use cases in the future,
then its use might become convoluted and hard to maintain. At that point, it
might be worth looking into replacing the 'Action' abstraction with a proper
sorting phase that takes dependencies between Haskell declarations (like
auxiliary @newtype@s) into account.
-}

-- | An action that is run in the 'HsM' monad.
--
-- There are two types of actions:
--
-- * immediate ('immediateM'): side-effects are performed immediately, and the
--   result value can be accessed using 'runAction'.
--
-- * delayed ('delayM'): side-effects are postponed until the result value is
--   accessed using 'runAction'.
--
-- Actions are composable using 'Functor', 'Applicative', and 'Monad'.
newtype Action a = Action (HsM a)
  deriving newtype (Functor, Applicative, Monad)

-- | Access the result value of an action.
--
-- This performs delayed side-effects, if there are any.
runAction :: Action a -> HsM a
runAction (Action k) = k

-- | Create an immediate action that has no side-effects.
--
-- The result value can be accessed using 'runAction'.
--
immediate :: a -> Action a
immediate x = pure x

-- | Create an immediate action
--
-- Side-effects are performed immediately, and the result value can be accessed
-- using 'runAction'.
--
immediateM :: HsM a -> HsM (Action a)
immediateM action = do
    x <- action
    pure $ pure x

-- | Create a delayed action
--
-- Side-effects are postponed until the result value is accessed using
-- 'runAction'.
--
delayM :: HsM a -> HsM (Action a)
delayM action = pure $ Action action
