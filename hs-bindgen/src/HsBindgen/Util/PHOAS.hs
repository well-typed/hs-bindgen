-- | Our particular flavour of PHOAS
--
-- Intended for unqualified import.
module HsBindgen.Util.PHOAS (
    -- * Main definitions
    PHOAS
  , Bound
    -- * Lifted types
  , List(..)
    -- * Showing PHOAS types
  , Unique(..)
  , ShowOpen(..)
  , showClosed
    -- * Deriving-via support
  , Degenerate(..)
  ) where

import Data.Fin qualified as Fin
import Data.Kind
import Data.List (intersperse)
import Data.Type.Nat
import Data.Vec.Lazy (Vec(..))
import Data.Vec.Lazy qualified as Vec
import Generics.SOP
import GHC.Generics qualified as GHC
import GHC.Show
import Data.Foldable

{-------------------------------------------------------------------------------
  Main definitions
-------------------------------------------------------------------------------}

type PHOAS = (Type -> Type) -> Type

-- | Bound name
data Bound

{-------------------------------------------------------------------------------
  Lifted types
-------------------------------------------------------------------------------}

type List :: PHOAS -> PHOAS
newtype List a f = List { getList :: [a f] }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo)

deriving anyclass instance ShowOpen (a Unique) => ShowOpen (List a Unique)

{-------------------------------------------------------------------------------
  Showing PHOAS types
-------------------------------------------------------------------------------}

newtype Unique a = Unique Int
  deriving newtype (Enum)

showUnique :: Unique a -> ShowS
showUnique (Unique u) = showString "x" . showsPrec 0 u

class ShowOpen (term :: Type) where
  -- | Show open term
  --
  -- Precondition: all bound variables already present in the term must be
  -- strictly less than the specified next available 'Unique'.
  showOpen :: Unique Bound -> Int -> term -> ShowS

  default showOpen ::
       (HasDatatypeInfo term, All2 ShowOpen (Code term))
    => Unique Bound -> Int -> term -> ShowS
  showOpen u p term =
      gshowOpen u p
        (constructorInfo (datatypeInfo (Proxy @term)))
        (from term)

instance ShowOpen (Unique Bound) where
  showOpen _ _ = showUnique

instance ShowOpen (a Unique) => ShowOpen (Unique Bound -> a Unique) where
  showOpen u p f = showParen (p >= appPrec1) $
        showString "\\"
      . showUnique u
      . showString " -> "
      . showOpen (succ u) 0 (f u)

instance ( ShowOpen (a Unique)
         , SNatI n
         ) => ShowOpen (Vec n (Unique Bound) -> a Unique) where
  showOpen (Unique u) p f = showParen (p >= appPrec1) $
        showString "\\("
      . intercalateS (showString " ::: ") (map showUnique $ toList newUniques)
      . showString " ::: VNil) -> "
      . showOpen (Unique $ u + Vec.length newUniques) p (f newUniques)
    where
      newUniques :: Vec n (Unique Bound)
      newUniques = Vec.tabulate $ \n ->
                     Unique (u + fromIntegral (Fin.toInteger n))

instance ShowOpen a => ShowOpen [a] where
  showOpen u _p xs =
        showString "["
      . intercalateS (showString ", ") xs'
      . showString "]"
    where
      xs' :: [ShowS]
      xs' = map (showOpen u 0) xs

-- | Show closed PHOAS term
showClosed :: forall a. ShowOpen (a Unique) => (forall f. a f) -> String
showClosed term = showOpen (Unique 0) 0 (term :: a Unique) ""

{-------------------------------------------------------------------------------
  Generic 'ShowOpen'

  This is based on "Generis.SOP.Show" from @basic-sop@.
-------------------------------------------------------------------------------}

gshowOpen ::
     All2 ShowOpen xss
  => Unique Bound -> Int -> NP ConstructorInfo xss -> SOP I xss -> ShowS
gshowOpen u p cs (SOP sop) =
    hcollapse $ hcliftA2 (Proxy @(All ShowOpen)) (gshowConstr u p) cs sop

gshowConstr ::
     All ShowOpen xs
  => Unique Bound -> Int -> ConstructorInfo xs -> NP I xs -> K ShowS xs
gshowConstr u p (Constructor n) args = K $ showParen (p >= appPrec1) $
    intercalateS (showString " ") (showString n : args')
  where
    args' :: [ShowS]
    args' = hcollapse $
        hcliftA (Proxy @ShowOpen) (K . showOpen u appPrec1 . unI) args
gshowConstr u p (Record n ns) args = K $ showParen (p >= appPrec1) $
      showString n
    . showString " {"
    . intercalateS (showString ", ") args'
    . showString "}"
  where
    args' :: [ShowS]
    args' = hcollapse $ hcliftA2 (Proxy @ShowOpen) (gshowField u) ns args
gshowConstr _ _ Infix{} _ =
    error "gshowConstr: TODO: Infix"

gshowField :: ShowOpen a => Unique Bound -> FieldInfo a -> I a -> K ShowS a
gshowField u (FieldInfo field) (I x) = K $
      showString field
    . showString " = "
    . showOpen u 0 x

{-------------------------------------------------------------------------------
  Degenerate 'ShowOpen' instances
-------------------------------------------------------------------------------}

newtype Degenerate a = Degenerate a

instance Show a => ShowOpen (Degenerate a) where
  showOpen _ p (Degenerate x) = showsPrec p x

deriving via Degenerate Int instance ShowOpen Int

{-------------------------------------------------------------------------------
  Auxiliary: ShowS
-------------------------------------------------------------------------------}

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep = foldr (.) id . intersperse sep