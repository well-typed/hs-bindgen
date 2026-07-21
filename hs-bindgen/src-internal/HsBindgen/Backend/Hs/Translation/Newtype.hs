module HsBindgen.Backend.Hs.Translation.Newtype (
    newtypeDec
  , hasFFITypeDecs
  , hasFieldCompatDecs
  , hasFieldPtrDecs
  ) where

import Control.Monad.State qualified as State
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.Monad (HsM)
import HsBindgen.Backend.Hs.Translation.Monad qualified as HsM
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.Language.Haskell qualified as Hs

-- | Smart constructor for creating a 'Hs.Newtype'
newtypeDec ::
     HasCallStack
  => Hs.Name Hs.NsTypeConstr
  -> Hs.Name Hs.NsConstr
  -> Hs.Field
  -> Origin.Decl Origin.Newtype
  -> Maybe HsDoc.Comment
  -> Set Inst.TypeClass -- ^ Candidate instances
  -> Set Inst.TypeClass -- ^ Known instances
  -> HsM Hs.Newtype
newtypeDec name constr field orig comment candidateInsts knownInsts = do
    hsNewtype <- aux <$> State.get
    State.modify' $ #instanceMap %~ Map.insert hsNewtype.name hsNewtype.instances
    pure hsNewtype
  where
    aux :: HsM.St -> Hs.Newtype
    aux transState = Hs.Newtype {
            name      = name
          , constr    = constr
          , field     = field
          , origin    = orig
          , instances = insts
          , comment   = comment
          }
      where
        resolvedInsts :: Set Inst.TypeClass
        resolvedInsts =
          Hs.getInstances
            transState.instanceMap
            (Just name)
            candidateInsts
            [field.typ]

        insts :: Set Inst.TypeClass
        insts = knownInsts <> resolvedInsts

hasFFITypeDecs ::
     Hs.Newtype
  -> [Hs.Decl l]
hasFFITypeDecs nt =
    [mk | Inst.HasFFIType `elem` nt.instances]
  where
    mk :: Hs.Decl l
    mk = Hs.DeclDeriveInstance Hs.DeriveInstance{
          strategy = Hs.DeriveNewtype
        , clss     = Inst.HasFFIType
        , name     = nt.name
        , comment  = Nothing
        }

{-------------------------------------------------------------------------------
  HasField
-------------------------------------------------------------------------------}

-- | Class instances for 'GHC.Records.Compat.HasField'
--
-- Given a generated newtype (for a @typedef@, @enum@, or macro type):
--
-- > newtype MyType = MyType { unwrapMyType :: CInt }
--
-- GHC automatically generates 'GHC.Records.HasField' instances for the
-- @unwrapMyType@ field.
--
-- 'hasFieldCompatDecs' will generate roughly the following class instances:
--
-- > instance GHC.Records.Compat.HasField "unwrapMyType" MyType CInt
--
hasFieldCompatDecs :: Hs.Newtype -> [Hs.Decl l]
hasFieldCompatDecs nt =
    [ Hs.DeclDefineInstance $
        Hs.DefineInstance {
              comment      = Nothing
            , instanceDecl = Hs.InstanceHasFieldCompat hasFieldCompatDecl
            }
    | Inst.HasFieldCompat `elem` nt.instances
    ]
  where
    parentType :: Hs.Type
    parentType = Hs.TypRef nt.name (Just nt.field.typ)

    hasFieldCompatDecl :: Hs.HasFieldCompatInstance
    hasFieldCompatDecl = Hs.HasFieldCompatInstance {
          parentType  = parentType
        , fieldName   = nt.field.name
        , fieldType   = nt.field.typ
        , impl = Hs.HasFieldCompatImplRecord {
              otherFields = []
            , constr = nt.constr
            }
        }

-- | Class instances for 'GHC.Records.HasField' for the pointer manipulation API
--
-- Given a generated newtype (for a @typedef@, @enum@, or macro type):
--
-- > newtype MyType = MyType { unwrapMyType :: CInt }
--
-- 'hasFieldPtrDecs' will generate roughly the following class instances:
--
-- > instance HasCField "unwrapMyType" MyType where
-- >   type CFieldType "unwrapMyType" MyType = CInt
-- > instance GHC.Records.HasField "unwrapMyType" (Ptr MyType) (Ptr CInt)
--
hasFieldPtrDecs :: Hs.Newtype -> [Hs.Decl l]
hasFieldPtrDecs nt = concat [
      [ Hs.DeclDefineInstance $
          Hs.DefineInstance {
              comment      = Nothing
            , instanceDecl = Hs.InstanceHasFieldPtr hasFieldPtrDecl
            }
      | Inst.HasFieldPtr `elem` nt.instances
      ]
    , [ Hs.DeclDefineInstance $
          Hs.DefineInstance {
              comment      = Nothing
            , instanceDecl = Hs.InstanceHasCField hasCFieldDecl
            }
      | Inst.HasCField `elem` nt.instances
      ]
    ]
  where
    parentType :: Hs.Type
    parentType = Hs.TypRef nt.name (Just nt.field.typ)

    hasFieldPtrDecl :: Hs.HasFieldPtrInstance
    hasFieldPtrDecl = Hs.HasFieldPtrInstance{
          parentType = parentType
        , fieldName  = nt.field.name
        , fieldType  = nt.field.typ
        , deriveVia  = Hs.ViaHasCField
        }

    hasCFieldDecl :: Hs.HasCFieldInstance
    hasCFieldDecl = Hs.HasCFieldInstance{
          parentType  = parentType
        , fieldName   = nt.field.name
        , cFieldType  = nt.field.typ
        , fieldOffset = 0
        }
