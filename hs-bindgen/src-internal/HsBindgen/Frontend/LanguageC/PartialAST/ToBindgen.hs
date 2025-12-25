-- | Translate the partial AST to our internal AST
module HsBindgen.Frontend.LanguageC.PartialAST.ToBindgen (
    -- * Declarations
    fromDecl
  , fromNamedDecl
  , fromFunDecl
    -- * Types
  , fromKnownType
  ) where

import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LanguageC.Monad
import HsBindgen.Frontend.LanguageC.PartialAST
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

fromDecl :: PartialDecl -> FromLanC (Maybe CName, C.Type HandleMacros)
fromDecl PartialDecl{partialName, partialType} = do
    typ <- fromKnownType <$> fromPartialType partialType
    return (partialName, typ)

fromNamedDecl :: PartialDecl -> FromLanC (CName, C.Type HandleMacros)
fromNamedDecl PartialDecl{partialName, partialType} = do
    name <- partialFromJust partialName
    typ  <- fromKnownType <$> fromPartialType partialType
    return (name, typ)

fromFunDecl ::
     PartialDecl
  -> FromLanC (
         CName
       , ( [(Maybe CName, C.Type HandleMacros)]
         , C.Type HandleMacros
         )
       )
fromFunDecl PartialDecl{partialName, partialType} = do
    name          <- partialFromJust partialName
    (params, res) <- fromTopLevelFun =<< fromPartialType partialType
    return (name, (params, res))

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

fromPartialType :: PartialType -> FromLanC KnownType
fromPartialType = \case
     PartialUnknown{} -> unexpected "incomplete type"
     PartialKnown typ -> return typ

fromKnownType :: KnownType -> C.Type HandleMacros
fromKnownType = \case
    KnownType   typ        -> typ
    TopLevelFun params res -> C.TypeFun (map snd params) res

fromTopLevelFun ::
     KnownType
  -> FromLanC (
         [(Maybe CName, C.Type HandleMacros)]
       , C.Type HandleMacros
       )
fromTopLevelFun = \case
    TopLevelFun params res -> return (params, res)
    other -> unexpected $ show other

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

partialFromJust :: HasCallStack => Maybe a -> FromLanC a
partialFromJust Nothing  = unexpected "Nothing"
partialFromJust (Just x) = return x
