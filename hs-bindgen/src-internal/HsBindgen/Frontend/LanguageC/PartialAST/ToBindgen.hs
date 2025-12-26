{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoRecordWildCards #-}

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
fromDecl partialDecl = do
    typ <- fromKnownType <$> fromPartialType partialDecl.typ
    return (partialDecl.name, typ)

fromNamedDecl :: PartialDecl -> FromLanC (CName, C.Type HandleMacros)
fromNamedDecl partialDecl = do
    name <- partialFromJust partialDecl.name
    typ  <- fromKnownType <$> fromPartialType partialDecl.typ
    return (name, typ)

fromFunDecl ::
     PartialDecl
  -> FromLanC (
         CName
       , ( [(Maybe CName, C.Type HandleMacros)]
         , C.Type HandleMacros
         )
       )
fromFunDecl partialDecl = do
    name          <- partialFromJust partialDecl.name
    (params, res) <- fromTopLevelFun =<< fromPartialType partialDecl.typ
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
