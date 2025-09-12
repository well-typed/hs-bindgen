-- | Translate the partial AST to our internal AST
module HsBindgen.Frontend.LanguageC.PartialAST.ToBindgen (
    -- * Declarations
    fromDecl
  , fromNamedDecl
  , fromFunDecl
    -- * Types
  , fromKnownType
  ) where

import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.LanguageC.Monad
import HsBindgen.Frontend.LanguageC.PartialAST
import HsBindgen.Frontend.Naming
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

fromDecl :: PartialDecl p -> FromLanC p (Maybe Name, Type p)
fromDecl PartialDecl{partialName, partialType} = do
    typ <- fromKnownType <$> fromPartialType partialType
    return (partialName, typ)

fromNamedDecl :: PartialDecl p -> FromLanC p (Name, Type p)
fromNamedDecl PartialDecl{partialName, partialType} = do
    name <- partialFromJust partialName
    typ  <- fromKnownType <$> fromPartialType partialType
    return (name, typ)

fromFunDecl ::
     ValidPass p
  => PartialDecl p
  -> FromLanC p (
         Name
       , ( [(Maybe Name, Type p)]
         , Type p
         )
       )
fromFunDecl PartialDecl{partialName, partialType} = do
    name          <- partialFromJust partialName
    (params, res) <- fromTopLevelFun =<< fromPartialType partialType
    return (name, (params, res))

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

fromPartialType :: PartialType p -> FromLanC p (KnownType p)
fromPartialType = \case
     PartialUnknown{} -> unexpected "incomplete type"
     PartialKnown typ -> return typ

fromKnownType :: KnownType p -> Type p
fromKnownType = \case
    KnownType   typ        -> typ
    TopLevelFun params res -> TypeFun (map snd params) res

fromTopLevelFun ::
     ValidPass p
  => KnownType p
  -> FromLanC p (
         [(Maybe Name, Type p)]
       , Type p
       )
fromTopLevelFun = \case
    TopLevelFun params res -> return (params, res)
    other -> unexpected $ show other

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

partialFromJust :: HasCallStack => Maybe a -> FromLanC p a
partialFromJust Nothing  = unexpected "Nothing"
partialFromJust (Just x) = return x
