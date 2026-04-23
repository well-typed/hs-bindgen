-- | Translate the partial AST to our internal AST
module HsBindgen.Frontend.LanguageC.PartialAST.ToBindgen (
    -- * Declarations
    fromDecl
  , fromNamedDecl
  , fromFunDecl
    -- * Types
  , fromPartialType
  , fromUnknownType
  , fromKnownType
  ) where

import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LanguageC.Monad
import HsBindgen.Frontend.LanguageC.PartialAST
import HsBindgen.Frontend.Pass (NoAnn (..))
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

fromDecl :: PartialDecl -> FromLanC (Maybe CName, C.Type ReparseMacroExpansions)
fromDecl partialDecl = do
    typ <- fromKnownType <$> fromPartialType partialDecl.typ
    return (partialDecl.name, typ)

fromNamedDecl :: PartialDecl -> FromLanC (CName, C.Type ReparseMacroExpansions)
fromNamedDecl partialDecl = do
    name <- partialFromJust partialDecl.name
    typ  <- fromKnownType <$> fromPartialType partialDecl.typ
    return (name, typ)

fromFunDecl ::
     PartialDecl
  -> FromLanC (
         CName
       , ( [(Maybe CName, C.Type ReparseMacroExpansions)]
         , C.Type ReparseMacroExpansions
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
     PartialUnknown typ -> fromUnknownType typ
     PartialKnown typ -> return typ

fromUnknownType :: UnknownType -> FromLanC KnownType
fromUnknownType uty =
      fmap (
          KnownType
        . (if uty.isConst then C.TypeQual C.QualConst else id)
        . (if uty.isComplex then C.TypeComplex else C.TypePrim)
        )
    $ case uty of
        CChar{}  -> pure $ C.PrimChar (C.PrimSignImplicit Nothing)
        CSChar{} -> pure $ C.PrimChar (C.PrimSignExplicit C.Signed)
        CUChar{} -> pure $ C.PrimChar (C.PrimSignExplicit C.Unsigned)
        CShort{} -> pure $ C.PrimIntegral C.PrimShort C.Signed
        CSShort{} -> pure $ C.PrimIntegral C.PrimShort C.Signed
        CUShort{} -> pure $ C.PrimIntegral C.PrimShort C.Unsigned
        CInt{} -> pure $ C.PrimIntegral C.PrimInt C.Signed
        CSInt{} -> pure $ C.PrimIntegral C.PrimInt C.Signed
        CUInt{} -> pure $ C.PrimIntegral C.PrimInt C.Unsigned
        CLong{} -> pure $ C.PrimIntegral C.PrimLong C.Signed
        CSLong{} -> pure $ C.PrimIntegral C.PrimLong C.Signed
        CULong{} -> pure $ C.PrimIntegral C.PrimLong C.Unsigned
        CLLong{} -> pure $ C.PrimIntegral C.PrimLongLong C.Signed
        CSLLong{} -> pure $ C.PrimIntegral C.PrimLongLong C.Signed
        CULLong{} -> pure $ C.PrimIntegral C.PrimLongLong C.Unsigned
        CFloat{} -> pure $ C.PrimFloating C.PrimFloat
        CDouble{} -> pure $ C.PrimFloating C.PrimDouble
        CLDouble{} -> unsupported "long double"
        CTypeUnknown{} -> unexpected $ "incomplete or invalid type: " <> (show uty)

fromKnownType :: KnownType -> C.Type ReparseMacroExpansions
fromKnownType = \case
    KnownType   typ        -> typ
    TopLevelFun params res -> C.TypeFun (map (mkTypeFunArg . snd) params) res
  where
    mkTypeFunArg typ = C.TypeFunArgF {
          typ = typ
        , ann = NoAnn
        }

fromTopLevelFun ::
     KnownType
  -> FromLanC (
         [(Maybe CName, C.Type ReparseMacroExpansions)]
       , C.Type ReparseMacroExpansions
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
