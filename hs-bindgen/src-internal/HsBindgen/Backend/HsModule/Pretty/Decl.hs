{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.HsModule.Pretty.Decl (

  ) where

import Data.Text qualified as Text
import DeBruijn (Env (..))
import Text.SimplePrettyPrint (CtxDoc, Pretty (..), ($$), (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.HsModule.Names
import HsBindgen.Backend.HsModule.Pretty.Comment
import HsBindgen.Backend.HsModule.Pretty.Expr ()
import HsBindgen.Backend.HsModule.Pretty.Type
import HsBindgen.Backend.SHs.AST
import HsBindgen.Instances as Inst
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

instance Pretty SDecl where
  pretty = \case
    DTypSyn typSyn ->
         prettyTopLevelComment typSyn.comment
      $$ PP.fsep [
             "type" <+> pretty typSyn.name <+> PP.char '='
           , PP.nest 2 (pretty typSyn.typ)
           ]
    DInst inst ->
      let constraints = map pretty inst.super
          -- @flist@ should either be @PP.hlist@ or @PP.vlist@
          clsContext flist = flist "(" ")" constraints
          clsHead = PP.hsep (pretty (resolveTypeClass inst.clss) : map (prettyPrec 1) inst.args)
          cls flist =
                "instance"
            <+> (if null inst.super
                  then PP.empty
                  else clsContext flist <+> "=>")
            <+> clsHead
            <+> "where"

          instanceHead = PP.ifFits (cls PP.hlist) (cls PP.hlist) (cls PP.vlist)
          typs = flip map inst.types $ \(g, typArgs, typSyn) -> PP.nest 2 $ PP.fsep
            [ "type" <+> prettyUnqualResolvedName (resolveGlobal g) <+> PP.hsep (map (prettyPrec 1) typArgs)
                <+> PP.char '='
            , PP.nest 2 (pretty typSyn)
            ]
          decs = flip map inst.decs $ \(name, expr) -> PP.nest 2 $ PP.fsep
            [ prettyUnqualResolvedName (resolveGlobal name) <+> PP.char '='
            , PP.nest 2 (pretty expr)
            ]

      in  PP.vsep $ (prettyTopLevelComment inst.comment) : instanceHead : typs ++ decs

    DRecord record ->
      let d = PP.hsep ["data", pretty record.typ, PP.char '=', pretty record.con]
      in  prettyTopLevelComment record.comment
       $$ ( PP.hang d 2 $ PP.vcat [
                PP.vlist "{" "}" [
                       PP.hsep [
                           pretty field.name
                         , "::"
                         , pretty field.typ
                         ]
                    $$ prettyFieldComment
                  | field <- record.fields
                  , let prettyFieldComment = maybe PP.empty (pretty . PartOfDeclarationComment) field.comment
                  ]
              , nestedDeriving record.deriv
              ]
          )

    DEmptyData empty ->
      prettyTopLevelComment empty.comment
        $$ PP.hsep ["data", pretty empty.name]

    DNewtype newtyp ->
      let d = PP.hsep ["newtype", pretty newtyp.name, PP.char '=', pretty newtyp.con]
          prettyFieldComment = maybe PP.empty (pretty . PartOfDeclarationComment) newtyp.field.comment
      in  prettyTopLevelComment newtyp.comment
       $$ ( PP.hang d 2 $ PP.vcat [
                PP.vlist "{" "}" [
                      PP.hsep [
                          pretty newtyp.field.name
                        , "::"
                        , pretty newtyp.field.typ
                        ]
                    $$ prettyFieldComment
                  ]
              , nestedDeriving newtyp.deriv
              ]
          )

    DForeignImport foreignImport ->
      -- Variable names here refer to the syntax of foreign declarations at
      -- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1540008.4>
      let callconv, impent :: CtxDoc
          (callconv, impent) =
            case foreignImport.callConv of
              CallConvUserlandCapi _ -> ("ccall",
                  PP.string $ Text.unpack foreignImport.origName.text
                )
              CallConvGhcCapi header -> ("capi", PP.hcat [
                  PP.string header
                , PP.string $ Text.unpack foreignImport.origName.text
                ])
              CallConvGhcCCall style -> ("ccall", PP.hcat [
                  case style of
                    ImportAsValue -> ""
                    ImportAsPtr   -> "&"
                , PP.string $ Text.unpack foreignImport.origName.text
                ])

      in  prettyTopLevelComment foreignImport.comment
       $$ PP.hsep [ "foreign import"
               , callconv
               , safety foreignImport.safety
               , "\"" >< impent >< "\""
               , pretty foreignImport.name
               , "::"
               ]
       $$ PP.nest 5 (prettyBindingType foreignImport.parameters foreignImport.result)

    DBinding Binding{..} ->
      let prettyName    = pretty name
          prettyTyp     = prettyBindingType parameters result
          prettySignature =
            if null parameters; then
              prettyName <+> "::" <+> prettyTyp
            else
              prettyName <+> "::" $$  PP.nest 5 prettyTyp
      in  PP.vcat (map (prettyPragma name) pragmas)
       $$ prettyTopLevelComment comment
       $$ prettySignature
       $$ PP.fsep
            [ prettyName <+> PP.char '='
            , PP.nest 2 $ pretty body
            ]

    DDerivingInstance deriv ->
      prettyTopLevelComment deriv.comment
        $$ "deriving" <+> strategy deriv.strategy
                      <+> "instance"
                      <+> pretty deriv.typ

    DPatternSynonym patSyn ->
      PP.vcat [
          prettyTopLevelComment patSyn.comment
        , "pattern" <+> pretty patSyn.name <+> "::" <+> pretty patSyn.typ
        , "pattern" <+> pretty patSyn.name <+> "=" <+> pretty patSyn.rhs
        ]

{-------------------------------------------------------------------------------
  PatEpxr pretty-printing
-------------------------------------------------------------------------------}

instance Pretty PatExpr where
  prettyPrec = prettyPatExpr

prettyPatExpr :: Int -> PatExpr -> CtxDoc
prettyPatExpr prec = \case
    PELit i -> PP.parensWhen (i < 0) $ PP.show i
    PEApps n ps -> PP.parensWhen (prec > 3) $ pretty n <+> PP.hsep (map (prettyPatExpr 4) ps)

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Nested deriving clauses (as part of a datatype declaration)
nestedDeriving :: [(Hs.Strategy ClosedType, [Inst.TypeClass])] -> CtxDoc
nestedDeriving = PP.vcat . map (uncurry aux)
  where
    aux :: Hs.Strategy ClosedType -> [Inst.TypeClass] -> CtxDoc
    aux strat insts =
      let l = auxOneLine strat insts
      in  PP.ifFits l l $ auxMultiLines strat insts

    auxOneLine :: Hs.Strategy ClosedType -> [Inst.TypeClass] -> CtxDoc
    auxOneLine strat insts = PP.hsep [
        "deriving"
      , strategy strat
      , PP.hlist "(" ")" (map (pretty . resolveTypeClass) insts)
      ]

    auxMultiLines :: Hs.Strategy ClosedType -> [Inst.TypeClass] -> CtxDoc
    auxMultiLines strat insts = PP.hang ("deriving" <+> strategy strat) 2 $
      PP.vlist "(" ")" (map (pretty . resolveTypeClass) insts)

strategy :: Hs.Strategy ClosedType -> CtxDoc
strategy Hs.DeriveNewtype  = "newtype"
strategy Hs.DeriveStock    = "stock"
strategy (Hs.DeriveVia ty) = "via" <+> pretty ty

prettyPragma :: Hs.Name Hs.NsVar -> Pragma -> CtxDoc
prettyPragma n = \case
  NOINLINE -> "{-# NOINLINE" <+> pretty n <+> "#-}"

safety :: Safety -> CtxDoc
safety Safe = "safe"
safety Unsafe = "unsafe"

prettyBindingType :: [Parameter] -> Result -> CtxDoc
prettyBindingType params result =
  case params of
    [] -> prettyResultType result.typ
    _  -> prettyParams params
  where
    prettyParam p =
      case p.typ of
        TFun {} -> prettyType EmptyEnv 1 p.typ
        _       -> prettyType EmptyEnv 0 p.typ
      $$ maybe PP.empty (pretty . PartOfDeclarationComment) p.comment


    prettyResultType t = prettyType EmptyEnv 0 t

    prettyParams []     = prettyResultType result.typ
    prettyParams (p:ps) =
         prettyParam p
      $$ PP.nest (-3) ("->" <+> prettyParams ps)

prettyTopLevelComment :: Maybe HsDoc.Comment -> CtxDoc
prettyTopLevelComment = maybe PP.empty (pretty . TopLevelComment)


-- | Pretty-print a 'ResolvedName' unqualified
--
-- This is needed in instance declarations.
prettyUnqualResolvedName :: ResolvedName -> CtxDoc
prettyUnqualResolvedName resolved =
    PP.parensWhen (resolved.typ == OperatorName) $ PP.string resolved.string

resolveTypeClass :: Inst.TypeClass -> ResolvedName
resolveTypeClass = resolveGlobal . typeClassGlobal
