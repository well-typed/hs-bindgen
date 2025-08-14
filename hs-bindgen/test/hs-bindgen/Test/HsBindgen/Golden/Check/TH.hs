{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Golden test: TH output
module Test.HsBindgen.Golden.Check.TH (check) where

import Data.Generics qualified as SYB
import Data.Map (Map)
import Data.Map.Strict qualified as Map

import Control.Monad (join)
import Control.Monad.State.Strict (State, get, put, runState)

import Clang.Version

import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Ppr qualified as TH
import Language.Haskell.TH.PprLib qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

import System.FilePath (makeRelative)

import HsBindgen.Backend.Artefact.PP.Render (CommentKind (..),
                                             prettyCommentKind)
import HsBindgen.Backend.Hs.Haddock.Documentation (Comment (..))
import HsBindgen.Guasi
import HsBindgen.Lib
import HsBindgen.Pipeline.TH qualified as PipelineTH

import Text.SimplePrettyPrint

import HsBindgen.Pipeline.TH (getExtensions)
import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden (ActualValue (..))
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources
import Test.Tasty hiding (after)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenAnsiDiff "th" fixture $ \_report ->
      if ghcAtLeast904 then do
        pkgroot <- getTestPackageRoot testResources
        let artefacts = Dependencies :* FinalDecls :* getExtensions :* Nil
        (I deps :* I decls :* I requiredExts :* Nil) <-
          runTestHsBindgen testResources test artefacts

        let thDecls :: Qu [TH.Dec]
            thDecls = PipelineTH.genBindingsFromCHeader deps decls requiredExts

            (QuState{..}, thdecs) = runQu thDecls

            -- Here we might have headers outside of our package, but in our
            -- test setup that SHOULD cause an error, as we use bundled stdlib.
            -- And we will cause those on CI, which runs tests on different
            -- systems.
            output :: String
            output = unlines $ concat [
                [    "-- addDependentFile "
                  ++ convertWindows (makeRelative pkgroot fp)
                | fp <- dependencyFiles
                ]
              , [ "-- " ++ normalizeQuotes l
                | src <- cSources, l <- lines src
                ]
              , [ show $ prettyWithDocumentationMap True documentationMap d
                | d <- unqualNames thdecs
                ]
              ]

        return $ ActualValue output
      else
        return $ ActualSkipped "ghc < 9.4"
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".th.txt")

    -- Clang version 19 uses <> for some reason.
    --
    -- If generating Golden tests from an older clang (e.g. 19) version then
    -- this is needed.
    --
    normalizeQuotes :: String -> String
    normalizeQuotes =
      case clangVersion of
        ClangVersionUnknown _ -> id
        ClangVersion v
          -- Put your clang version here
          | v >= (19, 0 ,0) -> go False
          | otherwise       -> id
      where
        go _ [] = []
        go False ('"':xs) = '<' : go True xs
        go True ('"':xs) = '>' : go False xs
        go inBracket (x:xs) = x : go inBracket xs

{-------------------------------------------------------------------------------
  Internal auxiliary: manipulate output
-------------------------------------------------------------------------------}

-- unqualify names, qualified names are noisy *and* GHC.Base names move
unqualNames :: [TH.Dec] -> [TH.Dec]
unqualNames = SYB.everywhere $ SYB.mkT mangleName
  where
    mangleName :: TH.Name -> TH.Name
    mangleName n | n == ''()             = TH.Name (TH.OccName "Unit") TH.NameS
    mangleName (TH.Name occ TH.NameG {}) = TH.Name occ TH.NameS
    mangleName n                         = n

convertWindows :: FilePath -> FilePath
convertWindows = map f where
  f '\\' = '/'
  f c    = c

{-------------------------------------------------------------------------------
  Internal auxiliary: 'Qu' monad
-------------------------------------------------------------------------------}

-- | Deterministic monad with TH.Quote instance
newtype Qu a = Qu (State QuState a)
  deriving newtype (Functor, Applicative, Monad)

data QuState =
  QuState {
    dependencyFiles  :: [FilePath]
  , uniquenessNumber :: !Integer
  , cSources         :: [String]
  , documentationMap :: Map TH.DocLoc (Maybe Comment)
  }

emptyQuState :: QuState
emptyQuState = QuState [] 0 [] Map.empty

instance TH.Quote Qu where
    newName n = Qu $ do
        q@QuState{ uniquenessNumber = u } <- get
        put $! q { uniquenessNumber = u + 1 }
        return $ TH.Name (TH.OccName n) (TH.NameU u)

instance Guasi Qu where
    addDependentFile fp = Qu $ do
        q@QuState{ dependencyFiles = depfiles } <- get
        put $! q { dependencyFiles = depfiles ++ [fp] }

    addCSource src = Qu $ do
        q@QuState{ cSources = csources } <- get
        put $! q { cSources = csources ++ [src] }

    -- Note: we could mock these better, if we want to test error reporting
    -- Currently (2025-04-15) we only report missing extensions,
    -- so there isn't much to test.
    extsEnabled = return []
    reportError _ = return ()

    withDecDoc s qDec = do
      dec <- qDec
      Qu $ do
        q@QuState{ documentationMap = docMap } <- get
        case getDecDocLoc dec of
          Nothing     -> pure ()
          Just docLoc ->
            put $!
              q { documentationMap =
                    Map.insert docLoc s docMap
                }
        return dec

    putFieldDoc docLoc s = Qu $ do
        q@QuState{ documentationMap = docMap } <- get
        put $!
          q { documentationMap =
                Map.insert docLoc s docMap
            }

runQu :: Qu a -> (QuState, a)
runQu (Qu m) = case runState m emptyQuState of
    (x, q) -> (q, x)

{-------------------------------------------------------------------------------
  Internal auxiliary: Template Haskell functions
-------------------------------------------------------------------------------}

-- | This function pretty prints 'TH.Dec' with their associated documentation.
prettyWithDocumentationMap :: Bool -> Map TH.DocLoc (Maybe Comment) -> TH.Dec -> TH.Doc
prettyWithDocumentationMap isTop docMap dec =
  case dec of
    -- Compatibility with older TH version (removes braces from case expressions)

    TH.ValD p r ds ->
            formatDecDoc docMap dec
      TH.$$ thCompatValD docMap p r ds

    -- Declarations with associated documentation

    TH.DataD cxt name tyvars mkind cons derivs ->
            formatDecDoc docMap dec
      TH.$$ ppTypeDef docMap "data" cxt (Just name) tyvars mkind cons derivs

    TH.NewtypeD cxt name tyvars mkind con derivs ->
            formatDecDoc docMap dec
      TH.$$ ppTypeDef docMap "newtype" cxt (Just name) tyvars mkind [con] derivs

    TH.ClassD cxt name tyvars fundeps decs ->
            formatDecDoc docMap dec
      TH.$$ thCompatClassD docMap name tyvars fundeps cxt decs

    TH.InstanceD overlap cxt typ decs ->
            formatDecDoc docMap dec
      TH.$$ thCompatInstanceD docMap overlap cxt typ decs

    _ ->    formatDecDoc docMap dec
      TH.$$ TH.ppr_dec isTop dec

-- | Helper function to lookup and format documentation
--
formatDecDoc :: Map TH.DocLoc (Maybe Comment) -> TH.Dec -> TH.Doc
formatDecDoc docMap thDec =
  case getDecDocLoc thDec >>= join . (`Map.lookup` docMap) of
    Nothing -> TH.empty
    Just c  -> pure
             $ runCtxDoc defaultContext (prettyCommentKind False (TopLevelComment c))

{-------------------------------------------------------------------------------
  TH Compatibility Functions

  These functions provide compatibility with older Template Haskell versions.
  They are extracted here to keep the main logic clean.
-------------------------------------------------------------------------------}

-- | Compatible version of 'TH.ValD' pretty-printing (removes braces from case
-- expressions)
--
thCompatValD :: Map TH.DocLoc (Maybe Comment) -> TH.Pat -> TH.Body -> [TH.Dec] -> TH.Doc
thCompatValD docMap p r ds =
        TH.ppr p TH.<+> thCompatPprBody docMap True r
  TH.$$ thCompatWhereClause docMap ds

-- | Compatible version of 'TH.ClassD' pretty-printing
--
thCompatClassD :: TH.PprFlag a
               => Map TH.DocLoc (Maybe Comment) -> TH.Name -> [TH.TyVarBndr a]
               -> [TH.FunDep] -> TH.Cxt -> [TH.Dec] -> TH.Doc
thCompatClassD docMap name tyvars fundeps cxt decs =
        TH.text "class" TH.<+> TH.pprCxt cxt
                       TH.<+> TH.ppr name
                       TH.<+> TH.hsep (map TH.ppr tyvars)
                       TH.<+> TH.ppr fundeps
  TH.$$ thCompatWhereClause docMap decs

-- | Compatible version of 'TH.InstanceD' pretty-printing
--
thCompatInstanceD :: Map TH.DocLoc (Maybe Comment) -> Maybe TH.Overlap -> TH.Cxt -> TH.Type -> [TH.Dec] -> TH.Doc
thCompatInstanceD docMap overlap cxt typ decs =
        TH.text "instance" TH.<+> maybe TH.empty TH.ppr_overlap overlap
                          TH.<+> TH.pprCxt cxt
                          TH.<+> TH.ppr typ
  TH.$$ thCompatWhereClause docMap decs

-- | Compatible version of where clause pretty-printing (removes braces from
--   case expressions)
--
thCompatWhereClause :: Map TH.DocLoc (Maybe Comment) -> [TH.Dec] -> TH.Doc
thCompatWhereClause _ [] = TH.empty
thCompatWhereClause docMap ds = TH.nest nestDepth
                              $ TH.text "where"
                         TH.<+> TH.vcat (map (prettyWithDocumentationMap False docMap) ds)

-- | Compatible 'TH.Body' pretty-printing
thCompatPprBody :: Map TH.DocLoc (Maybe Comment) -> Bool -> TH.Body -> TH.Doc
thCompatPprBody docMap eq body =
  case body of
    TH.GuardedB xs -> TH.nest nestDepth
                   $ TH.vcat
                   $ map (thCompatPprGuarded docMap eqDoc) xs
    TH.NormalB  e  -> eqDoc TH.<+> thCompatPprExp docMap e
  where eqDoc | eq        = TH.equals
              | otherwise = TH.arrow

-- | Compatible guarded expression pretty-printing
--
thCompatPprGuarded :: Map TH.DocLoc (Maybe Comment) -> TH.Doc -> (TH.Guard, TH.Exp) -> TH.Doc
thCompatPprGuarded docMap eqDoc (guard, expr) =
  case guard of
    TH.NormalG guardExpr -> TH.bar
                     TH.<+> thCompatPprExp docMap guardExpr
                     TH.<+> eqDoc
                     TH.<+> thCompatPprExp docMap expr
    TH.PatG    stmts     ->
            TH.bar TH.<+> TH.vcat (TH.punctuate TH.comma $ map TH.ppr stmts)
      TH.$$ TH.nest nestDepth (eqDoc TH.<+> thCompatPprExp docMap expr)

-- | No precedence 'TH.Exp' printer (compatible version)
--
-- We preserve #13856 pattern match
--
thCompatPprExp :: Map TH.DocLoc (Maybe Comment) -> TH.Exp -> TH.Doc
thCompatPprExp docMap expr =
  case expr of
    TH.LamE [] e -> thCompatPprExp docMap e -- #13856
    TH.LamE ps e -> TH.char '\\' TH.<> TH.hsep (map (TH.pprPat TH.appPrec) ps)
                                TH.<+> TH.text "->" TH.<+> thCompatPprExp docMap e
    TH.CaseE e ms   ->
            TH.text "case" TH.<+> thCompatPprExp docMap e TH.<+> TH.text "of"
      TH.$$ semiSep (thCompatPprMatch docMap) ms
    TH.LamCaseE ms  ->
            TH.text "\\case"
      TH.$$ semiSep (thCompatPprMatch docMap) ms
    _               -> TH.ppr expr
  where
    semiSep f = TH.sep . TH.punctuate TH.semi . map f

-- | Compatible match pretty-printing
thCompatPprMatch :: Map TH.DocLoc (Maybe Comment) -> TH.Match -> TH.Doc
thCompatPprMatch docMap (TH.Match p rhs ds) =
        TH.pprMatchPat p TH.<+> thCompatPprBody docMap False rhs
  TH.$$ thCompatWhereClause docMap ds

{-------------------------------------------------------------------------------
  Type Definition Pretty-Printing
-------------------------------------------------------------------------------}

-- | Pretty-print type definitions with documentation
--
ppTypeDef :: TH.PprFlag a
          => Map TH.DocLoc (Maybe Comment) -> String -> TH.Cxt -> Maybe TH.Name
          -> [TH.TyVarBndr a] -> Maybe TH.Kind -> [TH.Con]
          -> [TH.DerivClause] -> TH.Doc
ppTypeDef docMap s cxt mbName tyvars mkind cons derivs =
  TH.sep [        TH.text s
           TH.<+> TH.pprCxt cxt
           TH.<+> case mbName of
             Just name -> TH.pprName' TH.Applied name TH.<+> TH.hsep (map TH.ppr tyvars)
             Nothing   -> TH.hsep (map TH.ppr tyvars)
           TH.<+> ksigDoc TH.<+> maybeWhere
         , TH.nest nestDepth (TH.vcat (pref $ map (ppCon docMap) cons))
         , if null derivs
              then TH.empty
              else TH.nest nestDepth
                 $ TH.vcat $ map TH.ppr_deriv_clause derivs
      ]
  where
    pref :: [TH.Doc] -> [TH.Doc]
    pref xs | isGadtDecl = xs
    pref []              = [] -- No constructors; can't happen in H98
    pref (d:ds)          = (TH.char '=' TH.<+> d):map (TH.bar TH.<+>) ds

    maybeWhere :: TH.Doc
    maybeWhere | isGadtDecl = TH.text "where"
               | otherwise  = TH.empty

    isGadtDecl :: Bool
    isGadtDecl = not (null cons) && all isGadtCon cons
        where isGadtCon TH.GadtC{}         = True
              isGadtCon TH.RecGadtC{}      = True
              isGadtCon (TH.ForallC _ _ x) = isGadtCon x
              isGadtCon  _                 = False

    ksigDoc = case mkind of
                Nothing -> TH.empty
                Just k  -> TH.dcolon TH.<+> TH.ppr k

-- | Pretty print 'TH.Con' with documentation
--
ppCon :: Map TH.DocLoc (Maybe Comment) -> TH.Con -> TH.Doc
ppCon docMap con =
  case con of
    TH.RecC name fields ->
            TH.ppr name TH.<+> ppRecordFields docMap fields
      TH.$$ getConNamesDoc docMap [name]

    TH.ForallC _ _ innerCon ->
            TH.ppr con
      TH.$$ getConNamesDoc docMap (get_cons_names innerCon)

    TH.RecGadtC names fields rtype ->
            TH.commaSepApplied names TH.<+> TH.dcolon
                                     TH.<+> ppRecordFields docMap fields
                                     TH.<+> TH.arrow
                                     TH.<+> TH.ppr rtype
      TH.$$ getConNamesDoc docMap names

    _ ->    TH.ppr con
      TH.$$ getConNamesDoc docMap (get_cons_names con)

-- | Pretty-print record fields with documentation
ppRecordFields :: Map TH.DocLoc (Maybe Comment) -> [TH.VarBangType] -> TH.Doc
ppRecordFields docMap fields =
  TH.braces $ TH.sep $ TH.punctuate TH.comma $ map ppField fields
  where
    ppField (fname, bang, ftype) =
      let fieldBase = TH.pprName' TH.Applied fname TH.<+> TH.dcolon TH.<+>
                      TH.pprBangType (bang, ftype)
      in     fieldBase
       TH.$$ getConNamesDoc docMap [fname]

-- | Aggregate all comments for each name and pretty print
getConNamesDoc :: Map TH.DocLoc (Maybe Comment) -> [TH.Name] -> TH.Doc
getConNamesDoc docMap names =
  case foldMap (\n -> join $ Map.lookup (TH.DeclDoc n) docMap) names of
    Nothing -> TH.empty
    Just c  -> pure
             $ runCtxDoc defaultContext (prettyCommentKind False (PartOfDeclarationComment c))

-- | Get constructor names (defined here as it's not in all TH versions)
get_cons_names :: TH.Con -> [TH.Name]
get_cons_names (TH.NormalC n _)     = [n]
get_cons_names (TH.RecC n _)        = [n]
get_cons_names (TH.InfixC _ n _)    = [n]
get_cons_names (TH.ForallC _ _ c)   = get_cons_names c
get_cons_names (TH.GadtC ns _ _)    = ns
get_cons_names (TH.RecGadtC ns _ _) = ns

{-------------------------------------------------------------------------------
  DocLoc extraction
-------------------------------------------------------------------------------}

-- | Get DocLoc for declarations that can have attached documentation.
getDecDocLoc :: TH.Dec -> Maybe TH.DocLoc
getDecDocLoc (TH.FunD n _)                                        = Just $ TH.DeclDoc n
getDecDocLoc (TH.ValD (TH.VarP n) _ _)                            = Just $ TH.DeclDoc n
getDecDocLoc (TH.DataD _ n _ _ _ _)                               = Just $ TH.DeclDoc n
getDecDocLoc (TH.NewtypeD _ n _ _ _ _)                            = Just $ TH.DeclDoc n
#if MIN_VERSION_template_haskell(2,20,0)
getDecDocLoc (TH.TypeDataD n _ _ _)                               = Just $ TH.DeclDoc n
#endif
getDecDocLoc (TH.TySynD n _ _)                                    = Just $ TH.DeclDoc n
getDecDocLoc (TH.ClassD _ n _ _ _)                                = Just $ TH.DeclDoc n
getDecDocLoc (TH.SigD n _)                                        = Just $ TH.DeclDoc n
getDecDocLoc (TH.ForeignD (TH.ImportF _ _ _ n _))                 = Just $ TH.DeclDoc n
getDecDocLoc (TH.ForeignD (TH.ExportF _ _ n _))                   = Just $ TH.DeclDoc n
#if MIN_VERSION_template_haskell(2,22,0)
getDecDocLoc (TH.InfixD _ _ n)                                    = Just $ TH.DeclDoc n
#else
getDecDocLoc (TH.InfixD _ n)                                      = Just $ TH.DeclDoc n
#endif
getDecDocLoc (TH.DataFamilyD n _ _)                               = Just $ TH.DeclDoc n
getDecDocLoc (TH.OpenTypeFamilyD (TH.TypeFamilyHead n _ _ _))     = Just $ TH.DeclDoc n
getDecDocLoc (TH.ClosedTypeFamilyD (TH.TypeFamilyHead n _ _ _) _) = Just $ TH.DeclDoc n
getDecDocLoc (TH.PatSynD n _ _ _)                                 = Just $ TH.DeclDoc n
getDecDocLoc (TH.PatSynSigD n _)                                  = Just $ TH.DeclDoc n
getDecDocLoc (TH.DefaultSigD n _)                                 = Just $ TH.DeclDoc n
getDecDocLoc (TH.InstanceD _ _ t _)                               = Just $ TH.InstDoc t
getDecDocLoc (TH.DataInstD _ _ t _ _ _)                           = Just $ TH.InstDoc t
getDecDocLoc (TH.NewtypeInstD _ _ t _ _ _)                        = Just $ TH.InstDoc t
getDecDocLoc (TH.TySynInstD (TH.TySynEqn _ t _))                  = Just $ TH.InstDoc t
getDecDocLoc _                                                    = Nothing

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Same as Template Haskell's
--
nestDepth :: Int
nestDepth = 4

-- | Skip TH for older ghc
ghcAtLeast904 :: Bool
#if __GLASGOW_HASKELL__ >=904
ghcAtLeast904 = True
#else
ghcAtLeast904 = False
#endif
