{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

-- base
import Control.Arrow
  ( first )
import Data.List
  ( isPrefixOf )
import Data.String
  ( IsString(fromString) )
import Control.Monad
  ( when )

import Data.Maybe
  ( catMaybes, maybeToList, isNothing )
import Data.Traversable
  ( for )
import System.Exit

-- containers
import Data.Map.Strict
  ( Map )
import Data.Map.Strict qualified as Map

-- directory
import System.Directory
  ( doesFileExist, listDirectory, getCurrentDirectory )

-- filepath
import System.FilePath
  ( (</>), (-<.>) )

-- fin
import Data.Type.Nat

-- text
import Data.Text qualified as Text
  ( pack )

-- vec
import Data.Vec.Lazy
  ( Vec(..) )

-- hs-bindgen
import HsBindgen.Clang.Args qualified as Clang

-- c-expr
import C.Type
import C.Type.Internal.Universe

import C.Operators
  ( Op(..), UnaryOp(..), BinaryOp(..)
  , opResType, pprOpApp, pprOp
  )

-- c-expr:test
import CallClang
  ( CType(..)
--  , queryClangBuildTargetTriple
  , queryClangForResultType, getExpansionTypeMapping
  )

--------------------------------------------------------------------------------

-- | We want to pass pre-defined @musl@ standard library headers to Clang
-- for the testsuite. These are currently in the @hs-bindgen@ package, so we
-- do a bit of faff to be able to find them.
findPackageDirectory :: FilePath -> String -> IO (Maybe FilePath)
findPackageDirectory root pkgname = do
  dirs0 <- listDirectory root
  dirs1 <- listDirectory (root </> "..")
  let attempts = "." : ".." : filter (pkgname `isPrefixOf`) dirs0 ++ map (".." </>) (filter (pkgname `isPrefixOf`) dirs1)
  try attempts
  where
    try :: [FilePath] -> IO (Maybe FilePath)
    try [] =
      return Nothing
    try (dir:dirs) = do
      mbRes <- tryOne dir
      case mbRes of
        Nothing -> try dirs
        Just r  -> return $ Just r
    tryOne :: FilePath -> IO (Maybe FilePath)
    tryOne dir = do
      here <- doesFileExist (root </> dir </> pkgname -<.> ".cabal")
      return $
        if here
        then Just dir
        else Nothing

main :: IO ()
main = do

  cwd <- getCurrentDirectory
  mbHsBindgenDir <- findPackageDirectory cwd "hs-bindgen"
  when (isNothing mbHsBindgenDir) $
    putStrLn $ unlines
      [ "WARNING: unable to find 'hs-bindgen' directory"
      , "The test-suite will use whichever C header files it finds on your system."
      ]


  let platform = buildPlatform
      clangArgs0 =
        Clang.defaultClangArgs
          { Clang.clangCStandard = Just Clang.C23 }
      clangArgs =
        case platformOS buildPlatform of
          Windows -> clangArgs0
              -- TODO query default system include path
              { Clang.clangSystemIncludePathDirs =
                  [ fromString "C:/tools/ghc-9.4.8/mingw/lib/clang/14.0.6/include"
                  ]
              }
          Posix ->
            clangArgs0
              { Clang.clangTarget =
                  Just (Clang.Target_Linux_X86_64, Clang.TargetEnvDefault)
              , Clang.clangSystemIncludePathDirs =
                  [ fromString (hsBindgenDir </> "musl-include/x86_64")
                  | hsBindgenDir <- maybeToList mbHsBindgenDir
                  ]
              }
  --buildTargetTriple <- queryClangBuildTargetTriple


  canonTys <- getExpansionTypeMapping clangArgs [ CType $ Arithmetic $ Integral $ IntLike PtrDiff ]

{-
  -- Quick debugging
  putStrLn $ "Canonical type mapping: " ++ show canonTys
  let intTy  = Arithmetic $ Integral $ IntLike $ Int Signed
      ptrTy1 = Ptr $ Arithmetic $ Integral $ IntLike $ Int Signed
  testRes <- queryClangForResultType ( ptrTy1 ::: intTy ::: VNil ) ( pprOpApp ( BinaryOp MRelEQ ) )
  putStrLn $ "Result of ty_1* == int: " ++ show testRes
-}

  putStrLn "Unary operators"
  unaries <- unaryTests platform clangArgs canonTys
  badUnary <-
    fmap catMaybes <$> for unaries $ \ ( op, tests ) -> do
      putStrLn $ pprOp ( UnaryOp op )
      let ( ok, bad ) = partitionTests tests
      if null bad
      then do
        putStrLn $ "   PASSED (" ++ show (length ok) ++ " tests)"
        return Nothing
      else do
        putStrLn $ unlines $
          ( "   FAILED:" )
          : map ( showFailure . first show ) bad
        return $ Just bad
  putStrLn "Binary operators"
  binaries <- binaryTests platform clangArgs canonTys
  badBinary <-
    fmap catMaybes <$> for binaries $ \ ( op, tests ) -> do
      putStrLn $ pprOp ( BinaryOp op )
      let ( ok, bad ) = partitionTests tests
      if null bad
      then do
        putStrLn $ "   PASSED (" ++ show (length ok) ++ " tests)"
        return Nothing
      else do
        putStrLn $ unlines $
          ( "   FAILED:" )
          : map ( showFailure . first show ) bad
        return $ Just bad
  if null badUnary && null badBinary
  then exitSuccess
  else exitFailure


showFailure :: ( String, ( Maybe CType, Maybe CType ) ) -> String
showFailure ( input, ( mbOurs, mbClang ) ) =
  unlines
    [ "   " ++ input
    , "     - computed type: " ++ showMaybeType mbOurs
    , "     -  Clang's type: " ++ showMaybeType mbClang
    ]
  where
    showMaybeType Nothing     = "<n/a>"
    showMaybeType ( Just ty ) = show ty

data TestResult a
  = TestOK !a
  | TestFailed
    { ours, clang's :: !a }
  deriving stock Show

partitionTests :: [ ( x, TestResult a ) ] -> ( [ ( x, a ) ], [ ( x, ( a, a ) ) ] )
partitionTests = foldMap $ \case
  ( x, TestOK a ) -> ( [ ( x, a ) ], [] )
  ( x, TestFailed b1 b2 ) -> ( [], [ ( x, ( b1, b2 ) ) ] )

eqTypeUpToExpansion :: Map CType CType -> Maybe CType -> Maybe CType -> Bool
eqTypeUpToExpansion canonTys ourTy clangTy = go ourTy
  where
    go mbTy
      | mbTy == clangTy
      = True
      | Just ty <- mbTy
      , Just ty' <- Map.lookup ty canonTys
      = go ( Just ty' )
      | otherwise
      = False

unaryTests :: Platform -> Clang.ClangArgs -> Map CType CType -> IO [ ( UnaryOp, [ ( CType, TestResult ( Maybe CType ) ) ] ) ]
unaryTests platform clangArgs canonTys =
  sequence
    [ ( op, ) <$> sequence
         [ do let ours = fmap CType $ opResType platform ( UnaryOp op ) ( ty ::: VNil )
              clang's <- queryClangForResultType clangArgs ( CType ty ::: VNil ) ( pprOpApp ( UnaryOp op ) )
              return $ ( CType ty , ) $
                if eqTypeUpToExpansion canonTys ours clang's
                then TestOK ours
                else TestFailed { ours, clang's }
         | ( ty ::: VNil ) <- mkCTypes <$> enumerateTypeTuples @( S Z )
         ]
    | op <- [ ( minBound :: UnaryOp ) .. maxBound ] ]


binaryTests :: Platform -> Clang.ClangArgs -> Map CType CType -> IO [ ( BinaryOp, [ ( ( CType, CType ), TestResult ( Maybe CType ) ) ] ) ]
binaryTests platform clangArgs canonTys =
  sequence
    [ ( op, ) <$>
      sequence
        [ do let ours = fmap CType $ opResType platform ( BinaryOp op ) ( ty1 ::: ty2 ::: VNil )
             clang's <- queryClangForResultType clangArgs ( CType ty1 ::: CType ty2 ::: VNil ) ( pprOpApp ( BinaryOp op ) )
             return $ ( ( CType ty1, CType ty2 ), ) $
               if eqTypeUpToExpansion canonTys ours clang's
               then TestOK ours
               else TestFailed { ours, clang's }
        | ( ty1 ::: ty2 ::: VNil ) <- mkCTypes <$> enumerateTypeTuples @( S ( S Z ) )
        ]
    | op <- [ ( minBound :: BinaryOp ) .. maxBound ] ]

mkCTypes :: Vec n ( Type OpaqueTy ) -> Vec n ( Type CType )
mkCTypes = fmap $ fmap $ \ ( OpaqueTy i ) -> TypeDef $ Text.pack ( "ty_" ++ show i )
