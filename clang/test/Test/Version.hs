{-# LANGUAGE OverloadedStrings #-}
module Test.Version (tests) where

import Control.Monad
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Clang.Version

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Version" [
      testCaseInfo "current"  testCurrent
    , testCase     "examples" testExamples
    ]

{-------------------------------------------------------------------------------
  Regression/sanity: test that we can parse specific version strings
-------------------------------------------------------------------------------}

testExamples :: Assertion
testExamples =
    forM_ examples $ \(str, expected) ->
      assertEqual (show str) expected $ parseClangVersion str
  where
    examples :: [(Text, ClangVersion)]
    examples = [
          ( "Ubuntu clang version 14.0.0-1ubuntu1.1"
          , ClangVersion (14, 0, 0)
          )
        , ( "Ubuntu clang version 18.1.3 (1ubuntu1)"
          , ClangVersion (18, 1, 3)
          )
        , ( "Ubuntu clang version 19.1.1 (1ubuntu1~24.04.2)"
          , ClangVersion (19, 1, 1)
          )
        , ( "clang version 14.0.6"
          , ClangVersion (14, 0, 6)
          )
        , ( "clang version 14.0.6 (git@github.com:llvm/llvm-project.git f28c006a5895fc0e329fe15fead81e37457cb1d1)"
          , ClangVersion (14, 0, 6)
          )
        , ( "clang version 18.1.8 (https://github.com/llvm/llvm-project.git ad36915a8c42d51218eee4b53f2c0aae80eb17e9)"
          , ClangVersion (18, 1, 8)
          )
        , ( "clang version 20.1.4 (https://github.com/llvm/llvm-project ec28b8f9cc7f2ac187d8a617a6d08d5e56f9120e)"
          , ClangVersion (20, 1, 4)
          )
        ]


{-------------------------------------------------------------------------------
  Check that we can parse the version string for whatever version we're running.
-------------------------------------------------------------------------------}

testCurrent :: IO String
testCurrent = do
    raw <- clang_getClangVersion
    case clangVersion of
      ClangVersionUnknown version ->
        assertFailure $ "Unknown version: " ++ show version
      ClangVersion version ->
        if plausible version
          then return $ show raw ++ " parsed as " ++ show version
          else assertFailure $ "Unexpected clang version: " ++ show version

-- | Check whether the parsed @clang@ version is plausible
--
-- As an additional sanity check on the parser (that we don't parse the wrong
-- part of the version string as the clang version) we verify that the version
-- number is plausible.
plausible :: (Int, Int, Int) -> Bool
plausible version@(major, _minor, _patch) = or [
      version `elem` historicReleases

      -- For the current version we don't know which minor/patch to expect
    , major == 20
    ]

-- | Historic @llvm@ releases
--
-- See <https://releases.llvm.org/>, or
-- https://github.com/llvm/llvm-project/releases/.
historicReleases :: [(Int, Int, Int)]
historicReleases = [
      ( 19, 1, 7) -- 14 Jan 2025
    , ( 19, 1, 1) -- 01 Oct 2024
    , ( 19, 1, 0) -- 17 Sep 2024
    , ( 18, 1, 8) -- 20 Jun 2024
    , ( 18, 1, 7) -- 06 Jun 2024
    , ( 18, 1, 6) -- 18 May 2024
    , ( 18, 1, 5) -- 02 May 2024
    , ( 18, 1, 4) -- 17 Apr 2024
    , ( 18, 1, 3) -- 04 Apr 2024
    , ( 18, 1, 2) -- 19 Mar 2024
    , ( 18, 1, 1) -- 08 Mar 2024
    , ( 18, 1, 0) -- 05 Mar 2024
    , ( 17, 0, 6) -- 28 Nov 2023
    , ( 17, 0, 5) -- 14 Nov 2023
    , ( 17, 0, 4) -- 31 Oct 2023
    , ( 17, 0, 3) -- 17 Oct 2023
    , ( 17, 0, 2) -- 03 Oct 2023
    , ( 17, 0, 1) -- 09 Sep 2023
    , ( 16, 0, 6) -- 13 Jun 2023
    , ( 16, 0, 5) -- 02 Jun 2023
    , ( 16, 0, 4) -- 16 May 2023
    , ( 16, 0, 3) -- 03 May 2023
    , ( 16, 0, 2) -- 19 Apr 2023
    , ( 16, 0, 1) -- 05 Apr 2023
    , ( 16, 0, 0) -- 17 Mar 2023
    , ( 15, 0, 7) -- 12 Jan 2023
    , ( 15, 0, 6) -- 29 Nov 2022
    , ( 15, 0, 5) -- 16 Nov 2022
    , ( 15, 0, 4) -- 02 Nov 2022
    , ( 15, 0, 3) -- 18 Oct 2022
    , ( 15, 0, 2) -- 04 Oct 2022
    , ( 15, 0, 1) -- 20 Sep 2022
    , ( 15, 0, 0) -- 06 Sep 2022
    , ( 14, 0, 6) -- 24 Jun 2022
    , ( 14, 0, 5) -- 10 Jun 2022
    , ( 14, 0, 4) -- 24 May 2022
    , ( 14, 0, 3) -- 29 Apr 2022
    , ( 14, 0, 2) -- 26 Apr 2022
    , ( 14, 0, 1) -- 12 Apr 2022
    , ( 14, 0, 0) -- 25 Mar 2022
    , ( 13, 0, 1) -- 07 Feb 2022
    , ( 13, 0, 0) -- 04 Oct 2021
    , ( 12, 0, 1) -- 08 Jul 2021
    , ( 12, 0, 0) -- 14 Apr 2021
    , ( 11, 1, 0) -- 25 Feb 2021
    , ( 11, 0, 1) -- 14 Jan 2021
    , ( 11, 0, 0) -- 12 Oct 2020
    , ( 10, 0, 1) -- 06 Aug 2020
    , ( 10, 0, 0) -- 24 Mar 2020
    , (  9, 0, 1) -- 20 Dec 2019
    , (  9, 0, 0) -- 19 Sep 2019
    , (  8, 0, 1) -- 19 Jul 2019
    , (  7, 1, 0) -- 10 May 2019
    , (  8, 0, 0) -- 20 Mar 2019
    , (  7, 0, 1) -- 21 Dec 2018
    , (  7, 0, 0) -- 19 Sep 2018
    , (  6, 0, 1) -- 05 Jul 2018
    , (  5, 0, 2) -- 16 May 2018
    , (  6, 0, 0) -- 08 Mar 2018
    , (  5, 0, 1) -- 21 Dec 2017
    , (  5, 0, 0) -- 07 Sep 2017
    , (  4, 0, 1) -- 04 Jul 2017
    , (  4, 0, 0) -- 13 Mar 2017
    , (  3, 9, 1) -- 23 Dec 2016
    , (  3, 9, 0) -- 02 Sep 2016
    , (  3, 8, 1) -- 11 Jul 2016
    , (  3, 8, 0) -- 08 Mar 2016
    , (  3, 7, 1) -- 05 Jan 2016
    , (  3, 7, 0) -- 01 Sep 2015
    , (  3, 6, 2) -- 16 Jul 2015
    , (  3, 6, 1) -- 26 May 2015
    , (  3, 6, 0) -- 27 Feb 2015
    , (  3, 5, 2) -- 02 Apr 2015
    , (  3, 5, 1) -- 20 Jan 2015
    , (  3, 5, 0) -- 03 Sep 2014
    , (  3, 4, 2) -- 19 Jun 2014
    , (  3, 4, 1) -- 07 May 2014
    , (  3, 4, 0) -- 02 Jan 2014
    , (  3, 3, 0) -- 17 Jun 2013
    , (  3, 2, 0) -- 20 Dec 2012
    , (  3, 1, 0) -- 22 May 2012
    , (  3, 0, 0) -- 01 Dec 2011
    , (  2, 9, 0) -- 06 Apr 2011
    , (  2, 8, 0) -- 05 Oct 2010
    , (  2, 7, 0) -- 27 Apr 2010
    , (  2, 6, 0) -- 23 Oct 2009
    , (  2, 5, 0) -- 02 Mar 2009
    , (  2, 4, 0) -- 09 Nov 2008
    , (  2, 3, 0) -- 09 Jun 2008
    , (  2, 2, 0) -- 11 Feb 2008
    , (  2, 1, 0) -- 26 Sep 2007
    , (  2, 0, 0) -- 23 May 2007
    , (  1, 9, 0) -- 19 Nov 2006
    , (  1, 8, 0) -- 09 Aug 2006
    , (  1, 7, 0) -- 20 Apr 2006
    , (  1, 6, 0) -- 08 Nov 2005
    , (  1, 5, 0) -- 18 May 2005
    , (  1, 4, 0) -- 09 Dec 2004
    , (  1, 3, 0) -- 13 Aug 2004
    , (  1, 2, 0) -- 19 Mar 2004
    , (  1, 1, 0) -- 17 Dec 2003
    , (  1, 0, 0) -- 24 Oct 2003
    ]

