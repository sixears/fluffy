{-# LANGUAGE ScopedTypeVariables #-}

import Fluffy.Data.Time  ( FDurOpts(..)
                         , formatDuration, formatDuration', scanDuration )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@?=), testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck as QC

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ formatDurationProps, formatDurationChecks ]

fdur :: Int -> String
fdur = formatDuration

fdurs :: Int -> String
fdurs = formatDuration' FDurOpts { noRHSTrim = True }

formatDurationProps :: TestTree
formatDurationProps =
  testGroup "formatDurationProps"
    [
      QC.testProperty                                            "scan-format" $
        \n -> scanDuration (formatDuration (n :: Int)) == Just n
    ]

formatDurationChecks :: TestTree
formatDurationChecks =
  testGroup "formatDurationChecks"
    [ testCase         "35s" $ fdur     35 @?=         "35s"
    , testCase       "5m05s" $ fdur    305 @?=       "5m05s"
    , testCase    "5h05m50s" $ fdur  18350 @?=    "5h05m50s"
    , testCase "5d07h52m30s" $ fdur 460350 @?= "5d07h52m30s"
    , testCase    "5d07h52m" $ fdur 460320 @?=    "5d07h52m"
    , testCase       "7h52m" $ fdur  28320 @?=       "7h52m"
    , testCase       "5d07h" $ fdur 457200 @?=       "5d07h"
    , testCase    "5d00h52m" $ fdur 435120 @?=    "5d00h52m"
    , testCase          "5d" $ fdur 432000 @?=          "5d"

-- add test for 0s with & without options
      
    , testCase "5d07h52m00s" $ fdurs 460320 @?=    "5d07h52m00s"
    , testCase    "7h52m00s" $ fdurs  28320 @?=       "7h52m00s"
    , testCase    "5d07h00s" $ fdurs 457200 @?=    "5d07h00m00s"
    , testCase "5d00h52m00s" $ fdurs 435120 @?=    "5d00h52m00s"
    , testCase       "5d00s" $ fdurs 432000 @?=    "5d00h00m00s"
    ]
