import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TRenaming
import TPaths
import TWlp
import TNorm
import TSat
import TFull

main = defaultMain
  [ constructTestSuite testName testSuite
  | (testName, testSuite) <- [ ("RENAMING", renamingTests)
                             , ("PATHS", pathTests)
                             , ("WLP", wlpTests)
                             , ("NORM", normTests)
                             , ("SAT", satTests)
                             , ("FULL", fullTests)
                             ]
  ]

constructTestSuite s suite =
  testGroup s [testCase (s ++ "_" ++ show i) t | (i, t) <- zip [1..] suite]
