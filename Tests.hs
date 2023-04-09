{-# LANGUAGE LambdaCase #-}

import Data.RLE as RLE (tests)
import Data.BWT as BWT (tests)
import Data.MTF as MTF (tests)
import Data.FMIndex as FMIndex (tests)
import System.Exit
import Test.HUnit hiding (Path)

main :: IO ()
main =
  runTestTT (TestList [RLE.tests, BWT.tests, MTF.tests, FMIndex.tests]) >>= \case
    Counts {errors = 0, failures = 0} -> return ()
    _ -> exitWith $ ExitFailure 1

tests =
  TestList [RLE.tests, BWT.tests, MTF.tests, FMIndex.tests]
